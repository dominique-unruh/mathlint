package latex

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.security.MessageDigest

import cmathml.CMathML.arith1
import cmathml._
import latex.LatexRender.SymbolProperties
import mathcontent.Properties.{ArgumentPriority, Priority, ToLatex}
import org.bitbucket.inkytonik.kiama.attribution.{Attribute, Attribution}
import org.bitbucket.inkytonik.kiama.relation.Tree

import scalaz.Cord
import misc.CordUtils._
import misc.PropertyMap

import scala.sys.process.{Process, ProcessLogger}


class LatexRender(props: SymbolProperties, val math : CMathML) {
  import LatexRender._

  val tree = new Tree[CMathML,CMathML](math)
  object Attr extends Attribution
  import Attr._

  val priorities : Map[CSymbol.Id,Int] = for {
    (sym,symProps) <- props
    Some(pri) = symProps(Priority)
  } yield sym -> pri

  private def getPriority(math:CMathML) = math match {
    case Apply(_, sym : CSymbol, _*) =>
      priorities.get(sym.id) match {
        case Some(pri) => pri
        case None => sys.error(s"Priority for $sym unknown")
      }
    case _:CSymbol => priorityAtom
    case _:CI => priorityAtom
    case _:CN => priorityAtom
    case _:CS => priorityAtom
    case _:CError => priorityAtom
    case _ => sys.error(s"nyi: $math")
  }


  val priority = attrWithName[CMathML,Int]("priority", {
    cmml => getPriority(cmml)
  })

  private def getDescendentKind(self: CMathML, parent: CMathML) : DescendantKind = {
    def totalEq(a:Any, b:Any) = (a,b) match {
      case (a2 : AnyRef, b2 : AnyRef) => a2 eq b2
      case (_: AnyRef, _) => false
      case (_, _: AnyRef) => false
      case _ => a==b
    }

    parent match {
      case _: cmathml.Leaf =>
      case Apply(_, hd, args@_*) =>
        if (hd eq self) return Head
        val idx = args.indexWhere(self eq _)
        if (idx >= 0) return Arg(idx)
      case Bind(_, hd, vars, body) =>
        if (hd eq self) return Head
        if (hd eq body) return Body
        val idx = vars.indexWhere(self eq _)
        if (idx >= 0) return Var(idx)
    }

    val ((cd,name),_) = (parent : CMathML).attributes.find({ case (_,value) => totalEq(self,value) }).get
    Attrib(cd,name)
  }

  val descendantKind = attrWithName[CMathML,DescendantKind]("descendantKind", {
    case self @ tree.parent(parent) => getDescendentKind(self,parent)
    case _ => Root
  })

  def argumentPriority(parent: CMathML, descendantKind: DescendantKind) : Int = (parent, descendantKind) match {
//    case (arith1.divideE(_,_), Arg(_)) => priorityLowest // TODO: Read from config.yaml
    case (Apply(_, sym : CSymbol, args@_*), Arg(_)) =>
      props(sym.id)(ArgumentPriority) match {
        case Some(pri) => pri
        case None => priorities(sym.id)
      }
    case (Apply(_, sym : CSymbol, args@_*), Head) => 0 // TODO (matters only for symbols that have no infix renderer)
    case _ => sys.error(s"nyi(argumentPriority): $parent $descendantKind")
  }

  val needsParenthesis = attrWithName[CMathML,Boolean]("needsParenthesis", {
    case self @ tree.parent(parent) =>
      if (priority(self) <= argumentPriority(parent, descendantKind(self))) true
      else false
    case _ => false // is root
  })

  def dumpTree() : Unit = dumpTree(tree.root, 0)

  def dumpTree(node: CMathML, indent: Int) : Unit = {
    val att = s"needsParenthesis=${needsParenthesis(node)}"

    print(s"${" "*indent}$node: $att\n")
    for (child <- tree.child(node))
      dumpTree(child, indent+1)
  }

  def toLatex(math:CMathML = tree.root) : Cord = {
    val ltx : Cord = math match {
      case Apply(_, hd, args @_*) =>
        val render = hd match { case sym:CSymbol => props(sym.id)(ToLatex); case _ => None }
        render match {
          case None =>
            val argList = Cord.mkCord(",", args.map(toLatex): _*)
            rcord"\mathbf{$hd}\left($argList\right)"
          case Some(render) =>
            render(math,toLatex)
        }

      case CI(_, name) =>
        if (name.length == 1) name
        else rcord"\mathit{$name}"

      case CN(_, num) => num.toString
    }

    if (needsParenthesis(math))
      rcord"\left( $ltx \right)"
    else
      ltx
  }
}

object LatexRender {
  sealed trait DescendantKind
  case object Head extends DescendantKind
  case object Root extends DescendantKind
  case object Body extends DescendantKind
  final case class Attrib(cd: String, name:String) extends DescendantKind
  final case class Arg(idx: Int) extends DescendantKind
  final case class Var(idx: Int) extends DescendantKind

  val priorityAtom = Int.MaxValue
  val priorityLowest = Int.MinValue

  def run(cmd: Seq[String], cwd: File) : Unit = {
    val output = new StringBuilder
    val logger = new ProcessLogger {
      override def buffer[T](f: => T): T = f
      override def out(s: => String): Unit = { output ++= s; output += '\n' }
      override def err(s: => String): Unit = { output ++= s; output += '\n' }
    }
    if (Process(cmd, cwd = cwd).run(logger).exitValue!=0) {
      throw new RuntimeException(
        s"Command failed: ${cmd.mkString(" ")}\nOutput: $output")
    }
  }

  def makePng(latex: String) : File = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(latex.getBytes("utf-8"))
    val hash = String.format("%064x", new java.math.BigInteger(1, md.digest))
    val f = new PrintWriter("images/"+hash+".tex")
    f.write(raw"""
            \documentclass{article}
            \${""}usepackage[active,tightpage,textmath]{preview}
            \begin{document}
              \( $latex \)
            \end{document}
      """)
    f.close()

    run(List("latex","-interaction","nonstopmode",hash), cwd=new File("images"))
    run(List("dvipng","--strict","--noghostscript","-o",hash+".png",hash), cwd=new File("images"))

    new File("images/"+hash+".png")
  }

  type SymbolProperties = Map[CSymbol.Id,PropertyMap]

  def formulaListToHtml(props: SymbolProperties, formulaList : File, htmlFile : File) = {
    val reader = new BufferedReader(new FileReader(formulaList))
    val formulas : Seq[String] = reader.lines.toArray.toList.asInstanceOf[List[String]]
    reader.close()
    val html = new PrintWriter(htmlFile)
    html.write("<html><head><meta http-equiv='refresh' content='5'></head><body><table border=1>")
    for (f <- formulas) {
      val render = new LatexRender(props, CMathML.fromPopcorn(f))
      val latex = render.toLatex().toString
      val png = makePng(latex)
      html.write(s"<tr><td>$f</td><td><img src='$png'/></td></tr>\n")
    }
    html.write("</table></body></html>")
    html.close()
  }
}
