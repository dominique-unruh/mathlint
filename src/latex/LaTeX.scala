package latex

import cmathml.CMathML._
import cmathml._
import misc.Log

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks
import scalaz.Cord



import misc.CordUtils._

object LaTeX {
  val renderParen : CSymbol.Id = ("render","paren")

  def toLatexRaw(math:CMathML) : Cord = {
    val ltx : Cord = math match {
      case arith1.plusE(args@_*) => Cord.mkCord("+", args.map(toLatexRaw): _*)
      case arith1.timesE(args@_*) => Cord.mkCord("*", args.map(toLatexRaw): _*)
      case arith1.divideE(a,b) => rcord"\frac{${toLatexRaw(a)}}{${toLatexRaw(b)}}"
      case Apply(_, hd, args@_*) =>
        val argList = Cord.mkCord("+", args.map(toLatexRaw): _*)
        rcord"\mathbf{$hd}\left($argList\right)"
      case CI(_, name) =>
        if (name.length == 1) name
        else rcord"\mathit{$name}"
      case CN(_, num) => num.toString
    }

    if (math.attributes.contains(renderParen)) rcord"\left( $ltx \right)" else ltx
  }

  def toLatex(math:CMathML) : String = {
    toLatexRaw(preprocess(math)).toString
  }

  val priorityAtom = Int.MaxValue
  val priorities : Map[CSymbol.Id,Int] = List(
    arith1.plus -> 100,
    arith1.times -> 200,
    arith1.divide -> priorityAtom
  ).map { case (s,p) => (s.id,p) }.toMap[CSymbol.Id,Int]

  private def getPriority(math:CMathML) = math match {
    case Apply(_, sym : CSymbol, _*) =>
      priorities.get(sym.id) match {
        case Some(pri) => pri
        case None => sys.error(s"Priority for $sym unknown")
      }
    case _:CI => priorityAtom
    case _:CN => priorityAtom
    case _:CS => priorityAtom
    case _:CError => priorityAtom
  }

  private def addParens(sym:CSymbol.Id, pri:Int) : Rule = {
    val (cd,name) = sym

    {
      case Apply(attrs, hd @ CSymbol(_,`cd`,`name`), args @ _*) =>
        var changed = false
        val newArgs = ListBuffer() : ListBuffer[CMathML]
        for (a <- args) {
          val argPri = getPriority(a)
          if (argPri <= pri && !a.attributes.contains(renderParen)) {
            changed = true
            newArgs += a.updateAttributes(renderParen -> logic1.trueSym)
          } else
            newArgs += a
        }
        if (changed)
          Some(Apply(attrs, hd, newArgs.toList : _*))
        else
          None
      case _ => None }
  }

  private def makePriorityRules : Seq[(CSymbol.Id,Rule)] = {
    val exclusions = HashSet(arith1.divide.id)

    priorities.toSeq filter {
      case (sym,pri) => !exclusions.contains(sym)
    } map {
      case (sym,pri) => sym -> addParens(sym,pri)
    }
  }

  private def makeRules = {
    val priorityRules = makePriorityRules
    priorityRules.foldRight(Map.empty : Map[CSymbol.Id,List[Rule]]) {
      case ((id,rule),map) => map.updated(id,rule :: map.getOrElse(id,Nil))
    }
  }
  val rules : Map[CSymbol.Id,Seq[Rule]] = makeRules

  type Rule = CMathML => Option[CMathML]

  def preprocess1level(math:CMathML) : Option[CMathML] = {

    def getRules(m:CMathML) = m match {
      case Apply(_, hd: CSymbol, _*) =>
        rules.getOrElse(hd.id, Nil)
      case _ => Nil
    }

    var changed = true
    var changedOverall = false

    var math2 = math

    while (changed) {
      changed = false
      for (rule <- getRules(math2)) {
        rule(math2) match {
          case None =>
          case Some(m) => math2 = m; changed = true
        }
      }
      if (changed) changedOverall = true
    }

    if (changedOverall) Some(math2)
    else None
  }

  def preprocess1(math:CMathML) : Option[CMathML] = math match {
    case Apply(attrs, hd, args @_*) =>
      val newArgs = ListBuffer() : ListBuffer[CMathML]
      var changed = false
      for (a <- args) {
        preprocess1(a) match {
          case None => newArgs.append(a)
          case Some(a2) =>
            newArgs.append(a2)
            changed = true
        }
      }

      var math2 = Apply(attrs, hd, newArgs.toList :_*)
      preprocess1level(math2) match {
        case None => if (changed) Some(math2) else None
        case m @ Some(_) => m
      }
    case m => preprocess1level(m)
  }

  @tailrec def preprocess(math:CMathML) : CMathML = preprocess1(math) match {
    case None => math
    case Some(m2) =>
      assert(math!=m2,"preprocess1 returns unchanged formula")
      preprocess(m2)
  }
}

