package mathcontent

import java.io.{File, FileReader}

import cmathml.{Apply, CMathML, CN, CSymbol}
import latex.LatexRender
import misc.{OptionProperty, Property, PropertyMap, Utils}
import org.yaml.snakeyaml.Yaml

import scala.language.postfixOps
import scalaz.Cord

object Properties {
  val knownProperties = List(
    Priority, ToLatex, ArgumentPriority
  )
  val properties = (for (pri <- knownProperties) yield pri.name -> pri).toMap[String,Property[_]]

  object Priority extends OptionProperty[Int] {
    override val name: String = "priority"
    override def parse(string: String): Option[Int] = string match {
      case "atom" => Some(LatexRender.priorityAtom)
      case "lowest" => Some(LatexRender.priorityLowest)
      case _ => Some(string.toInt)
    }
  }

  type ToLatexT = (CMathML, CMathML => Cord) => Cord
  object ToLatex extends OptionProperty[ToLatexT] {
    override val name = "latex"
    val templatePattern = "(#[0-9]+|[^#]+)".r
    override def parse(string: String): Option[ToLatexT] = {
      val (kind,code) = Utils.splitString2(string, ' ')
      kind match {
        case "infix" =>
          val code2 = code + " " : Cord;
          def render(math:CMathML, toLatex:CMathML=>Cord) = math match {
            case Apply(_, _, args @_*) => Cord.mkCord(code2, args.map(toLatex): _*) }
          Some(render)
        case "template" =>
          val frags : List[Either[Int,Cord]] = for (m <- templatePattern.findAllIn(code).toList)
            yield if (m.charAt(0)=='#') Left(m.substring(1).toInt-1)
                  else Right(m : Cord)
          val numargs = frags collect {case Left(i) => i+1} max
          def render(math:CMathML, toLatex:CMathML=>Cord) = math match {
            case Apply(_, _, args @_*) =>
              assert(args.length==numargs)
              val frags2 = frags map { case Left(i) => toLatex(args(i)); case Right(c) => c }
              Cord.mkCord(Cord.empty,frags2 :_*)
          }
          Some(render)
        case _ => throw new RuntimeException(s"When parsing $name property: Unknown renderer type $kind")
      }
    }
  }

  object ArgumentPriority extends OptionProperty[Int] {
    override val name: String = "argPriority"

    override def parse(string: String): Option[Int] = string match {
      case "atom" => Some(LatexRender.priorityAtom)
      case "lowest" => Some(LatexRender.priorityLowest)
      case _ => Some(string.toInt)
    }
  }

  private def splitCdName(cdname:String) : CSymbol.Id = {
    val Seq(cd,name) = cdname.split('.').toSeq
    (cd,name)
  }

  def readProperties(file:File): Map[CSymbol.Id, PropertyMap] = {
    import scala.collection.JavaConversions._

    val yaml = new Yaml()
    val config = yaml.load(new FileReader(file)).asInstanceOf[java.util.HashMap[String,java.util.HashMap[String,Any]]]

    val propMaps = for {
      (cdname, props) <- config
      (cd, name) = splitCdName(cdname)
      propMap0 = for ((propName, valStr) <- props) yield propName -> valStr.toString
      propMap = PropertyMap.parsePropertyMap(propMap0, properties)
    } yield (cd,name) -> propMap

    Map(propMaps.toSeq : _*)
  }
}
