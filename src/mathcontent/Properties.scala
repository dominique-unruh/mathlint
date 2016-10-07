package mathcontent

import java.io.{File, FileReader}

import cmathml.{CMathML, CN, CSymbol}
import latex.LatexRender
import misc.{Property, PropertyMap}
import org.yaml.snakeyaml.Yaml

object Properties {
  val knownProperties = List(
    Priority
  )
  val properties = (for (pri <- knownProperties) yield pri.name -> pri).toMap[String,Property[_]]

  object Priority extends Property[Option[Int]] {
    override val name: String = "priority"
    override val default: Option[Int] = None
    override def parse(string: String): Option[Int] = string match {
      case "atom" => Some(LatexRender.priorityAtom)
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
