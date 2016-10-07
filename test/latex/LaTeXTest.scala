package latex

import java.io.File

import cmathml.CMathML
import mathcontent.Properties

object LatexTest {
  def main(args: Array[String]): Unit = {
    val props = Properties.readProperties(new File("config.yaml"))
//    println(props)
    LatexRender.formulaListToHtml(props, new File("formulas.txt"), new File("formulas.html"))
  }
}