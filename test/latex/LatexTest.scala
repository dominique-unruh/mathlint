package latex

import java.io.File

import mathcontent.Properties

object LatexTest {
  def main(args: Array[String]): Unit = {
    val props = Properties.readProperties(new File("config.yaml"))
    new File("images").mkdir()
    LatexRender.formulaListToHtml(props, new File("formulas.txt"), new File("formulas.html"))
  }
}
