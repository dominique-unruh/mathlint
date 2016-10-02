package latex

import java.io.File

import cmathml.CMathML

object LatexTest {
  def main(args: Array[String]): Unit = {
//    val p = "1*(2*3)"
//    val m = CMathML.fromPopcorn(p)
//    val r = new LatexRender(m)
//    r.dumpTree()
//    val l = r.toLatex().toString
//    r.makePng(l)
    LatexRender.formulaListToHtml(new File("formulas.txt"), new File("formulas.html"))
  }

}