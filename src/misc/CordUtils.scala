package misc

import scalaz.Cord

/**
  * Created by unruh on 9/17/16.
  */
object CordUtils {
  implicit class CordInterpolate(val sc: StringContext) extends AnyVal {
    import sc._

    def standardInterpolator(process: String => String, args: Seq[Any]): Cord = {
      checkLengths(args)
      val pi = parts.iterator
      val ai = args.iterator
      var cord : Cord = process(pi.next())

      while (ai.hasNext) {
        val acord: Cord = ai.next() match {
          case c: Cord => c
          case s => s.toString: Cord
        }
        cord ++= acord
        cord += process(pi.next())
      }
      cord
    }

    def cord(args: Any*) = standardInterpolator(StringContext.treatEscapes, args)
    def rcord(args: Any*) = standardInterpolator(identity, args)
  }
}
