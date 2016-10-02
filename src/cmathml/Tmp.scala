package cmathml

case class C[T](x:T) {
  def f(y:T) = ???
}

object Tmp {
  val x : T forSome {type T <: AnyRef} = ???
  val y : C[x.type] = ???

  def main(args: Array[String]): Unit = {
    val dep : (T,C[T]) forSome {type T} = ???

    dep match {
      case (a, b) =>
        val a2 = a
        val b2 = b
        b2.f(a2)
    }
  }
}
