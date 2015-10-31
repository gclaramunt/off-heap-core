package ohc

import ohc.annotation.struct

object StructMacroTest {

  trait GenericVector[T, A <: Allocator[A]] extends Any {
    def _1(implicit a: A): T
    def _2(implicit a: A): T

    def asTuple(implicit a: A) = (_1, _2)
  }
  @struct(debug = true) class Point[A <: Allocator[A]](var x: Int, var y: Int) extends GenericVector[Int, A] {
    def _1(implicit a: A) = x
    def _2(implicit a: A) = y
  }
  object Point {}


  val stack = new Stack(new DirectMemory(10))
  stack.contextualized { implicit c =>
    val p = Point(4, 65)
    val Point(a, b) = p
  }
}
