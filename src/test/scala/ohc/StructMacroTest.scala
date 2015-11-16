package ohc

import ohc.annotation.struct
import shapeless.syntax.singleton._
import shapeless.tag._

object StructMacroTest {

  trait GenericVector[T, A <: Allocator[A]] extends Any {
    def _1(implicit a: A): T
    def _2(implicit a: A): T

    def asTuple(implicit a: A) = (_1, _2)
  }
  @struct class Point[A <: Allocator[A]](var x: Int, var y: Int) extends GenericVector[Int, A] {
    def _1(implicit a: A) = x
    def _2(implicit a: A) = y
  }
  object Point {}

  @struct class Pixel[A <: Allocator[A]](r: Byte, g: Byte, b: Byte)

  val stack = new Stack(new DirectMemory(10))
  stack.contextualized { implicit c =>
    val p = Point(4, 65)
    val Point(a, b) = p
    p.asTuple
    p.x = 23

    val pixel = Pixel(0, 0, 0)

    val p2 = new Point(2323233l.asInstanceOf[Long @@ stack.StackAllocator])


    Array(100, Pixel)
  }
}