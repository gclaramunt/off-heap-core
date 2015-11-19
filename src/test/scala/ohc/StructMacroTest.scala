package ohc

import ohc.annotation.struct
import shapeless.syntax.singleton._
import shapeless.tag._

object StructMacroTest {
  @struct class Point[A <: Allocator[A]](var x: Int, var y: Int)
  object Point {}

  @struct class Pixel[A <: Allocator[A]](r: Byte, g: Byte, b: Byte)

  val stack = new Stack(new DirectMemory(10))
  stack.contextualized { implicit c =>
    val p = Point(4, 65)
//    val Point(a, b) = p
    p.x = 23

    val pixel = Pixel(0, 0, 0)

    val p2 = new Point(2323233l.asInstanceOf[Long @@ stack.StackAllocator])


    val arr = Array(100, Pixel)
    arr(Point.size)
    arr.fold(34)((state, pixel, i) => state + pixel.b)

    arr foreach ((e, i) => println(e))
  }
}