package ohc

import ohc.annotation.struct
import shapeless.syntax.singleton._
import shapeless.tag._
import ohc.support.Nat.LessThan
import scala.reflect.runtime.universe._

object StructMacroTest extends App {
  @struct class Point[A <: Allocator[A]](var x: Int, var y: Int)
  object Point {}

  @struct class Pixel[A <: Allocator[A]](r: Byte, g: Byte, b: Byte)

  val stack = new Stack(new DirectMemory(12*100))
  stack.contextualized { implicit c =>
    val p = Point(4, 65)
//    val Point(a, b) = p
    p.x = 23

    val pixel = Pixel(0, 0, 0)

    val p2 = new Point(2323233l.asInstanceOf[Long @@ stack.StackAllocator])


    val arr = Array(100, Pixel)
    arr(Point.size)
    arr.fold(34)((state, pixel) => state + pixel.b)

    val pixel2points = Array(100, Point)
    arr.foreachWithIndex { (pixel, idx) =>
      val point = pixel2points(new LessThan[arr.Length](idx))
      point.x = pixel.r << 8 | pixel.g
      point.y = pixel.b
    }
  }
}