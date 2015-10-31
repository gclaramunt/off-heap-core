package ohc

import shapeless.test.illTyped
/**
 * Quick example using the stack allocator
 */
object QuickTest extends App {

  class Point[A <: Allocator[A]](val pointer: Long) extends AnyVal with Struct[A] {
    def x(implicit allocator: A) = allocator.memory.getInt(pointer)
    def x_=(v: Int)(implicit allocator: A) = allocator.memory.setInt(pointer, v)
    def y(implicit allocator: A) = allocator.memory.getInt(pointer + 4)
    def y_=(v: Int)(implicit allocator: A) = allocator.memory.setInt(pointer + 4, v)
  }
  object Point {
    implicit val PointStruct = new StructDef[Point] {
      def apply[A <: Allocator[A]](pointer) = new Point[A](pointer)
      def size: Long = 8
    }
    def apply[A <: Allocator[A]](x: Int, y: Int)(implicit alloc: A): Point[A] = {
      val res = alloc.allocate[Point]
      res.x_=(x)
      res.y = y
      res
    }
  }

  val stack = new Stack(new DirectMemory(10))
  val stack2 = new Stack(new DirectMemory(1))

  stack.contextualized { implicit a =>
    val p = Point(1,2)

    //proof that you can't manipulate the vector from a differnt context
    illTyped("""
    stack2.contextualized { implicit a =>
      p.x = 34
    }
    """)

    println((p.x, p.y))
    p.y = 32
    p.x = 23
    println((p.x, p.y))
  }
  
  println(stack.memory.getLong(0))
}
