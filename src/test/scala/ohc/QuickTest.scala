package ohc

import language.{higherKinds, existentials }

import shapeless.test.illTyped
/**
 * Quick example using the stack allocator
 */
object QuickTest extends App {

  class Point[Allocator <: AllocatorDefintion[Allocator]](val pointer: Long) extends AnyVal with Struct[Allocator] {
    def x(implicit allocator: Allocator) = allocator.memory.getInt(pointer)
    def x_=(v: Int)(implicit allocator: Allocator) = allocator.memory.setInt(pointer, v)
    def y(implicit allocator: Allocator) = allocator.memory.getInt(pointer + 4)
    def y_=(v: Int)(implicit allocator: Allocator) = allocator.memory.setInt(pointer + 4, v)
  }
  object Point {
    implicit val PointStruct = new StructDef[Point] {
      def apply[A <: AllocatorDefintion[A]](pointer) = new Point[A](pointer)
      def size: Long = 8
    }
    def apply[AD <: AllocatorDefintion[AD]](x: Int, y: Int)(implicit alloc: AD): Point[AD] = {
      val res = alloc.allocate[Point]
      res.x = x
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
