package ohc

import shapeless.test.illTyped
import shapeless.tag._
/**
 * Quick example using the stack allocator
 */
object QuickTest extends App {

  class Point[A <: Allocator[A]](val _ptr: Long @@ A) extends AnyVal with Struct[A] {
    def x(implicit allocator: A) = allocator.memory.getInt(_ptr)
    def x_=(v: Int)(implicit allocator: A) = allocator.memory.setInt(_ptr, v)
    def y(implicit allocator: A) = allocator.memory.getInt(_ptr + 4)
    def y_=(v: Int)(implicit allocator: A) = allocator.memory.setInt(_ptr + 4, v)
  }
  object Point extends StructDef[Point] {
    def apply[A <: Allocator[A]]()(implicit allocator) = new Point[A](allocator allocate size)
    def apply[A <: Allocator[A]](ptr) = new Point[A](ptr)
    def size: Long = 8

    implicit val structDef = this

    def apply[A <: Allocator[A]](x: Int, y: Int)(implicit alloc: A): Point[A] = {
      val res = Point()
      res.x_=(x)
      res.y = y
      res
    }

    def copier[A <: Allocator[A]](implicit a: A) = new Copier(a)
    class Copier[A <: Allocator[A]](val a: A) extends AnyVal {
      def copy(p: Point[A])(x: Int = p.x(a), y: Int = p.y(a)) = Point(x, y)(a)
    }
  }

  val stack = new Stack(new DirectMemory(2000))
  val stack2 = new Stack(new DirectMemory(8))

  stack.contextualized { implicit a =>
    val p = Point(1,2)

    Point.copier.copy(p)(y = 3)
    //proof that you can't manipulate the vector from a differnt context
    illTyped("""
    stack2.contextualized { implicit a =>
      p.x = 34
    }
    """)

    stack2.contextualized { implicit b =>
      val anotherPoint = p.cloneIn(b)
    }

    println((p.x, p.y))
    p.y = 32
    p.x = 23
    println((p.x, p.y))

  }
  
  println(stack.memory.getLong(0))

  def randomInt() = (math.random * 100).toInt
  var res = 0
  while(math.random < 0.9999999999999) {
    stack.contextualized { implicit a =>
      val p = Point(randomInt(), randomInt())
      val y = p.y
      res += (if (math.random > 0.5) p.x else p.y)
      val arr = Array(100, Point)

      val p2 = arr(13)
      illTyped("Point(arr(1000002))")
      p2.x = randomInt()
      p2.y = randomInt()
      res += (if (math.random > 0.5) p2.x else p2.y)
    }
  }
  println(res)
}
