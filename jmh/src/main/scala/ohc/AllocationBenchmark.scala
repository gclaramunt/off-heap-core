package ohc

import org.openjdk.jmh.annotations._
import ohc.annotation.struct
import shapeless.syntax.singleton._

@State(Scope.Thread)
class AllocationBenchmark {
  import AllocationBenchmark._

  implicit val sd = Point.structDef //why do I have to do this?
  val amount = 10.narrow
  val stack = new Stack(new DirectMemory(Point.size * amount))

  @Benchmark
  @BenchmarkMode(scala.Array(Mode.SampleTime))
  def measure0OffHeap: (Double, Double) = {
    stack.contextualized { implicit a =>
      val arr = Array(amount, Point)
      var i = -1
      while({i += 1; i < amount}) {
        val idx = new support.Nat.LessThan[arr.Length](i)
        val p = arr(idx)
        p.x = i / (i + 1)
        p.y = i / (i + 2)
      }
      val p = arr(2)
      p.x -> p.y
    }
  }

  class PointInHeap(var x: Double, var y: Double)
  val inHeapArr = new scala.Array[PointInHeap](amount)
  @Benchmark
  @BenchmarkMode(scala.Array(Mode.SampleTime))
  def measure1InHeap: (Double, Double) = {
    var i = -1
    while({i += 1; i < amount}) {
      inHeapArr(i) = new PointInHeap(0, 0)
      val p = inHeapArr(i)
      p.x = i / (i + 1)
      p.y = i / (i + 2)
    }
    val p = inHeapArr(2)
    p.x -> p.y
  }
}
object AllocationBenchmark {
  @struct class Point[T <: Allocator[T]](var x: Double, var y: Double)
}