package ohc

import ohc.annotation.struct

trait GenericVector[T] extends Any {
  def _1: T
  def _2: T

  def asTuple = (_1, _2)
}
object StructMacroTest {


  @struct(debug = true) class Point[A <: Allocator[A]](var x: Int, var y: Int)
}
