package ohc

import language.{ higherKinds, implicitConversions }
import scala.annotation.implicitNotFound
import shapeless.tag._

@implicitNotFound("Can't allocate structure, no memory context available.")
trait Allocator[Self <: Allocator[Self]] {
  def allocate[S[X <: Allocator[X]] <: Struct[X]](implicit structDef: StructDef[S]): S[Self]
  def memory: Memory
}
