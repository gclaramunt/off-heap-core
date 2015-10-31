package ohc

import language.{ higherKinds, implicitConversions }
import scala.annotation.implicitNotFound
import shapeless.tag._

@implicitNotFound("Can't allocate structure, no memory context available.")
trait Allocator[Self <: Allocator[Self]] {
  def allocate[S[X <: Allocator[X]] <: Struct[X]](implicit structDef: StructDef[S]): S[Self] //TODO: review this. Scalac currently boxes for an instant during a constructor of a Struct
                                                                                             // due to escape analysis, it has 0 overhead, but we shouldn't trust escape analysis.
  def memory: Memory
}
