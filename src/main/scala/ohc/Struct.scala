package ohc

import language.higherKinds

trait Struct[A <: Allocator[A]] extends Any
trait StructDef[T[X <: Allocator[X]] <: Struct[X]] extends Any {
  def apply[A <: Allocator[A]](pointer: Long): T[A]
  def size: Long
}
