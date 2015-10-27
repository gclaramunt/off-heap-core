package ohc

import language.higherKinds

trait Struct[Allocator <: AllocatorDefintion] extends Any
trait StructDef[T[X <: AllocatorDefintion] <: Struct[X]] extends Any {
  def apply[A <: AllocatorDefintion](pointer: Long): T[A]
  def size: Long
}
