package ohc

import language.higherKinds

trait Struct[Allocator <: AllocatorDefintion[Allocator]] extends Any
trait StructDef[T[X <: AllocatorDefintion[X]] <: Struct[X]] extends Any {
  def apply[A <: AllocatorDefintion[A]](pointer: Long): T[A]
  def size: Long
}
