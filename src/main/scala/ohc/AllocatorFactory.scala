package ohc

import language.higherKinds
import shapeless.tag._

/**
 * An allocator factory provides allocators, which un turned are used to manage structs.
 */
trait AllocatorFactory {
  type Allocator <: AllocatorDefintion[Allocator]

  def contextualized[R](f: Allocator => R): R
}

trait AllocatorDefintion[Self <: AllocatorDefintion[Self]] extends Any {
  def allocate[S[X <: AllocatorDefintion[X]] <: Struct[X]](implicit structDef: StructDef[S]): S[Self]
  def memory: Memory
}