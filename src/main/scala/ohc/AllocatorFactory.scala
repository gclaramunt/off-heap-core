package ohc

import language.higherKinds
import shapeless.tag._

/**
 * An allocator factory provides allocators, which un turned are used to manage structs.
 */
trait AllocatorFactory {
  type Allocator <: AllocatorDefintion

  def contextualized[R](f: Allocator => R): R
}

trait AllocatorDefintion {
  type Self <: AllocatorDefintion
  def allocate[S[X <: AllocatorDefintion] <: Struct[X]](implicit structDef: StructDef[S]): S[Self]
  def memory: Memory
}

object AllocatorDefintion {
  
  /**
   * An allocator is equal to its Self type by defintion. //TODO find a better way to enforce this typecheck
   */
  @inline implicit def allocatorIsItSelf(implicit a: AllocatorDefintion) = a.asInstanceOf[a.Self]
}
