package ohc

import language.higherKinds
import shapeless.tag._

/**
 * A simple stack over Memory.
 */
final class Stack(val memory: Memory) extends AllocatorFactory {

  private[this] var memoryOffset: Long = 0

  type Allocator = StackAllocator
  sealed trait StackAllocator extends AllocatorDefintion {
    type Self = Allocator
    def allocate[S[X <: AllocatorDefintion] <: Struct[X]](implicit structDef: StructDef[S]): S[Allocator] = {
      if (memory.size - memoryOffset < structDef.size) throw new IllegalStateException(s"Not enough memory. Used ${memoryOffset}/${memory.size}, required ${structDef.size}")
      val res = structDef[Allocator](memoryOffset)
      memoryOffset += structDef.size
      res
    }
    val memory = Stack.this.memory
  }

  private object AllocatorImpl extends Allocator

  def contextualized[R](f: Allocator => R): R = {
    val origOffset = memoryOffset
    val res = f(AllocatorImpl)
    val finalOffset = memoryOffset
    memory.clear(origOffset, finalOffset - origOffset)
    memoryOffset = origOffset
    res
  }
}
