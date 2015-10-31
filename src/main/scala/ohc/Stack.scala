package ohc

import language.higherKinds
import shapeless.tag._

/**
 * A simple stack over Memory.
 */
final class Stack(val memory: Memory) {

  private[this] var memoryOffset: Long = 0

  sealed trait StackAllocator extends Allocator[StackAllocator] {
    def allocate[S[X <: Allocator[X]] <: Struct[X]](implicit structDef: StructDef[S]): S[StackAllocator] = {
      if (memory.size - memoryOffset < structDef.size) throw new IllegalStateException(s"Not enough memory. Used ${memoryOffset}/${memory.size}, required ${structDef.size}")
      val res = structDef[StackAllocator](memoryOffset)
      memoryOffset += structDef.size
      res
    }
    val memory = Stack.this.memory
  }

  private object AllocatorImpl extends StackAllocator

  def contextualized[R](f: StackAllocator => R): R = {
    val origOffset = memoryOffset
    val res = f(AllocatorImpl)
    val finalOffset = memoryOffset
    memory.clear(origOffset, finalOffset - origOffset)
    memoryOffset = origOffset
    res
  }
}
