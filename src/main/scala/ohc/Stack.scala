package ohc

import language.higherKinds
import shapeless.tag._

/**
 * A simple stack over Memory.
 */
final class Stack(val memory: Memory) {

  private[this] var memoryOffset: Long = 0

  sealed trait StackAllocator extends Allocator[StackAllocator] {
    def allocate(size: Long): Long @@ StackAllocator = {
      if (memory.size - memoryOffset < size) throw new IllegalStateException(s"Not enough memory. Used ${memoryOffset}/${memory.size}, required $size")
      val res = memoryOffset
      memoryOffset += size
      res.asInstanceOf[Long @@ StackAllocator]
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
