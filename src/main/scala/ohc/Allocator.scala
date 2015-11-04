package ohc

import language.{ higherKinds, implicitConversions }
import scala.annotation.implicitNotFound
import shapeless.tag._

@implicitNotFound("Can't allocate structure, no memory context available.")
trait Allocator[Self <: Allocator[Self]] {
  /**
   * Requests this allocator for an amount of memory. It returns the memory address point to the beggining of such block.
   */
  def allocate(size: Long): Long
  def memory: Memory
}
