package ohc

import language.higherKinds
import shapeless.tag._

trait StructDef[T[X <: Allocator[X]] <: Struct[X]] extends Any {
  def apply[A <: Allocator[A]]()(implicit allocator: A): T[A]

  /**
   * Low level initializer given a memory address.
   */
  def apply[A <: Allocator[A]](ptr: Long @@ A): T[A]
  def size: Long
}

trait Struct[A <: Allocator[A]] extends Any {
  def _ptr: Long
}

object Struct {

  implicit class StructOps[A <: Allocator[A], S[X <: Allocator[X]] <: Struct[X]](val s: S[A]) extends AnyVal {
    @inline def cloneIn[B <: Allocator[B]](b: B)(implicit a: A, sd: StructDef[S]): S[B] = {
      val res = sd.apply()(b)
      a.memory.copyTo(b.memory, s._ptr, sd.size, res._ptr)
      res
    }
  }
}