package ohc

import language.higherKinds

trait StructDef[T[X <: Allocator[X]] <: Struct[X]] extends Any {
  def apply[A <: Allocator[A]](pointer: Long): T[A]
  def size: Long
}

trait Struct[A <: Allocator[A]] extends Any {
  def _ptr: Long
}

object Struct {

  implicit class StructOps[A <: Allocator[A], S[X <: Allocator[X]] <: Struct[X]](val s: S[A]) extends AnyVal {
    def cloneIn[B <: Allocator[B]](implicit a: A, b: B, sd: StructDef[S]): S[B] = {
      val res = b.allocate[S]
      a.memory.copyTo(b.memory, s._ptr, sd.size, res._ptr)
      res
    }
  }
}