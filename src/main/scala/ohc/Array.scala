package ohc

import language.higherKinds
import shapeless.Nat
import shapeless.tag._
import shapeless.ops.nat._

class Array[A <: Allocator[A], S[X <: Allocator[X]] <: Struct[X], N <: Nat](val _ptr: Long) extends AnyVal with Struct[A] {
  def apply(m: Nat)(implicit ev: m.N LT N, mDim: ToInt[m.N], sd: StructDef[S]): Long @@ A = (_ptr + sd.size * mDim()).asInstanceOf[Long @@ A]
  def length(implicit nDim: ToInt[N]) = nDim()
}


object Array {
  def apply[A <: Allocator[A], S[X <: Allocator[X]] <: Struct[X]](n: Nat, sd: StructDef[S])(implicit alloc: A, nDim: ToInt[n.N]): Array[A, S, n.N] = {
    val addr = alloc allocate sd.size * nDim()
    new Array(addr)
  }
}
