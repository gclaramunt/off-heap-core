package ohc

import language.{ higherKinds, experimental }, experimental.macros
import support.Nat._
import support.DefaultsTo
import shapeless.tag._

class Array[A <: Allocator[A], S[X <: Allocator[X]] <: Struct[X], N <: Nat](val _ptr: Long) extends AnyVal with Struct[A] {
  type Length = N
  def offset[N <: Nat](lessThan: LessThan[N])(implicit sd: StructDef[S], eq: N LE Length): Long @@ A = (_ptr + sd.size * lessThan.num).asInstanceOf[Long @@ A]
  /**
   * Syntactic rewrite to StructDef(arr.offset(m)). See offset.
   */
  def apply(lessThan: LessThan[_])(implicit sd: StructDef[S]): S[A] = macro support.ArrayMacros.getStructFromLessThan
  def apply(index: Int)(implicit sd: StructDef[S]): S[A] = macro support.ArrayMacros.getStructFromIndex
  def length(implicit nDim: ToInt[Length]) = nDim()
}


object Array {
  def apply[A <: Allocator[A], S[X <: Allocator[X]] <: Struct[X]](n: Nat, sd: StructDef[S])(implicit alloc: A, nDim: ToInt[n.N]): Array[A, S, n.N] = {
    val addr = alloc allocate sd.size * nDim()
    new Array(addr)
  }
}
