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
  def apply[N <: Nat](lessThan: LessThan[N])(implicit sd: StructDef[S], eq: N LE Length): S[A] = macro support.ArrayMacros.getStructFromLessThan[N]
  def apply(n: Nat)(implicit sd: StructDef[S], toInt: ToInt[n.N], eq: n.N LE Length): S[A] = macro support.ArrayMacros.getStructFromNat
  def apply(index: Int)(implicit sd: StructDef[S]): S[A] = macro support.ArrayMacros.getStructFromIndex
  def length(implicit nDim: ToInt[Length]) = nDim()

  def fold[T](initValue: T)(f: (T, S[A], Int) => T): T = macro support.ArrayMacros.fold[T]
  def foreach(f: (S[A], Int) => Unit): Unit = macro support.ArrayMacros.foreach
}


object Array {
  def apply[A <: Allocator[A], S[X <: Allocator[X]] <: Struct[X]](n: Nat, sd: StructDef[S])(implicit alloc: A, nDim: ToInt[n.N]): Array[A, S, n.N] = {
    val addr = alloc allocate sd.size * nDim()
    new Array(addr)
  }
}
