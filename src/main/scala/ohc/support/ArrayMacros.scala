package ohc.support

import language.higherKinds
import Nat.Nat

class ArrayMacros(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  val arr = c.prefix.tree

  def getStructFromLessThan[N <: Nat](lessThan: Tree)(sd: Tree, eq: Tree): Tree = q"$sd($arr.offset($lessThan)($sd, $eq))"
  def getStructFromNat(n: Tree)(sd: Tree, toInt: Tree, eq: Tree): Tree = q"$sd($arr.offset(new _root_.ohc.support.Nat.LessThan[${n}.N]($toInt()))($sd, null))"
  def getStructFromIndex(index: Tree)(sd: Tree): Tree = q"$sd($arr.offset(_root_.ohc.support.Nat.nat2LessThan[$arr.Length](_root_.ohc.support.Nat.int2Nat($index))))"

  def fold[S](initValue: Tree)(f: Tree)(implicit stateTT: WeakTypeTag[S]): Tree = {
    val stateTerm = c.freshName(TermName("state"))
    val size = c.freshName(TermName("size"))
    val noState = stateTT.tpe =:= definitions.UnitTpe

    val (state, value, index, expr) = c.untypecheck(f).collect { case f@q"($state, $value, $index) => $expr" => (state, value, index, expr) }.head
    val it = index.name

    //optimize the case where state is Unit, so nothing is used, this is useful for other higher other methods such as foreach
    val declareState = if (noState) q"" else q"var $stateTerm = $initValue"
    val assignState = if(noState) q"" else q"val ${state.name} = $stateTerm"
    val updateState = if(noState) q"" else q"$stateTerm = $expr"
    val returnState = if(noState) q"()" else q"$stateTerm"
    val res = q"""
     $declareState
     var $it = -1
     val $size = $arr.length
     while({$it += 1; $it < $size}) {
       $assignState
       val ${value.name} = $arr(new _root_.ohc.support.Nat.LessThan[$arr.Length]($it))(implicitly, null)
       $updateState
     }
     $returnState
    """
    res
  }

  def foreach(f: Tree): Tree = {
    val (value, idx, expr) = c.untypecheck(f).collect { case f@q"($value, $idx) => $expr" => (value, idx, expr) }.head
    q"""$arr.fold(())((state, $value, $idx) => $expr)"""
  }
}
