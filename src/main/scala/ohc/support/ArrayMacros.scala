package ohc.support

import language.higherKinds
import Nat.Nat

class ArrayMacros(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  val arr = c.prefix.tree

  def getStructFromLessThan(lessThan: Tree)(sd: Tree): Tree = q"$sd($arr.offset($lessThan))"
  def getStructFromIndex(index: Tree)(sd: Tree): Tree = q"$sd($arr.offset(_root_.ohc.support.Nat.nat2LessThan[$arr.Length](_root_.ohc.support.Nat.int2Nat($index))))"
}
