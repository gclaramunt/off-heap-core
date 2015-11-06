package ohc.support

import language.higherKinds

class ArrayMacros(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._
  val arr = c.prefix.tree

  def getStruct(m: Tree)(sd: Tree): Tree = q"$sd($arr.offset($m))"
}
