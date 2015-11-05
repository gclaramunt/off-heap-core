package ohc
package support

import Nat._
import scala.reflect.runtime.universe._
import shapeless.test.illTyped

object NatTests extends App {

  val n1 = int2Nat(34)
  val n2 = int2Nat(41)


  def test(n: Nat, n2: Nat)(implicit ev: n.N Eq n2.N): Unit = ()
  test(n1, n1)
  test(n1, 34)
  illTyped("test(n1, n2)")

  implicitly[ToInt[n1.N]]
  implicitly[n1.N LT n2.N]
  illTyped("implicitly[n1.N LT n1.N]")
  illTyped("implicitly[n2.N LT n1.N]")
  implicitly[n1.N LE n2.N]
  implicitly[n1.N LE n1.N]
  illTyped("implicitly[n2.N LE n1.N]")
  
  implicitly[n2.N GT n1.N]
  illTyped("implicitly[n1.N GT n1.N]")
  illTyped("implicitly[n1.N GT n2.N]")
  implicitly[n2.N GE n1.N]
  implicitly[n1.N GE n1.N]
  illTyped("implicitly[n1.N GE n2.N]")
}
