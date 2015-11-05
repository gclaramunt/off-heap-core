package ohc.support

import language.{ implicitConversions, experimental }, experimental.macros
import scala.annotation.implicitNotFound

/**
 * Implementation of the Natural Numbers as types, entirely based on macros to avoid compiler problems such as stack overflow.
 */
object Nat {

  /**
   * General definition of a natural number. This is an universal trait, since implementations will be value classes erasing to just int.
   */
  trait Nat extends Any { self =>
    /**
     * A Self type useful for path dependant natural's usage
     */
    type N <: Nat
    /**
     * The constant that this natural represents
     */
    type Num <: Int
    /**
     * Obtain the constant.
     */
    def apply(): Int
  }
  object Nat {
    type Aux[N <: Int] = Nat { type Num = N }
  }
  implicit def int2Nat(num: Int): Nat = macro NatWhiteboxMacros.implicitNat

  /**
   * Value class implementation of Nat. Instances are created via the macro int2Nat
   */
  private[Nat] class NatImpl[T <: Int](val n: Int) extends Nat {
    type N = this.type
    type Num = T
    def apply() = n
  }

  trait Eq[N1 <: Nat, N2 <: Nat]
  
  trait LT[N1 <: Nat, N2 <: Nat]
  trait LE[N1 <: Nat, N2 <: Nat]
  trait GT[N1 <: Nat, N2 <: Nat]
  trait GE[N1 <: Nat, N2 <: Nat]
  
  implicit def n1EqN2[N1 <: Nat, N2 <: Nat]: Eq[N1, N2] = macro NatBlackboxMacros.n1EqN2[N1, N2]
  implicit def lessThan[N1 <: Nat, N2 <: Nat]: LT[N1, N2] = macro NatBlackboxMacros.lessThan[N1, N2]
  implicit def lessEq[N1 <: Nat, N2 <: Nat]: LE[N1, N2] = macro NatBlackboxMacros.lessEq[N1, N2]
  implicit def greaterThan[N1 <: Nat, N2 <: Nat]: GT[N1, N2] = macro NatBlackboxMacros.greaterThan[N1, N2]
  implicit def greaterEq[N1 <: Nat, N2 <: Nat]: GE[N1, N2] = macro NatBlackboxMacros.greaterEq[N1, N2]

  class ToInt[N <: Nat](val num: Int) extends AnyVal { def apply() = num }
  implicit def materializeToInt[N <: Nat]: ToInt[N] = macro NatBlackboxMacros.materializeToInt[N]


  class NatWhiteboxMacros(override val c: scala.reflect.macros.whitebox.Context) extends NatBlackboxMacros(c) {
    import c.universe._

    def implicitNat(num: Tree): Tree = q"new $NatImplSym[${num.tpe}]($num)"

  }
  class NatBlackboxMacros(val c: scala.reflect.macros.blackbox.Context) {
    import c.universe._
    val NatImplSym = symbolOf[NatImpl[_]]
    val ToIntSym = symbolOf[ToInt[_]]

    def getToInt(nat: Type): (Int, Tree) = {
      val numberTpe = NatImplSym.info.member(TypeName("Num")).infoIn(nat)
      val number = numberTpe.toString.stripPrefix("Int(").stripSuffix(")").toInt
      number -> q"new $ToIntSym[$nat]($number)"
    }

    def materializeToInt[N <: Nat](implicit nTypeTag: WeakTypeTag[N]): Tree = getToInt(nTypeTag.tpe)._2

    def compare(n1Tpe: Type, n2Tpe: Type, comparison: (Int, Int) => Boolean, descr: String): Tree = {
      val n1 = getToInt(n1Tpe)._1
      val n2 = getToInt(n2Tpe)._1
      if (comparison(n1, n2)) q"null"
      else c.abort(c.enclosingPosition, s"$n1Tpe($n1) $descr $n2Tpe($n2)")
    }
    def n1EqN2[N1 <: Nat, N2 <: Nat](implicit n1: WeakTypeTag[N1], n2: WeakTypeTag[N2]): Tree = compare(n1.tpe, n2.tpe, (_ == _), "!=")
    def lessThan[N1 <: Nat, N2 <: Nat](implicit n1: WeakTypeTag[N1], n2: WeakTypeTag[N2]): Tree = compare(n1.tpe, n2.tpe, (_ < _), ">=")
    def lessEq[N1 <: Nat, N2 <: Nat](implicit n1: WeakTypeTag[N1], n2: WeakTypeTag[N2]): Tree = compare(n1.tpe, n2.tpe, (_ <= _), ">")
    def greaterThan[N1 <: Nat, N2 <: Nat](implicit n1: WeakTypeTag[N1], n2: WeakTypeTag[N2]): Tree = compare(n1.tpe, n2.tpe, (_ > _), "<=")
    def greaterEq[N1 <: Nat, N2 <: Nat](implicit n1: WeakTypeTag[N1], n2: WeakTypeTag[N2]): Tree = compare(n1.tpe, n2.tpe, (_ >= _), "<")
  }
}

