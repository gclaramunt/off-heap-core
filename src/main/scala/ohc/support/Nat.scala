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
  trait Nat { self =>
    /**
     * A Self type useful for path dependant natural's usage
     */
    type N <: Nat
    /**
     * The constant that this natural represents
     */
    type Num <: Int
  }
  /**
   * Type representing instances of Nat. Normally you shouldn't be concerned by this type
   */
  trait NatInstance[T <: Int] extends Nat {
    type Num = T
    type N = this.type
  }
  implicit def int2Nat(num: Int): Nat = macro NatWhiteboxMacros.implicitNat

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

  /**
   * Instances of this class are provided via the macro materializeToInt. You should not need instantiate it by hand.
   */
  class ToInt[N <: Nat](val num: Int) extends AnyVal { def apply() = num }
  implicit def materializeToInt[N <: Nat]: ToInt[N] = macro NatBlackboxMacros.materializeToInt[N]


  class NatWhiteboxMacros(override val c: scala.reflect.macros.whitebox.Context) extends NatBlackboxMacros(c) {
    import c.universe._

    def implicitNat(num: Tree): Tree = q"null.asInstanceOf[$NatInstanceSym[${num.tpe}]]"
  }
  class NatBlackboxMacros(val c: scala.reflect.macros.blackbox.Context) {
    import c.universe._
    val NatSym = symbolOf[Nat]
    val NatInstanceSym = symbolOf[NatInstance[_]]
    val ToIntSym = symbolOf[ToInt[_]]

    def getToInt(nat: Type): (Int, Tree) = {
      val numberTpe = nat.member(TypeName("Num")).infoIn(nat)
//      println(numberTpe)
      try {
        val number = numberTpe.toString.stripPrefix("Int(").stripSuffix(")").toInt
        number -> q"new $ToIntSym[$nat]($number)"
      } catch { case ne: NumberFormatException => c.abort(c.enclosingPosition, s"Nat[$nat] of unkown size")}
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

