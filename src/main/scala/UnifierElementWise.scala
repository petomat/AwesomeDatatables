import shapeless._
import shapeless.ops.hlist.{Transposer, Unifier}

/**
 * Type class unifying types element-wise
 *
 * @example
 * {{{
 *   import shapeless._
 *   import shapeless.ops.hlist.Prepend
 *
 *   sealed trait Fruit
 *   case object Apple  extends Fruit
 *   case object Banana extends Fruit
 *
 *   case class Holder[C <: HList](content: C) {
 *     def append[R <: HList, CR <: HList, C0 <: HList](other: R)(implicit
 *       prepend: Prepend.Aux[C, R :: HNil, CR],
 *       unifier: UnifierElementWise.Aux[CR, C0]
 *     ): Holder[C0] = {
 *       Holder(unifier(prepend(content, other :: HNil)))
 *     }
 *   }
 *
 *   val hl1:  Apple.type :: HNil =  Apple :: HNil
 *   val hl2: Banana.type :: HNil = Banana :: HNil
 *   val holder1: Holder[(Apple.type :: HNil) :: HNil] = Holder(hl1 :: HNil)
 *   val holder2 = holder1 append hl2
 *   type F = Fruit with Product with Serializable
 *   shapeless.test.typed[Holder[(F :: HNil) :: (F :: HNil) :: HNil]](holder2)
 * }}}
 *
 * @see https://gitter.im/milessabin/shapeless?at=56fd08d98f5147e119f21897
 *
 * @author Peter Schmitz
 */
trait UnifierElementWise[HL <: HList] extends DepFn1[HL] with Serializable {
  type Out <: HList
}

object UnifierElementWise {
  trait Helper[HL <: HList] extends DepFn1[HL] with Serializable {
    type Out <: HList
  }
  object Helper {
    type Aux[HL <: HList, Out0 <: HList] = Helper[HL] { type Out = Out0 }
    implicit val hnilHelper: Aux[HNil, HNil] = new Helper[HNil] {
      type Out = HNil
      def apply(l: HNil): Out = HNil
    }
    //  implicit def hsingleHelper[T <: HList, U <: HList](implicit
    //    unifier: Unifier.Aux[T, U]
    //  ): Aux[T :: HNil, U :: HNil] = new Helper[T :: HNil] {
    //    type Out = U :: HNil
    //    def apply(l: T :: HNil): Out = unifier(l.head) :: HNil
    //  }
    implicit def hlistHelper[H <: HList, T <: HList, HU <: HList](implicit
      unifier: Unifier.Aux[H, HU],
      helper: Helper[T]
    ): Aux[H :: T, HU :: helper.Out] = new Helper[H :: T] {
      type Out = HU :: helper.Out
      def apply(l: H :: T): Out = unifier(l.head) :: helper(l.tail)
    }
  }
  def apply[HL <: HList](implicit unifier: UnifierElementWise[HL]): Aux[HL, unifier.Out] = unifier
  type Aux[HL <: HList, Out0 <: HList] = UnifierElementWise[HL] { type Out = Out0 }
  implicit val hnilUnifier: Aux[HNil, HNil] = new UnifierElementWise[HNil] {
    type Out = HNil
    def apply(hl: HNil): Out = HNil
  }
  implicit def hlistUnifier[HL <: HList, HLT <: HList, HLTU <: HList, HLTUT <: HList](implicit
    transposer: Transposer.Aux[HL, HLT],
    helper: Helper.Aux[HLT, HLTU],
    backTransposer: Transposer.Aux[HLTU, HLTUT]
  ): Aux[HL, HLTUT] = {
    new UnifierElementWise[HL] {
      type Out = HLTUT
      def apply(hl: HL): Out = backTransposer(helper(transposer(hl)))
    }
  }
}
