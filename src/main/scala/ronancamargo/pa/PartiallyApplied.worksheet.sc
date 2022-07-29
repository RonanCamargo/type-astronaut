import cats.data._
import cats._

//Cats example
class PurePartiallyApplied[F[_]](val dummy: Boolean = true) {
  def apply[A](value: A)(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT(F.pure(Some(value)))
}

def pure[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]
pure[List](1)

//Shapeless example
import shapeless._
import shapeless.ops.hlist

case class AA(num: Int, id: String, i: Int)
case class BB(id: String, num: Int)

class PAShapeless[B](dummy: Boolean = true) {
  def apply[A, HA <: HList, HB <: HList](a: A)(implicit
      genA: LabelledGeneric.Aux[A, HA],
      genB: LabelledGeneric.Aux[B, HB],
      // intersection: hlist.Intersection.Aux[HA, HB, HB],
      align: hlist.Align[HA, HB]
  ): B = {
    genB.from(align(genA.to(a)))
  }
}
def b[B] = new PAShapeless[B]

// b[BB](AA(1, "a"))

//AA(1,"a").b[BB]
cats.syntax.applicative

//a.to[B](implicit pa: PAShapeless[B]) = pa.apply(a)

implicit def bops[A, HA <: HList, HB <: HList](
    a: A
)(implicit genA: LabelledGeneric.Aux[A, HA]) = new BOps[A, HA, HB](a, genA)

class BOps[A, HA <: HList, HB <: HList](a: A, genA: LabelledGeneric.Aux[A, HA]) {
  def to[B](implicit genB: LabelledGeneric.Aux[B, HB], align: hlist.SelectAll[HA, HB]): B = genB.from(align(genA.to(a)))

//  def with[C](implicit genB: LabelledGeneric.Aux[C, HC], interleave: hlist.Interleave[])
}

AA(1, "a", 2).to[BB]

case class A1(n: Int, s: String)
case class B1(n: Int)
case class B2(s: String)
case class B3(b: Boolean)

val gen: LabelledGeneric[AA] {
  type Repr =
    Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged["num"], Int] ::
      String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged["id"], String] ::
      Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged["i"], Int] ::
      shapeless.HNil
} = LabelledGeneric[AA]
