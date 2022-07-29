import org.w3c.dom.html.HTMLIsIndexElement
import shapeless._
import shapeless.ops.hlist._

val sops = shapeless.ops.hlist

val hlist1 = 1 :: "Ronan" :: HNil
val hlist2 = "JIJI" :: true :: 3 :: HNil
sops.Union[Int :: String :: HNil, String :: Boolean :: Int :: HNil].apply(hlist1, hlist2)

sops.Prepend[Int :: String :: HNil, String :: Boolean :: Int :: HNil].apply(hlist1, hlist2)

sops.Unifier[Int :: String :: String :: Boolean :: Int :: HNil]

sops.Intersection.apply[Int :: String :: HNil, String :: Boolean :: Int :: HNil].apply(hlist1)

// sops.SelectAll[Int :: String :: HNil, String :: Boolean :: Int :: HNil]

case class Person(name: String, age: Int)
case class Person2(age: Int, name: String)
case class Name(name: String)

// class MapperOps[HA <: HList](a: Int)

implicit class MapperOps[A, HA <: HList, HB <: HList, HC <: HList, HD <: HList](a: A) {
  def to[B](implicit
      genA: LabelledGeneric.Aux[A, HA],
      genB: LabelledGeneric.Aux[B, HB],
      select: sops.SelectAll[HA, HB]
  ): B = {
    genB.from(select(genA.to(a)))
  }

  def withCD[B, C, D](c: C)(implicit
      genA: LabelledGeneric.Aux[A, HA],
      genB: LabelledGeneric.Aux[B, HB],
      genC: LabelledGeneric.Aux[C, HC],
      genD: LabelledGeneric.Aux[D, HD],
      select: sops.SelectAll[HA, HB],
      union: sops.Union.Aux[HB, HC, HD]
  ) = {
    val b  = genB.from(select(genA.to(a)))
    val un = union(genB.to(b), genC.to(c))
    genD.from(un)
  }
  def withC[B, C](c: C)(implicit
      genA: LabelledGeneric.Aux[A, HA],
      genB: LabelledGeneric.Aux[B, HB],
      genC: LabelledGeneric.Aux[C, HC],
      intersection: sops.Intersection.Aux[HA, HB, HA],
      union: sops.Union.Aux[HA, HC, HB]
  )= {
    val ha = genA.to(a)
    val int  = intersection(ha)
    val uni = union(ha, genC.to(c))
    // val un = union(genB.to(b), genC.to(c))
    // genD.from(un)
    genB.from(uni)
  }
}

Person("Someone", 100).to[Person2]

case class State(value: Boolean)
case class PersonWithState(name: String,age: Int,  value: Boolean)
// Person("Someone", 100).withCD[Person2, State, PersonWithState](State(true))

Person("Someone", 100).withC[PersonWithState, State](State(true))

trait EntityMapper[A, B] {
  def to(from: A): B
}
implicit def mkAutoEntityMapper[A, B, HA <: HList, HB <: HList](implicit
    genA: LabelledGeneric.Aux[A, HA],
    genB: LabelledGeneric.Aux[B, HB],
    select: sops.SelectAll[HA, HB]
): EntityMapper[A, B] = new EntityMapper[A, B] {
  override def to(from: A): B = from.to[B]
}

implicitly[EntityMapper[Person, Name]].to(Person("Someone", 100))
