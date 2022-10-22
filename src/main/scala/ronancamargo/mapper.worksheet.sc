import javax.swing.text.html.parser.Entity
import cats.instances.map
import org.w3c.dom.html.HTMLIsIndexElement
import shapeless._
import shapeless.ops.hlist._

val sops = shapeless.ops.hlist

val hlist1 = 1 :: "Ronan" :: HNil
val hlist2 = "" :: true :: 3 :: HNil
sops.Union[Int :: String :: HNil, String :: Boolean :: Int :: HNil].apply(hlist1, hlist2)

sops.Prepend[Int :: String :: HNil, String :: Boolean :: Int :: HNil].apply(hlist1, hlist2)

sops.Unifier[Int :: String :: String :: Boolean :: Int :: HNil]

sops.Intersection.apply[Int :: String :: HNil, String :: Boolean :: Int :: HNil].apply(hlist1)

// sops.SelectAll[Int :: String :: HNil, String :: Boolean :: Int :: HNil]

case class Person(name: String, age: Int)
case class Person2(age: Int, name: String)
case class Name(name: String)

// class MapperOps[HA <: HList](a: Int)

class Adder[A, B, HA <: HList, HB <: HList, HC <: HList](a: A)(implicit
    genA: LabelledGeneric.Aux[A, HA],
    genB: LabelledGeneric.Aux[B, HB],
    intersection: sops.Intersection.Aux[HA, HB, HA]
) {
  def adding[C](c: C)(implicit genC: LabelledGeneric.Aux[C, HC], union: sops.Union.Aux[HA, HC, HB]): B = {
    val ha  = genA.to(a)
    val int = intersection(ha)
    val uni = union(ha, genC.to(c))
    genB.from(uni)

  }
}

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
  ) = {
    val ha  = genA.to(a)
    val int = intersection(ha)
    val uni = union(ha, genC.to(c))
    genB.from(uni)
  }
  def targetTo[B](implicit
      genA: LabelledGeneric.Aux[A, HA],
      genB: LabelledGeneric.Aux[B, HB],
      intersection: sops.Intersection.Aux[HA, HB, HA]
  ): Adder[A, B, HA, HB, HC] = new Adder[A, B, HA, HB, HC](a)

  def permutations(implicit genA: LabelledGeneric.Aux[A, HA], perm: sops.Permutations[HA]) = perm(genA.to(a))

  def test[B](implicit genA: LabelledGeneric.Aux[A, HA], t: sops.Align[HA, HB]) = t(genA.to(a))
}

Person("Someone", 100).to[Person2]

case class State(value: Boolean)
case class PersonWithState(name: String, age: Int, value: Boolean)
// Person("Someone", 100).withCD[Person2, State, PersonWithState](State(true))

Person("Someone", 100).withC[PersonWithState, State](State(true))

Person("Someone", 100).targetTo[PersonWithState].adding(State(true))

trait EntityMapper[A, B] { self =>
  def to(from: A): B

  def map[C](f: B => C): EntityMapper[A, C] = new EntityMapper[A, C] {
    override def to(from: A): C = f(self.to(from))
  }
}
implicit def mkAutoEntityMapper[A, B, HA <: HList, HB <: HList](implicit
    genA: LabelledGeneric.Aux[A, HA],
    genB: LabelledGeneric.Aux[B, HB],
    select: sops.SelectAll[HA, HB]
): EntityMapper[A, B] = new EntityMapper[A, B] {
  override def to(from: A): B = genB.from(select(genA.to(from)))
}

implicitly[EntityMapper[Person, Name]].to(Person("Someone", 100))

PersonWithState("Name", 10, true).permutations
Person("a", 1).test[Name]
Person2(1, "a").test[PersonWithState]
PersonWithState("a", 1, true).test[Person2]

implicitly[EntityMapper[Person, Name]].map(_.name)
