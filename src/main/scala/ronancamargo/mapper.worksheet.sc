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

val genPerson: LabelledGeneric[Person] {
  type Repr = String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged["name"], String] ::
    Int with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged["age"], Int] ::
    shapeless.HNil
} = LabelledGeneric[Person]

val genPerson2: LabelledGeneric[Person2] {
  type Repr = Int with shapeless.labelled.KeyTag[
    Symbol with shapeless.tag.Tagged["age"],
    Int
  ] :: String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged["name"], String] :: shapeless.HNil
} = LabelledGeneric[Person2]

import shapeless.syntax.singleton._
import shapeless.labelled.FieldType
val record = Symbol("name") ->> "Ronan"
// val record2: FieldType["name", String] = Symbol("name") ->> "Ronan"