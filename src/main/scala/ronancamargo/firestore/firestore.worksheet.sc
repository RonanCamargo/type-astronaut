import ronancamargo.firestore.FirestoreEncoder
import ronancamargo.firestore.ast._
case class Person(
    name: String,
    age: Int,
    optional: Option[Int] = None,
    numbers: List[Int] = Nil,
    children: List[Person] = Nil
)

FirestoreEncoder[String].encode("Ronan")
FirestoreEncoder[Int].encode(1)
FirestoreEncoder[Boolean].encode(true)

FirestoreEncoder[Option[Int]].encode(Some(1))
FirestoreEncoder[Option[Int]].encode(None)
FirestoreEncoder[List[Int]].encode(List(1, 2, 3))
FirestoreEncoder[List[Int]].encode(Nil)

val elDiego = Person("El Diego", 51, None, List(1, 2, 3), List(Person("Dalma", 30)))
val encoded = FirestoreEncoder[Person].encode(elDiego)

FirestoreEncoder[Map[String, Person]].encode(Map("10" -> elDiego))

import scala.jdk.CollectionConverters._
def toFirestore(document: FirestoreDocument): AnyRef = document match {
  case FirestoreArray(array)     => array.map(toFirestore).asJava
  case FirestoreString(string)   => string.asInstanceOf[AnyRef]
  case FirestoreNull             => null
  case FirestoreObject(fields)   => fields.map { case (k, v) => k -> toFirestore(v) }.toMap.asJava
  case FirestoreInt(n)           => n.asInstanceOf[AnyRef]
  case FirestoreMap(map)         => map.map { case (k, v) => k -> toFirestore(v) }.asJava
  case FirestoreBoolean(boolean) => boolean.asInstanceOf[AnyRef]
}

toFirestore(encoded)
