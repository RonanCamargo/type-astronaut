package ronancamargo.firestore

import mouse.all._
import ronancamargo.firestore.ast._
import shapeless.HNil
import shapeless.HList
import shapeless.Lazy
import shapeless.::
import shapeless.Witness
import shapeless.labelled.FieldType
import shapeless.LabelledGeneric

trait FirestoreEncoder[A] {
  def encode(a: A): FirestoreDocument
}

trait FirestoreObjectEncoder[A] extends FirestoreEncoder[A] {
  def encode(a: A): FirestoreObject
}

object FirestoreEncoder {
  def apply[A](implicit encoder: FirestoreEncoder[A]): FirestoreEncoder[A] = encoder

  def instance[A](f: A => FirestoreDocument): FirestoreEncoder[A] = new FirestoreEncoder[A] {
    override def encode(a: A): FirestoreDocument = f(a)
  }

  def instanceObject[A](f: A => FirestoreObject): FirestoreObjectEncoder[A] = new FirestoreObjectEncoder[A] {
    override def encode(a: A): FirestoreObject = f(a)
  }

  implicit val intEncoder: FirestoreEncoder[Int]                            = instance[Int](FirestoreInt)
  implicit val stringEncoder: FirestoreEncoder[String]                      = instance[String](FirestoreString)
  implicit val booleanEncoder: FirestoreEncoder[Boolean]                    = instance[Boolean](FirestoreBoolean)

  implicit def optionEncoder[A](implicit encoder: FirestoreEncoder[A]): FirestoreEncoder[Option[A]]   =
    instance[Option[A]](_.map(encoder.encode).getOrElse(FirestoreNull))
  implicit def listEncoder[A](implicit encoder: FirestoreEncoder[A]): FirestoreEncoder[List[A]]       =
    instance[List[A]](_.map(encoder.encode).|>(FirestoreArray))
  implicit def mapEncoder[A](implicit encoder: FirestoreEncoder[A]): FirestoreEncoder[Map[String, A]] =
    instance[Map[String, A]](map => map.map { case (k, v) => k -> encoder.encode(v) }.|>(FirestoreMap))

  implicit val hnilEncoder: FirestoreObjectEncoder[HNil] = instanceObject[HNil](_ => FirestoreObject(Nil))

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[FirestoreEncoder[H]],
      tEncoder: FirestoreObjectEncoder[T]
  ): FirestoreObjectEncoder[FieldType[K, H] :: T] = {
    val name = witness.value.name
    instanceObject { hlist =>
      val head: (String, FirestoreDocument) = name -> hEncoder.value.encode(hlist.head)
      val tail                              = tEncoder.encode(hlist.tail)
      FirestoreObject(head :: tail.fields)
    }
  }

  implicit def genericObjectEncoder[A, H](implicit
      generic: LabelledGeneric.Aux[A, H],
      hEncoder: Lazy[FirestoreObjectEncoder[H]]
  ): FirestoreEncoder[A] =
    instanceObject { value => hEncoder.value.encode(generic.to(value)) }
}
