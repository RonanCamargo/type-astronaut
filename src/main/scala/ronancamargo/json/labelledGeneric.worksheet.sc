sealed trait JsonValue

case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
case class JsonArray(items: List[JsonValue])             extends JsonValue
case class JsonString(value: String)                     extends JsonValue
case class JsonNumber(value: Double)                     extends JsonValue
case class JsonBoolean(value: Boolean)                   extends JsonValue
case object JsonNull                                     extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}
object JsonEncoder   {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A]                          = enc
  def createEncoder[A](f: A => JsonValue): JsonEncoder[A]                             = new JsonEncoder[A] {
    override def encode(value: A): JsonValue = f(value)
  }

  implicit val stringEncoder: JsonEncoder[String]                                     =
    createEncoder(str => JsonString(str))
  implicit val doubleEncoder: JsonEncoder[Double]                                     =
    createEncoder(num => JsonNumber(num))
  implicit val intEncoder: JsonEncoder[Int]                                           =
    createEncoder(num => JsonNumber(num))
  implicit val booleanEncoder: JsonEncoder[Boolean]                                   =
    createEncoder(bool => JsonBoolean(bool))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]]     =
    createEncoder(list => JsonArray(list.map(enc.encode)))
  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))
}

case class IceCream(name: String, numCherries: Int, inCone: Boolean)
val iceCream = IceCream("Sundae", 1, false)
// Ideally we'd like to produce something like this:
val iceCreamJson: JsonValue =
  JsonObject(
    List(
      "name"        -> JsonString("Sundae"),
      "numCherries" -> JsonNumber(1),
      "inCone"      -> JsonBoolean(false)
    )
  )

//Start with LabelledGeneric
import shapeless.LabelledGeneric
val generic = LabelledGeneric[IceCream].to(iceCream)

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}
def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
  new JsonObjectEncoder[A] {
    def encode(value: A): JsonObject =
      fn(value)
  }
import shapeless.{::, HList, HNil, Lazy}
implicit val hnilEncoder: JsonObjectEncoder[HNil]                     =
  createObjectEncoder(hnil => JsonObject(Nil))

import shapeless.Witness
import shapeless.labelled.FieldType
implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
): JsonObjectEncoder[FieldType[K, H] :: T] = {
  val fieldName = witness.value.name
  createObjectEncoder { hlist =>
    val head = hEncoder.value.encode(hlist.head)
    val tail = tEncoder.encode(hlist.tail)
    JsonObject((fieldName, head) :: tail.fields)
  }
}

implicit def genericObjectEncoder[A, H](implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
): JsonEncoder[A] =
  createObjectEncoder { value => hEncoder.value.encode(generic.to(value)) }

//Result
JsonEncoder[IceCream].encode(iceCream)

//Witness
implicitly[Witness.Aux[1]].value
Witness.mkWitness(2).value

//Define a coproduct

sealed trait Shape
case class Rectangle(width: Double, height: Double) extends Shape
case class Circle(radius: Double)                   extends Shape

//Coproduct encoders
import shapeless.{:+:, CNil, Coproduct, Inl, Inr, Lazy, Witness}

implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
  createObjectEncoder(_ => throw new Exception("RIP"))

implicit def coproductObjectEncoder[H, T <: Coproduct, K <: Symbol](implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
): JsonObjectEncoder[FieldType[K, H] :+: T] = {
  val typeName = witness.value.name

  createObjectEncoder {
    case Inl(h) => JsonObject(List(typeName -> hEncoder.value.encode(h)))
    case Inr(t) => tEncoder.encode(t)
  }
}

val shape: Shape = Circle(1.0)
JsonEncoder[Shape].encode(shape)
