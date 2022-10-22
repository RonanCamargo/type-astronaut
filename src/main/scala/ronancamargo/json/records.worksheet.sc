import shapeless.Typeable
import shapeless.LabelledGeneric
import shapeless.Witness
import shapeless.Poly1
import shapeless.labelled
import shapeless.syntax.singleton._
import shapeless.{::, HList, HNil}
import shapeless.labelled.FieldType

val garfield: String with labelled.KeyTag["cat", String] :: Boolean with labelled.KeyTag["orange", Boolean] :: HNil =
  ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil

val betterTypedGarfield: FieldType["cat", String] :: FieldType["orange", Boolean] :: HNil = garfield

object getFieldName extends Poly1 {
  private def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K = witness.value
  implicit def caseName[K, V](implicit witness: Witness.Aux[K]): Case.Aux[FieldType[K, V], K] =
    at[FieldType[K, V]](getFieldName(_))
}

betterTypedGarfield.map(getFieldName)
