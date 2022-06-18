//Singleton types
import shapeless.syntax.singleton._
val one: 1 = 1.narrow
1

//Phantom types

val number      = 42
trait Cherries
val numCherries = number.asInstanceOf[Int with Cherries]

//->> comes from singleton package

import shapeless.labelled.{FieldType, KeyTag}
val betterNumCherries = "numCherries" ->> number
//Int with KeyTag[String("numCherries"),Int]

import shapeless.labelled.field
field[Cherries](123)

//type FieldType[K, V] = V with KeyTag[K, V]
val bnc: FieldType["numCherries", Int] = betterNumCherries
//Tags exist purely at compile 􏰀me and have no run􏰀me representa􏰀on

//How do we convert them to values we can use at run􏰀me?
//Shapeless provides a type class called Witness for this purpose
import shapeless.Witness
def getFieldName[K, V](field: FieldType[K, V])(implicit witness: Witness.Aux[K]): K = witness.value
getFieldName(bnc)
def getFieldValue[K, V](field: FieldType[K, V]): V                                  = field
getFieldValue(bnc)