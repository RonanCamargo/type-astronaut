package ronancamargo.firestore.ast

sealed trait FirestoreDocument

case class FirestoreObject(fields: List[(String, FirestoreDocument)]) extends FirestoreDocument
case class FirestoreInt(n: Int)                                       extends FirestoreDocument
case class FirestoreBoolean(boolean: Boolean)                         extends FirestoreDocument
case class FirestoreString(string: String)                            extends FirestoreDocument
case class FirestoreArray(array: List[FirestoreDocument])             extends FirestoreDocument
case class FirestoreMap(map: Map[String, FirestoreDocument])          extends FirestoreDocument
case object FirestoreNull                                             extends FirestoreDocument
