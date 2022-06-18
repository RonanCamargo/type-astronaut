package ronancamargo

object Main extends App {

  sealed abstract class Document extends Product with Serializable {}

  final case class DocBoolean(value: Boolean) extends Document
  final case class DocLong(value: Long)       extends Document
}
