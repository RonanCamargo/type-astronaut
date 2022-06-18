package ronancamargo.csv

import shapeless.{::, Generic, HList, HNil}

trait CsvEncoder[A] {
  def encode(a: A): List[String]
}

object CsvEncoder {
  def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] = encoder

  def instance[A](f: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    override def encode(a: A): List[String] = f(a)
  }

  implicit val stringEncoder: CsvEncoder[String]       = CsvEncoder.instance(List(_))
  implicit val booleanEncoder: CsvEncoder[Boolean]     = CsvEncoder.instance(b => List(if (b) "yes" else "no"))
  implicit val intEncoder: CsvEncoder[Int]             = CsvEncoder.instance(i => List(i.toString))
  implicit val hnilEncoder: CsvEncoder[HNil]           = CsvEncoder.instance(_ => Nil)
  implicit def hlistEncoder[H, T <: HList](implicit
      hEncoder: CsvEncoder[H],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    CsvEncoder.instance[H :: T] { case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t) }

  implicit def genericEncoder[A, R](implicit generic: Generic.Aux[A, R], encoder: CsvEncoder[R]): CsvEncoder[A] =
    CsvEncoder.instance[A](a => encoder.encode(generic.to(a)))
}
