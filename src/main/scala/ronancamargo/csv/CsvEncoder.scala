package ronancamargo.csv

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

trait CsvEncoder[A] {
  def encode(a: A): List[String]
}

object CsvEncoder {
  def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] = encoder

  def instance[A](f: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    override def encode(a: A): List[String] = f(a)
  }

  implicit val stringEncoder: CsvEncoder[String]       = instance(List(_))
  implicit val booleanEncoder: CsvEncoder[Boolean]     = instance(b => List(if (b) "yes" else "no"))
  implicit val intEncoder: CsvEncoder[Int]             = instance(i => List(i.toString))
  implicit val doubleEncoder: CsvEncoder[Double]       = instance(d => List(d.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = instance(_ => Nil)
  implicit def hlistEncoder[H, T <: HList](implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    instance[H :: T] { case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t) }

  implicit def genericEncoder[A, R](implicit generic: Generic.Aux[A, R], encoder: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
    instance[A](a => encoder.value.encode(generic.to(a)))

  implicit val cnilEncoder: CsvEncoder[CNil] = instance(cnil => throw new Exception("Inconceivable!"))
  implicit def coproductEncoder[H, T <: Coproduct](implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] =
    instance {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }
}
