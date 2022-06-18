package ronancamargo.csv

import ronancamargo.csv.data.{Circle, Employee, Rectangle, Shape}

import scala.util.chaining._

object CsvMain extends App {

  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]): String =
    values.map(v => encoder.encode(v).mkString(",")).mkString("\n")

  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  writeCsv(employees).tap(println)

  val shapes: List[Shape] = List(Circle(2), Rectangle(1, 5))

  writeCsv(shapes).tap(println)
}
