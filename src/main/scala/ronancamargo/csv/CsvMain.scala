package ronancamargo.csv

import ronancamargo.csv.data.Employee
import shapeless.Generic

import scala.util.chaining._

object CsvMain extends App {

//  implicit val employeeEncoder: CsvEncoder[Employee] = new CsvEncoder[Employee] {
//    override def encode(a: Employee): List[String] = List(a.name, a.number.toString, if (a.manager) "yes" else "no")
//  }
//
  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]): String =
    values.map(v => encoder.encode(v).mkString(",")).mkString("\n")

  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  writeCsv(employees).tap(println)

  val employeeGen = Generic[Employee]

}
