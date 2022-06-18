package ronancamargo.csv.data

sealed trait Shape

final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double)                   extends Shape
