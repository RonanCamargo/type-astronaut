package ronancamargo.json.data

sealed trait Shape
case class Rectangle(width: Double, height: Double) extends Shape
case class Circle(radius: Double)                   extends Shape
