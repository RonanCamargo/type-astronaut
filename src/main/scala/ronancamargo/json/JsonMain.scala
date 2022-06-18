package ronancamargo.json

import ronancamargo.json.data.{Circle, IceCream, Shape}

import scala.util.chaining._

object JsonMain extends App {

  val shape: Shape = Circle(1.0)
  JsonEncoder[Shape].encode(shape).tap(println)

  val iceCream = IceCream("Sundae", 1, false)
  JsonEncoder[IceCream].encode(iceCream).tap(println)

}
