package ronancamargo.json

import shapeless.syntax.SingletonOps

object JsonMain extends App {
  import shapeless.syntax.singleton._
  val one = 1.narrow
  println(one.getClass)
}
