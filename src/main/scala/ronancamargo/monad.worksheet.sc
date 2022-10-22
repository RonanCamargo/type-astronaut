import cats._
import scala.util.Try
val num = 1
List(num)

val pure: Int => List[Int] = x => List(x)
pure(num)

Option(num)
val pureOption: Int => Option[Int] = x => Option(x)
pureOption(num)

val nestedList = List(List(num))
nestedList.flatten

val nestedOption = Option(Option(num))
nestedOption.flatten

val nestedOption2 = Option(Option.empty[Int])
nestedOption2.flatten

val nestedList2 = List(List[Int]())
nestedList2.flatten

val right: Either[Int, Int]                   = Right(1)
val nestedLeft: Either[Int, Either[Int, Int]] = Right(right)

nestedLeft.flatten

val multiplyBy3: Int => Int = x => x * 3

List(3, 6, 9).map(multiplyBy3)
Option(3).map(multiplyBy3)
Try(3).map(multiplyBy3)

List(1).map(multiplyBy3)
List().map(multiplyBy3)

Option(1).map(multiplyBy3)
Option.empty[Int].map(multiplyBy3)

right.map(multiplyBy3)

val left: Either[Int, Int] = Left(1)
left.map(multiplyBy3)

left.map(multiplyBy3).map(multiplyBy3).map(multiplyBy3).map(multiplyBy3) == left

Option(1).map(x => x + 1)

Option(1).map(x => Option(x + 1))

Option(1).map(x => Option(x + 1)).flatten
Option(1).flatMap(x => Option(x + 1))

Option(1).flatMap(x => None)
Option(1).map(x => None)
Option(1).map(x => None).flatten

type Message = String
def validateLength(n: Int)(s: String): Either[Message, String] =
  if (s.length() <= n) Right(s)
  else Left("Invalid length")

def isPalindrome(s: String): Either[Message, String] =
  if (s == s.reverse) Right(s)
  else Left("Not palindrome")

Right("Hola").flatMap(x => validateLength(3)(x))
Right("Hola").map(x => validateLength(3)(x)).flatten

//=================
Right("Hola")
  .flatMap(x => validateLength(3)(x))
  .flatMap(x => isPalindrome(x))
for {
  x           <- Right("Hola")
  validLength <- validateLength(3)(x)
  palindrome  <- isPalindrome(validLength)
} yield x

//=============
Right("Hola")
  .flatMap(x => validateLength(4)(x))
  .flatMap(x => isPalindrome(x))
for {
  x           <- Right("Hola")
  validLength <- validateLength(4)(x)
  palindrome  <- isPalindrome(x)
} yield x

//======
import cats.implicits._

Right("Hola").flatMap(validateLength(4)).flatMap(isPalindrome)

"Hola".asRight[Message] >>= validateLength(4) >>= isPalindrome

Right("Hola")
  .flatMap(x =>
    validateLength(4)(x)
      .flatMap(validLength => isPalindrome(validLength))
  )

List(List(1, 2, 3), List(4, 5, 6)).flatten

Monad[Option].unit
Applicative[Option].unit

import cats.implicits._

Monad[Option].unit

Either.unit

//====map
Option("Hello")
//Option(5) //5 is the string length
//Option(())
//Option("World")

List("List", "of", "tuples")
//List(("List", 4), ("of", 2), ("tuples", 6)) //Tuple with length
//List(("List", "Hello"),("of", "Hello"),("tuples","Hello")) //"Hello" is a fixed value

//====flatMap
Option(1)
//None
//Option("Hello")

List(List(1, 2, 3), Nil, List(4, 5, 6))
//List(1, 2, 3, 4, 5, 6)


import io.circe.generic.auto._
import io.circe.syntax._
import cats.data._

case class Result[A](value: A)
Result(List(1)).asJson.noSpaces
Result(1.asRight[String]).asJson.noSpaces
// Result(Ior.right[String](List(1))).asJson.noSpaces

Bifunctor[(*, *)].bimap((1,1))(_ + 1, _ + 2)
