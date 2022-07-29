import org.w3c.dom.html.HTMLIsIndexElement
import shapeless.ops._
import shapeless.ops.hlist.HListToProduct
import shapeless.ops.product.ToHList
import shapeless._

HListToProduct[Int :: String :: HNil].apply(1 :: "" :: HNil)
ToHList[(Int, (String, Int))].apply((1, ("", 1)))

import shapeless.ops.adjoin.Adjoin

case class ABC(a: Int, b: String, c: Boolean)
Adjoin[Int :: (Int :: String :: HNil) :: HNil].apply(1 :: (2 :: "Hello" :: HNil) :: HNil)

implicit def toHlistRec[A, H <: HList, O <: HList](
    tuple: Tuple2[A, H]
)(implicit isCons: hlist.IsHCons[H], toHList: product.ToHList[Tuple2[A, H]]) = {

  toHList(tuple)
}

toHlistRec[Int, Int :: String :: HNil, Int :: HNil]((1, 1 :: "" :: HNil))

(1 :: "A" :: 2 :: HNil).tupled

import shapeless.syntax.std.TupleOps
import shapeless.syntax._
(1, "a")

val myTuple: (((Int, String), Boolean), Double) = 1 -> "Hello" -> true -> 2.1
tuple.IsComposite[(((Int, String), Boolean), Double)].head(myTuple)

import shapeless._
object poly2 extends Poly2 {
  implicit val a = at[String, String](_ :: _ :: HNil)
}
import syntax.std.tuple._

myTuple.productElements

def map[H, T <: HList](hlist: H :: T) = {
  hlist match {
    case head :: tail => head :: HNil
  }
}

def tup2[A](tuple: (A, A)): A :: A :: HNil = tuple._1 :: tuple._2 :: HNil

def tup[H <: Tuple2[_, _], T](tuple: (H, T)) = {
  tup2(tuple._1) :: tuple._2 :: HNil
}

def toHlist[A, B](tuple: (A, B)) = {
  tuple match {
    case (a: Tuple2[_, _], b) => tup2(tuple)
    case (a: B, b: B)         => tup2((a, b))
  }
}

toHlist(("1" -> "2" -> "3"))

map[Int, Int :: HNil](1 :: 2 :: HNil)

object stringPolyFolder extends Poly2 {
  implicit val caseAt: Case.Aux[String, String, List[String]] = at[String, String] {
    case (s, "") => List(s)
    case (s, a)  => List(s, a)
  }
  implicit val case2 = at[(String, String), List[String]]((x, y) => x._1 :: x._2 :: y :: Nil)
}
("1" -> "2" -> "3").foldRight("")(stringPolyFolder)
