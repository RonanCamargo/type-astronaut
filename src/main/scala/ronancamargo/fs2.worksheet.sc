import cats.effect
import cats.Id
import fs2._
import cats.data._
import cats.effect._

val stream = Stream
  .emits(List(1, 2, 3))
  .map(_ + 2) ++ Stream.emits(List(4, 5, 6))

// Stream.range(1,1000000000).take(2).compile.toList

// (1 to 10000000).toList
// (1 to 10005000).toList.take(1)

Stream.range(1, 10005000).take(5).compile.toList
// Stream.range(1, 10005000).compile.toList.take(5)

import cats.effect.unsafe.implicits.global
import scala.concurrent.duration._

10005000 / 2
// 5002500
// Stream
//   .evalSeq(IO(List.range(1, 5002500)))
//   .append(
//     Stream
//       .evalSeq(IO(List.range(1, 5002501)))
//   )
//   .append(
//     Stream
//       .evalSeq(IO(List.range(1, 5002503)))
//   )
//   .append(
//     Stream
//       .evalSeq(IO(List.range(1, 5002502)))
//   )
//   .append(
//     Stream
//       .evalSeq(IO(List.range(1, 5002500)))
//   )
//   .append(
//     Stream
//       .evalSeq(IO(List.range(1, 5002500)))
//   )
//   // .metered(500.millis)
//   .take(4)
//   .compile
//   .toList
//   .unsafeRunSync()

// List.range(1, 10005000)
// List.range(1, 5002500)
// List.range(1, 5002500)
// List.range(1, 5002500)

Stream.emit(1).covary[IO].map(_ * 2).compile.toList.unsafeRunSync()

import fs2.Pipe

Stream(1, 2, 3)
  .through(s => s.evalMap(x => IO(x)))
  .compile
  .toList
  .unsafeRunSync()

Stream(1, 2, 3).attempt.compile.toList
Stream(1, 2, 3).changes.toList
Stream(1, 2, 3).as("a").toList

val multiplyByTen: Pipe[IO, Int, Int] = _.map(_ * 10)
val multiplyByTwo: Pipe[IO, Int, Int] = _.map(_ * 2)
val failedPipe: Pipe[IO, Int, Int]    = s => Stream.raiseError[IO](new RuntimeException("RIP"))
// val toString: Pipe[Id, Int, String] = _.map(_.toString())
val broadcasted: Stream[IO, Int]      = Stream(1, 2, 3).broadcastThrough(multiplyByTen, multiplyByTwo, failedPipe)
broadcasted.attempt.compile.toList.unsafeRunSync()

import cats.implicits._
Stream(1, 2, 3).covary[IO].either(Stream(4, 5, 6)).compile.toList.unsafeRunSync()

Stream(1, 2, 3).imap(_.toString())(_.toInt)

Stream(1, 2, 3).covary[IO]

val s3 = for {
  s  <- Stream(1, 2, 3)
  s2 <- Stream(4, 5, 6)
} yield (s, s2)

s3.toList

IO(throw new RuntimeException("Error")).forceR(IO(2)).unsafeRunSync

import cats.implicits._
Option(1) *> Option(10)

// import cats.syntax.applicativeError._
// import cats.effect.instances.all._
// case class Error(message: String) extends RuntimeException(message)
// IO(1).attemptNarrow[RuntimeException]

// IO(1).timed.unsafeRunSync

// (IO(Thread.sleep(2)).timed,IO(Thread.sleep(2)).timed, IO(Thread.sleep(3)).timed).parTupled.timed.unsafeRunSync()

// (IO(Thread.sleep(2)).timed,IO(Thread.sleep(2)).timed, IO(Thread.sleep(3)).timed).tupled.timed.unsafeRunSync()

// IO.sleep(2.second).timed.unsafeRunSync

val list = List(1.asRight[String], "error1".asLeft[Int], "error2".asLeft[Int])
val list2 = List(5.asRight[String], "error3".asLeft[Int], "error4".asLeft[Int])



{
  for {
    a <- EitherT(list)
    b = a * 10
    c <- EitherT(list2)
  } yield b + c
}.value

val pull: Pull[Pure,Int,Unit] = Pull.output1(1) >> Pull.output1(2) >> Pull.output1(3) >> Pull.done
pull.stream.compile.toList

val pull2 = Pull.output1[IO, Int](1) >> Pull.eval(IO(1))
// pull2.stream
