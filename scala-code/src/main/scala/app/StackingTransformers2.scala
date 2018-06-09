package app

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object StackingTransformers2 extends App {

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  Await.ready(
    futureEitherOr.value.value,
    3.seconds
  )

  println(futureEitherOr) // OptionT(EitherT(Future(Success(Right(Some(42))))))

  println(futureEitherOr.value) // EitherT(Future(Success(Right(Some(42)))))

  println(futureEitherOr.value.value) // Future(Success(Right(Some(42))))

  val future: Future[Either[String, Option[Int]]] = futureEitherOr.value.value

  val result: Either[String, Option[Int]] = Await.result(
    futureEitherOr.value.value,
    3.seconds
  ) // Right(Some(42))

  println(result.right.get)
}
