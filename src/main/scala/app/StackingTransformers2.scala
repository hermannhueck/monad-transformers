package app

import cats.data.{EitherT, OptionT}
import cats.instances.future._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object StackingTransformers2 extends App {

  type FutureEither[A]       = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val stackedResult: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  Await.ready(
    stackedResult.value.value,
    3.seconds
  )

  println(stackedResult) // OptionT(EitherT(Future(Success(Right(Some(42))))))

  println(stackedResult.value) // EitherT(Future(Success(Right(Some(42)))))

  println(stackedResult.value.value) // Future(Success(Right(Some(42))))

  val future: Future[Either[String, Option[Int]]] = stackedResult.value.value

  val result: Either[String, Option[Int]] = Await.result(
    stackedResult.value.value,
    3.seconds
  ) // Right(Some(42))

  println(result)
}
