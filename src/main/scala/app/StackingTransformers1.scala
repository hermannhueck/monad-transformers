package app

import scala.language.higherKinds

import cats.data.{EitherT, OptionT}
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object StackingTransformers1 extends App {

  // with intermediate types

  def compute__[A]: A => Future[Either[String, Option[A]]] = { input =>
    val option: Option[A] = Some(input)
    val either: Either[String, Option[A]] = Right(option)
    val future: Future[Either[String, Option[A]]] = Future(either)
    future
  }

  def futureEitherOption__[A]: A => OptionT[EitherT[Future, String, ?], A] = { input =>
    val future: Future[Either[String, Option[A]]] = compute__(input)
    val eitherT: EitherT[Future, String, Option[A]] = EitherT(future)
    val optionT: OptionT[EitherT[Future, String, ?], A] = OptionT(eitherT)
    optionT
  }

  // shorter and without intermediate types

  def compute[A]: A => Future[Either[String, Option[A]]] =
    input => Future(Right(Some(input)))

  def stackMonads[A]: A => OptionT[EitherT[Future, String, ?], A] =
    input => OptionT(EitherT(compute(input)))

  val stackedResult: OptionT[EitherT[Future, String, ?], Int] =
    for {
      a <- stackMonads(10)
      b <- stackMonads(32)
    } yield a + b

  Await.ready(
    stackedResult.value.value,
    3.seconds
  )

  println(stackedResult) // OptionT(EitherT(Future(Success(Right(Some(42))))))

  println(stackedResult.value) // EitherT(Future(Success(Right(Some(42)))))

  println(stackedResult.value.value) // Future(Success(Right(Some(42))))

  val future: Future[Either[String, Option[Int]]] = stackedResult.value.value

  val result: Either[String, Option[Int]] = Await.result(future, 3.seconds) // Right(Some(42))

  println(result.right.get)
}
