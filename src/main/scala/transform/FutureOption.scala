package transform

import mycats.Monad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds

final case class FutureOption[A](value: Future[Option[A]]) {

  def map[B](f: A => B): FutureOption[B] =
    FutureOption(value.map(_ map f))

  def flatMap[B](f: A => FutureOption[B]): FutureOption[B] =
    FutureOption(
      value.flatMap {
        case None => Future.successful(Option.empty[B])
        case Some(a) => f(a).value
      }
    )

  def flatMapF[B](f: A => Future[Option[B]]): FutureOption[B] =
    flatMap(f andThen FutureOption.apply)

  def isDefined: Future[Boolean] = value.map(_.isDefined)
  def isEmpty: Future[Boolean] = value.map(_.isEmpty)

  def getOrElse(default: => A): Future[A] = value.map(_.getOrElse(default))

  def fold[B](default: => B)(f: A => B): Future[B] = value.map(_.map(f).getOrElse(default))
}

object FutureOption {

  implicit def monad: Monad[FutureOption] = new Monad[FutureOption] {
    override def pure[A](a: A): FutureOption[A] = FutureOption(Future.successful(Option(a)))
    override def flatMap[A, B](fa: FutureOption[A])(f: A => FutureOption[B]): FutureOption[B] = fa flatMap f
  }
}
