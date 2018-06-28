package transform

import mycats.{Applicative, Functor, Monad}

import scala.language.higherKinds

final case class OptionT[F[_], A](value: F[Option[A]]) {

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(_ map f))

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(
      F.flatMap(value) {
        case None => F.pure(Option.empty[B])
        case Some(a) => f(a).value
      }
    )
  /* same as:
    def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
      OptionT(
        F.flatMap(value)(optA =>
          optA.map(a => f(a).value)
            .getOrElse(F.pure(Option.empty[B]))
      )
    )
  */
  /* same as:
    def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
      OptionT(
        F.flatMap(value)(_.fold(F.pure(Option.empty[B]))(a => f(a).value))
      )
  */

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    flatMap(f andThen OptionT.apply)

  def isDefined(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isDefined)
  def isEmpty(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isEmpty)

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] = F.map(value)(_.getOrElse(default))

  def fold[B](default: => B)(f: A => B)(implicit F: Functor[F]): F[B] = F.map(value)(_.map(f).getOrElse(default))
}

object OptionT {

  implicit def functor[F[_] : Functor]: Functor[OptionT[F, ?]] = new Functor[OptionT[F, ?]] {
    override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa map f
  }

  implicit def monad[F[_]: Monad]: Monad[OptionT[F, ?]] = new Monad[OptionT[F, ?]] {
    override def pure[A](a: A): OptionT[F, A] = OptionT(Monad[F].pure(Option(a)))
    override def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa flatMap f
  }

  def fromOption[F[_]: Applicative, A](opt: Option[A]): OptionT[F, A] = OptionT(Applicative[F].pure(opt))

  def liftF[F[_]: Functor, A](fa: F[A]): OptionT[F, A] = OptionT(Functor[F].map(fa)(Option.apply))
}
