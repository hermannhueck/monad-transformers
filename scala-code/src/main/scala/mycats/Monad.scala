package mycats

import scala.language.higherKinds

// typeclass Monad
trait Monad[F[_]] extends Any with Applicative[F] { self =>

  // intrinsic abstract Applicative methods

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]


  // method implementations in terms of pure and flatMap

  def >>=[A, B](fa: F[A])(f: A => F[B]): F[B] = flatMap(fa)(f) // alias for flatMap

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)((f: A => B) => map(fa)(f))

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
}

object Monad {

  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  // default typeclass instances in implicit scope

  implicit def listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit def optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }
}
