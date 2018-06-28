package mycats

import scala.concurrent.Future
import scala.language.higherKinds

// typeclass Functor
trait Functor[F[_]] extends Any { self =>

  // intrinsic abstract Functor method

  def map[A, B](fa: F[A])(f: A => B): F[B]


  // method implementations in terms of map

  def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f) // alias for map

  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa)(f)

  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] = as(fa, ())

/*
  def compose[G[_]: Functor]: Functor[Lambda[X => F[G[X]]]] = new Functor[Lambda[X => F[G[X]]]] {
    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
      self.map(fga)(ga => implicitly[Functor[G]].map(ga)(f))
  }
*/

  def compose[G[_]: Functor]: Functor[Lambda[X => F[G[X]]]] =
    new Functor.Composite[F, G] {
      def F: Functor[F] = self
      def G: Functor[G] = Functor[G]
    }
}

object Functor {

  trait Composite[F[_], G[_]] extends Any with Functor[Lambda[X => F[G[X]]]] {
    def F: Functor[F]
    def G: Functor[G]
    override def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      F.map(fa)(G.lift(f))
  }

  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

  // default typeclass instances in implicit scope

  implicit def listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def futureFunctor: Functor[Future] = new Functor[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  implicit def idFunctor: Functor[Id] = new Functor[Id] {
    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }

  implicit def eitherFunctor[L]: Functor[Either[L, ?]] = new Functor[Either[L, ?]] {
    override def map[R1, R2](fa: Either[L, R1])(f: R1 => R2): Either[L, R2] = fa map f
  }

  object ops {

    implicit class FunctorF[F[_]: Functor, A](ctx: F[A]) {

      private val F = Functor[F]

      def map[B](f: A => B): F[B] = F.map(ctx)(f)
      def fmap[B](f: A => B): F[B] = map(f)
    }
  }
}
