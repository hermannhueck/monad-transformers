package transform

import mycats.Monad

import scala.language.higherKinds

final case class MyOptionT[F[_]: Monad, A](value: F[Option[A]]) {

  val F = Monad[F]

  def map[B](f: A => B): MyOptionT[F, B] =
    MyOptionT(F.map(value)(optA => optA map f))

  def flatMap[B](f: A => MyOptionT[F, B]): MyOptionT[F, B] =
    MyOptionT(
      F.flatMap(value) {
        case None    => F.pure(Option.empty[B])
        case Some(a) => f(a).value
      }
    )

  def flatMapF[B](f: A => F[Option[B]]): MyOptionT[F, B] =
    flatMap(a => MyOptionT(f(a)))

  def isDefined: F[Boolean] = F.map(value)(_.isDefined)
  def isEmpty: F[Boolean]   = F.map(value)(_.isEmpty)

  def getOrElse(default: => A): F[A] = F.map(value)(_.getOrElse(default))

  def fold[B](default: => B)(f: A => B): F[B] = F.map(value)(_.map(f).getOrElse(default))
}

object MyOptionT {

  implicit def monad[F[_]: Monad]: Monad[MyOptionT[F, *]] = new Monad[MyOptionT[F, *]] {
    override def pure[A](a: A): MyOptionT[F, A]                                               = MyOptionT(Monad[F].pure(Option(a)))
    override def flatMap[A, B](fa: MyOptionT[F, A])(f: A => MyOptionT[F, B]): MyOptionT[F, B] = fa flatMap f
  }
}

object MyOptionTApp extends App {

  println("\n----- Creating context")

  val loi = List(Some(1), None, Some(2), Some(3))

  val otli = MyOptionT[List, Int](loi)
  println(otli)
  println(otli.value)

  println("\n----- flatMap")

  def fillListWith(x: Int): List[Option[String]] = List.fill(x)(Option(x.toString))

  val otliFlatMapped = otli.flatMap(x => MyOptionT[List, String](fillListWith(x)))

  println(otliFlatMapped)
  println(otliFlatMapped.value)

  println("\n----- flatMapF")

  val otliFlatMappedF = otli.flatMapF(x => fillListWith(x))
  println(otliFlatMappedF)
  println(otliFlatMappedF.value)

  println("\n----- map")

  val otliMapped = otli.map { _.toString + "!" }
  println(otliMapped)

  println("\n----- for comprehension")

  val result4: MyOptionT[List, Int] = for {
    x <- otli
    y <- otli
  } yield x * y

  println(result4)
  println(result4.value)

  println("-----\n")
}
