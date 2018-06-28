package transform

import mycats.Monad

import scala.language.higherKinds

final case class ListOption[A](value: List[Option[A]]) {

  def map[B](f: A => B): ListOption[B] =
    ListOption(value.map(optA => optA map f))

  def flatMap[B](f: A => ListOption[B]): ListOption[B] =
    ListOption(
      value.flatMap {
        case None => List(Option.empty[B])
        case Some(a) => f(a).value
      }
    )

  def flatMapF[B](f: A => List[Option[B]]): ListOption[B] =
    flatMap(a => ListOption(f(a)))

  def isDefined: List[Boolean] = value.map(_.isDefined)
  def isEmpty: List[Boolean] = value.map(_.isEmpty)

  def getOrElse(default: => A): List[A] = value.map(_.getOrElse(default))

  def fold[B](default: => B)(f: A => B): List[B] = value.map(_.map(f).getOrElse(default))
}

object ListOption {

  implicit def monad: Monad[ListOption] = new Monad[ListOption] {
    override def pure[A](a: A): ListOption[A] = ListOption(List(Option(a)))
    override def flatMap[A, B](fa: ListOption[A])(f: A => ListOption[B]): ListOption[B] = fa flatMap f
  }
}

object ListOptionApp extends App {

  println("\n----- Creating ListOption")

  val loi = List(Some(1), None, Some(2), Some(3))

  val otli = ListOption[Int](loi)
  println(otli)
  println(otli.value)

  println("\n----- flatMap")

  def fillListWith(x: Int): List[Option[String]] = List.fill(x)(Option(x.toString))

  val otliFlatMapped = otli.flatMap(x => ListOption[String](fillListWith(x)))

  println(otliFlatMapped)
  println(otliFlatMapped.value)

  println("\n----- flatMapF")

  val otliFlatMappedF = otli.flatMapF(x => fillListWith(x))
  println(otliFlatMappedF)
  println(otliFlatMappedF.value)

  println("\n----- map")

  val otliMapped = Monad[ListOption].map(otli) { _.toString + "!" }
  println(otliMapped)

  println("\n----- for comprehension")

  val result4: ListOption[Int] = for {
    x <- otli
    y <- otli
  } yield x * y

  println(result4)
  println(result4.value)

  println("-----\n")
}

