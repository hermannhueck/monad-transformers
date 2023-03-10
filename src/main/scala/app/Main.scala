package app

import scala.language.higherKinds

import cats._, cats.data._, cats.implicits._
// import mycats._, transform._, Functor.ops._, Monad.ops._

object Main extends App {

  println("\n----- for comprehension for List[Option[Int]]")

  val loi1: List[Option[Int]] = List(Some(1), None, Some(2), Some(3))
  val loi2: List[Option[Int]] = List(Some(1), None, Some(2), Some(3))

  val result1: List[Option[Int]] =
    for {
      oi1: Option[Int] <- loi1
      if oi1.isDefined
      oi2: Option[Int] <- loi2
      if oi2.isDefined
    } yield Option(oi1.get * oi2.get)
  println(result1)

  val result2 =
    loi1
      .filter(_.isDefined)
      .flatMap { oi1 =>
        loi2
          .filter(_.isDefined)
          .map { oi2 => Option(oi1.get * oi2.get) }
      }
  println(result2)

  val result3: List[Option[Int]] =
    for {
      oi1: Option[Int] <- loi1
      oi2: Option[Int] <- loi2
    } yield for {
      i1: Int <- oi1
      i2: Int <- oi2
    } yield i1 * i2
  println(result3)

  // Wanted: a for comprehension to process Ints nested in 2 contexts:
  // something like this one ...
  //
  //  val result4: List[Option[Int]] =
  //    for {
  //      i1: Int <- loi1
  //      i2: Int <- loi2
  //    } yield i1 * i2

  println("\n----- Mapping a List[Int] with a Functor[List]")

  val li = List(1, 2, 3)

  val lSquared1 = li.map(x => x * x) // invokes List.map
  println(lSquared1)
  val lSquared2 = li.fmap(x => x * x) // invokes Functor[List].fmap
  println(lSquared2)
  val lSquared3 = Functor[List].map(li)(x => x * x) // invokes Functor[List].map
  println(lSquared3)

  println("\n----- Mapping a List[Option[Int]] with Functor[List] and Functor[Option]")

  val loi = List(Some(1), None, Some(2), Some(3))
  // println(loi.map(x => x * x)) // does not compile
  val loSquared1 = loi.map(oi => oi.map(x => x * x))
  println(loSquared1)
  val loSquared2 = Functor[List].map(loi)(oi => Functor[Option].map(oi)(x => x * x))
  println(loSquared2)
  val loSquared3 = (Functor[List] compose Functor[Option]).map(loi)(x => x * x)
  println(loSquared3)
  val cf         = Functor[List] compose Functor[Option]
  val loSquared4 = cf.map(loi)(x => x * x)
  println(loSquared4)

  println("\n----- Pimping List[Option[A]]")

  implicit class PimpedListOptionA[A](list: List[Option[A]]) {
    val composedFunctor                     = Functor[List] compose Functor[Option] // functor type: Functor[Lambda[X => List[Option[X]]]]
    def map[B](f: A => B): List[Option[B]]  = composedFunctor.map(list)(f)
    def fmap[B](f: A => B): List[Option[B]] = composedFunctor.map(list)(f)
  }

  val loSquared5 = loi.fmap(x => x * x)
  println(loSquared5)

  // val loSquared6 = loi.map(x => x * x) // doesn't compile !!!
  // println(loSquared6)

  println("\n----- Creating a List[Int => Int]")

  val lf1_1 = List((x: Int) => x * 1, (x: Int) => x * 2, (x: Int) => x * 3)
  val lf1_2 = List((_: Int) * 1, (_: Int) * 2, (_: Int) * 3)

  println("\n----- Creating a List[Int => Int] from List[Int]")
  val lf1_3 = List(1, 2, 3).map(x => (y: Int) => y * x)
  val lf1_4 = List(1, 2, 3).map(x => (_: Int) * x)
  val lf1   = lf1_4
  println(lf1)

  println("\n----- Creating a List[Option[Int => Int]]")

  val lof1 = lf1 map Option.apply
  println(lof1)

  println("\n----- Applying a List[Int => Int] to a List[Int]] with Applicative")

  val liResult = Applicative[List].ap(lf1)(li)
  println(liResult)

  println("\n----- Applying a List[Option[Int => Int]] to a List[Option[Int]] with Applicative")

  val loiResult = (Applicative[List] compose Applicative[Option]).ap(lof1)(loi)
  println(loiResult)

  println("\n----- Functors compose. Applicatives compose. But Monads do NOT compose!")

  val ca = Applicative[List] compose Applicative[Option]
  println(ca)
  val cm = Monad[List] compose Monad[Option]
  println(cm)

  println("\n----- Creating an OptionT[List, Int]")

  val otli = OptionT[List, Int](loi)
  println(otli)
  println(otli.value)

  println("\n----- FlatMapping an OptionT[List, Int]")

  def fillListWith(x: Int): List[Option[String]] = List.fill(x)(Option(x.toString))

  val otliFlatMapped = otli.flatMap(x => OptionT[List, String](fillListWith(x)))

  println(otliFlatMapped)
  println(otliFlatMapped.value)

  println("\n----- Convenience function flatMapF")

  val otliFlatMappedF = otli.flatMapF(x => fillListWith(x))
  println(otliFlatMappedF)
  println(otliFlatMappedF.value)

  println("\n----- Mapping an OptionT[List, Int]")

  val otliMapped = Monad[OptionT[List, *]].map(otli) { _.toString + "!" }
  println(otliMapped)

  println("\n----- OptionT.{isDefined isEmpty getOrElse}")

  val isDefined = otli.isDefined
  println(isDefined)

  val isEmpty = otli.isEmpty
  println(isEmpty)

  val got = otli.getOrElse(42)
  println(got)

  val dbl = otli.fold(42.0)(_.toDouble)
  println(dbl)

  println("\n----- For comprehension with OptionT[List, Int] encapsulating List[Option[Int]]")

  val result4: OptionT[List, Int] = for {
    x <- otli
    y <- otli
  } yield x * y

  println(result4)
  println(result4.value)

  println("\n----- Generic Processing of Monads")

  def processIntMonads[F[_]: Monad](monad1: F[Int], monad2: F[Int]): F[Int] =
    for {
      x <- monad1
      y <- monad2
    } yield x * y

  val result5 = processIntMonads(otli, otli)
  println(result5.value)

  println("\n----- Using monads List, Vector, Option, Future")

  println(processIntMonads(List(1, 2, 3), List(10, 20, 30)))
  println(processIntMonads(Vector(1, 2, 3), Vector(10, 20, 30)))
  println(processIntMonads(Option(5), Option(5)))

  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val fi = processIntMonads(Future(5), Future(5))
  Await.ready(fi, 1.second)
  println(fi)

  println("\n----- Using OptionT[Vector, Int], OptionT[Option, Int], OptionT[Future, Int]")

  val otvi = OptionT[Vector, Int](Vector(Option(3), Option(5)))
  println(processIntMonads(otvi, otvi).value)

  val otoi = OptionT[Option, Int](Option(Option(5)))
  println(processIntMonads(otoi, otoi).value)

  val otfi = processIntMonads(OptionT(Future(Option(5))), OptionT(Future(Option(5))))
  Await.ready(otfi.value, 1.second)
  println(otfi.value)

  println("\n----- Using the Id monad and OptionT[Id, Int]")

  val ii = processIntMonads(5: Id[Int], 5: Id[Int])
  println(ii)

  val otii = processIntMonads(OptionT[Id, Int](Option(5)), OptionT[Id, Int](Option(5)))
  println(otii.value)

  println("-----\n")
}
