package app

import scala.language.higherKinds

import cats.data.{EitherT, OptionT}
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object StackingTransformers1b extends App {

  // with intermediate types

  def compute__[A, B](f: A => B): A => Future[Either[String, Option[B]]] = { input =>
    val option: Option[A]                         = Some(input)
    val either: Either[String, Option[A]]         = Right(option)
    val future: Future[Either[String, Option[A]]] = Future(either)
    future.map(_.map(_.map(f)))
  }

  def futureEitherOption__[A, B](f: A => B): A => OptionT[EitherT[Future, String, *], B] = { input =>
    val future: Future[Either[String, Option[A]]]       = compute__(identity[A])(input)
    val eitherT: EitherT[Future, String, Option[A]]     = EitherT(future)
    val optionT: OptionT[EitherT[Future, String, *], A] = OptionT(eitherT)
    optionT.map(f)
  }

  // shorter and without intermediate types

  def compute[A, B](f: A => B): A => Future[Either[String, Option[B]]] =
    input => Future(Right(Some(input))).map(_.map(_.map(f)))

  def stackMonads[A, B](f: A => B): A => OptionT[EitherT[Future, String, *], B] =
    input => OptionT(EitherT(compute(identity[A])(input))).map(f)

  val stackedResult: OptionT[EitherT[Future, String, *], Int] =
    for {
      a <- stackMonads((_: Int) * 2)(5)
      b <- stackMonads((_: Int) * 2)(16)
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

  println(result)
  result foreach { opt => opt foreach println }

  final case class User(name: String, age: Int)

  val joe     = User("Joe", 42)
  val averell = User("Averell", 35)

  val userDb =
    List(joe, averell)
      .map(user => user.name -> user)
      .toMap

  def getUser(name: String): Option[User] =
    userDb.get(name)

  def getUserAsync(name: String): OptionT[EitherT[Future, Throwable, *], User] =
    OptionT(EitherT[Future, Throwable, Option[User]](Future(Right(getUser(name)))))

  def getUserAsync2(name: String): OptionT[EitherT[Future, Throwable, *], User] = {
    val future: Future[Either[Throwable, Option[User]]]       = Future(Right(getUser(name)))
    val eitherT: EitherT[Future, Throwable, Option[User]]     = EitherT(future)
    val optionT: OptionT[EitherT[Future, Throwable, *], User] = OptionT(eitherT)
    optionT
  }

  val ages = for {
    joe     <- getUserAsync("Joe")
    averell <- getUserAsync("Averell")
  } yield joe.age + averell.age

  println(ages)             // OptionT(EitherT(Future(Success(Right(Some(77))))))
  println(ages.value.value) // Future(Success(Right(Some(77))))

  val result2 = Await.result(ages.value.value, 1.second)
  result2.foreach(_.foreach(println)) // 77
}
