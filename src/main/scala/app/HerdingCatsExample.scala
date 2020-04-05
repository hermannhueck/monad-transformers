/*
  Code taken from "Herding Cats":
  http://eed3si9n.com/herding-cats/stacking-future-and-either.html

  and slightly modified.

  Original code stems from:
  https://gist.github.com/xuwei-k/051c3b00129b7a0dfcd6
 */

package app

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.language.postfixOps

case class User(id: Long, name: String)

// In actual code, probably more than 2 errors
sealed trait Error
object Error {
  final case class UserNotFound(userId: Long)       extends Error
  final case class ConnectionError(message: String) extends Error
}

object HerdingCatsExample extends App {

  println("\n----- Herding Cats Example from: http://eed3si9n.com/herding-cats/stacking-future-and-either.html")

  {
    object UserRepo {
      def followers(userId: Long): Either[Error, List[User]] =
        userId match {
          case 0L =>
            Right(List(User(1, "Michael")))
          case 1L =>
            Right(List(User(0, "Vito")))
          case x =>
            Left(Error.UserNotFound(x))
        }
    }
    import UserRepo.followers

    def isFriends0(user1: Long, user2: Long): Either[Error, Boolean] =
      for {
        a <- followers(user1) // .right
        b <- followers(user2) // .right
      } yield a.exists(_.id == user2) && b.exists(_.id == user1)

    println("\n ----- isFriends0")

    val result1 = isFriends0(0, 1)
    println(result1)

    val result2 = isFriends0(0, 2)
    println(result2)
  }

  {
    object UserRepo {
      def followers(userId: Long): Future[Either[Error, List[User]]] =
        userId match {
          case 0L =>
            Future { Right(List(User(1, "Michael"))) }
          case 1L =>
            Future { Right(List(User(0, "Vito"))) }
          case x =>
            Future.successful { Left(Error.UserNotFound(x)) }
        }
    }
    import UserRepo.followers

    def isFriends1(user1: Long, user2: Long): Future[Either[Error, Boolean]] =
      for {
        a <- followers(user1)
        b <- followers(user2)
      } yield for {
        x <- a // .right
        y <- b // .right
      } yield x.exists(_.id == user2) && y.exists(_.id == user1)

    def isFriends2(user1: Long, user2: Long): Future[Either[Error, Boolean]] =
      followers(user1) flatMap {
        case Right(a) =>
          followers(user2) map {
            case Right(b) =>
              Right(a.exists(_.id == user2) && b.exists(_.id == user1))
            case Left(e) =>
              Left(e)
          }
        case Left(e) =>
          Future.successful(Left(e))
      }

    println("\n--- isFriends1")

    val result1_1 = Await.result(isFriends2(0, 1), 1 second)
    println(result1_1)

    val result1_2 = Await.result(isFriends2(0, 2), 1 second)
    println(result1_2)

    println("\n--- isFriends2")

    val result2_1 = Await.result(isFriends2(0, 1), 1 second)
    println(result2_1)

    val result2_2 = Await.result(isFriends2(0, 2), 1 second)
    println(result2_2)
  }

  {
    import cats._, cats.data._, cats.implicits._
    object UserRepo {
      def followers(userId: Long): EitherT[Future, Error, List[User]] =
        userId match {
          case 0L =>
            EitherT.right(Future { List(User(1, "Michael")) })
          case 1L =>
            EitherT.right(Future { List(User(0, "Vito")) })
          case x =>
            EitherT.left(Future.successful { Error.UserNotFound(x) })
        }
    }
    import UserRepo.followers

    def isFriends3(user1: Long, user2: Long): Future[Either[Error, Boolean]] =
      (for {
        a <- followers(user1)
        b <- followers(user2)
      } yield a.exists(_.id == user2) && b.exists(_.id == user1)).value

    println("\n--- isFriends3")

    val result1 = Await.result(isFriends3(0, 1), 1 second)
    println(result1)

    val result2 = Await.result(isFriends3(0, 2), 1 second)
    println(result2)
  }

  println("-----\n")
}
