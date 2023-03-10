package app

import cats.data.OptionT, cats.instances.future._ // cats imports
// import transform.OptionT, mycats.Applicative.futureApplicative // my imports

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MakingTheTypesFit extends App {

  println("\n--- Making the types fit")

  type Nickname = String
  type Name     = String
  type Age      = Int

  final case class User(name: Name, age: Age, nickname: Nickname)

  val users = Map[Nickname, User]("mickey" -> User("Mickey Mouse", 25, "mickey"))

  def getUser(nickname: Nickname): Future[Option[User]] =
    Future(users.get(nickname))
  def getAge(user: User): Future[Age] =
    Future(user.age)
  def getName(user: User): Option[Name] =
    Option(user.nickname)

  {
    println("\n--- Adapting for OptionT - the classic way")

    def getNameAge(nickname: Nickname): Future[Option[(Name, Age)]] =
      (for {
        user <- OptionT(getUser(nickname))
        age  <- OptionT(getAge(user).map(Option(_)))
        name <- OptionT(Future(getName(user)))
      } yield (name, age)).value

    val userOrError = Await
      .result(
        getNameAge("mickey"),
        3.seconds
      )
      .getOrElse("User not found")

    println(userOrError)
  }

  {
    println("\n--- Using OptionT.liftF and OptionT.fromOption")

    def getNameAgeWithTypes(nickname: Nickname): Future[Option[(Name, Age)]] =
      (for {
        user: User <- OptionT[Future, User](getUser(nickname))
        age: Int   <- OptionT.liftF[Future, Int](getAge(user))
        name: Name <- OptionT.fromOption[Future](getName(user))
      } yield (name, age)).value

    def getNameAge(nickname: Nickname): Future[Option[(Name, Age)]] =
      (for {
        user <- OptionT(getUser(nickname))
        age  <- OptionT.liftF(getAge(user))
        name <- OptionT.fromOption(getName(user))
      } yield (name, age)).value

    val userOrError = Await
      .result(
        getNameAge("mickey"),
        3.seconds
      )
      .getOrElse("User not found")

    println(userOrError)
  }

  {
    println("\n--- Compare the solution without OptionT")

    def getNameAge(nickname: Nickname): Future[Option[(Name, Age)]] =
      for {
        maybeUser <- getUser(nickname)
        if maybeUser.isDefined
        user      = maybeUser.get
        maybeAge  <- getAge(user).map(Option(_))
        maybeName <- Future(getName(user))
      } yield for {
        name <- maybeName
        age  <- maybeAge
      } yield (name, age)

    val userOrError = Await
      .result(
        getNameAge("mickey"),
        3.seconds
      )
      .getOrElse("User not found")

    println(userOrError)
  }

  println("\n-----\n")
}
