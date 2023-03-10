name := "monad-transformers"
version := "0.2.0"

scalaVersion := "2.13.10"

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",        // source files are in UTF-8
  "-deprecation", // warn about use of deprecated APIs
  "-unchecked",   // warn about unchecked type parameters
  "-feature"      // warn about misused language features
  // "-Xlint",               // enable handy linter warnings
  // "-Ypartial-unification" // (only < 2.13.0) allow the compiler to unify type constructors of different arities
  // "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  // "-Xfatal-warnings",     // turn compiler warnings into errors
)

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-effect" % "3.4.8",
  "org.scalatest"  %% "scalatest"   % "3.2.15" % Test,
  "org.scalacheck" %% "scalacheck"  % "1.17.0" % Test,
  compilerPlugin("org.typelevel" % "kind-projector"      % "0.13.2" cross CrossVersion.full),
  compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
)
