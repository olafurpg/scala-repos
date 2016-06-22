import sbt._
import Import._
import complete.DefaultParsers._
import Keys._

object B extends Build {
  val check = InputKey[Unit]("check-max-errors")

  lazy val root = Project("root", file("."))
  lazy val sub =
    Project("sub", file("sub")) delegateTo (root) settings
      (check <<= checkTask)

  lazy val checkTask = InputTask(_ => Space ~> NatBasic) { result =>
    (result, maxErrors) map { (i, max) =>
      if (i != max)
        sys.error("Expected max-errors to be " + i + ", but it was " + max)
    }
  }
}
