import sbt._
import Keys._
import Defaults._
import Import._

object B extends Build {
  lazy val root = Project(
      "root",
      file("."),
      settings =
        defaultSettings ++ Seq(
            libraryDependencies +=
              "org.scalatest" %% "scalatest" % "1.9.1" % "test",
            parallelExecution in test := false
        ))
}
