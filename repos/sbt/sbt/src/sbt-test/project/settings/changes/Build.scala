import sbt._
import Keys._
import Import._

object B extends Build
  lazy val root = Project("root", file(".")) settings (maxErrors ~= (_ * 9))
