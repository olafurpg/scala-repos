package sbttest // you need package http://stackoverflow.com/questions/9822008/

import sbt._
import Keys._
import Import._

object C extends AutoPlugin
  object autoImport
    object bN extends AutoPlugin
      override def trigger = allRequirements
    lazy val check = taskKey[Unit](
        "Checks that the AutoPlugin and Build are automatically added.")

import C.autoImport._

object A extends AutoPlugin
  override def requires = bN
  override def trigger = allRequirements
  override def projectSettings = Seq(
      check := {}
  )

object B extends Build
  lazy val extra = project.enablePlugins(bN)
