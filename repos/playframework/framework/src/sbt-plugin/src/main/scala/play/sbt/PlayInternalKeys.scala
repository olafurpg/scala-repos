/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.sbt

import sbt._
import sbt.Keys._

object PlayInternalKeys {
  type ClassLoaderCreator = play.runsupport.Reloader.ClassLoaderCreator

  val playDependencyClasspath = TaskKey[Classpath](
      "playDependencyClasspath",
      "The classpath containing all the jar dependencies of the project")
  val playReloaderClasspath = TaskKey[Classpath](
      "playReloaderClasspath",
      "The application classpath, containing all projects in this build that are dependencies of this project, including this project")
  val playCommonClassloader = TaskKey[ClassLoader](
      "playCommonClassloader",
      "The common classloader, is used to hold H2 to ensure in memory databases don't get lost between invocations of run")
  val playDependencyClassLoader = TaskKey[ClassLoaderCreator](
      "playDependencyClassloader",
      "A function to create the dependency classloader from a name, set of URLs and parent classloader")
  val playReloaderClassLoader = TaskKey[ClassLoaderCreator](
      "playReloaderClassloader",
      "A function to create the application classloader from a name, set of URLs and parent classloader")
  val playReload = TaskKey[sbt.inc.Analysis](
      "playReload",
      "Executed when sources of changed, to recompile (and possibly reload) the app")
  val playCompileEverything = TaskKey[Seq[sbt.inc.Analysis]](
      "playCompileEverything",
      "Compiles this project and every project it depends on.")
  val playAssetsWithCompilation = TaskKey[sbt.inc.Analysis](
      "playAssetsWithCompilation",
      "The task that's run on a particular project to compile it. By default, builds assets and runs compile.")

  val playStop = TaskKey[Unit](
      "playStop",
      "Stop Play, if it has been started in non blocking mode")

  val playAllAssets = TaskKey[Seq[(String, File)]](
      "playAllAssets",
      "Compiles all assets for all projects")
  val playPrefixAndAssets = TaskKey[(String, File)](
      "playPrefixAndAssets",
      "Gets all the assets with their associated prefixes")
  val playAssetsClassLoader = TaskKey[ClassLoader => ClassLoader](
      "playAssetsClassloader",
      "Function that creates a classloader from a given parent that contains all the assets.")
}
