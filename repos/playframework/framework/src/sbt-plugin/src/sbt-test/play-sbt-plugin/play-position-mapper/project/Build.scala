/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */

import play.sbt.PlayScala
import sbt._
import Keys._

object ApplicationBuild extends Build {

  val appName = "play-position-mapper"
  val appVersion = "1.0-SNAPSHOT"

  val bufferLogger = new AbstractLogger {
    @volatile var messages = List.empty[String]
    def getLevel = Level.Error
    def setLevel(newLevel: Level.Value) = ()
    def setTrace(flag: Int) = ()
    def getTrace = 0
    def successEnabled = false
    def setSuccessEnabled(flag: Boolean) = ()
    def control(event: ControlEvent.Value, message: => String) = ()
    def logAll(events: Seq[LogEvent]) = events.foreach(log)
    def trace(t: => Throwable) = ()
    def success(message: => String) = ()
    def log(level: Level.Value, message: => String) = {
      if (level == Level.Error)
        synchronized {
          messages = message :: messages
        }
    }
  }

  import complete.DefaultParsers._

  def simpleParser(state: State) = Space ~> any.+.map(_.mkString(""))

  def checkLogContains(msg: String): Task[Boolean] = task {
    if (!bufferLogger.messages.exists(_.contains(msg))) {
      sys.error(
          "Did not find log message:\n    '" + msg + "'\nin output:\n" +
            bufferLogger.messages.reverse.mkString("    ", "\n    ", ""))
    }
    true
  }

  val checkLogContainsTask =
    InputKey[Boolean]("checkLogContains") <<=
      InputTask.separate[String, Boolean](simpleParser _)(state(s =>
        checkLogContains))

  val compileIgnoreErrorsTask =
    TaskKey[Unit]("compileIgnoreErrors") <<= state.map { state =>
      Project.runTask(compile in Compile, state)
    }

  val main = Project(appName, file("."))
    .enablePlugins(PlayScala)
    .settings(
        version := appVersion,
        extraLoggers ~= { currentFunction => (key: ScopedKey[_]) =>
          {
            bufferLogger +: currentFunction(key)
          }
        },
        scalaVersion := sys.props.get("scala.version").getOrElse("2.11.7"),
        checkLogContainsTask,
        compileIgnoreErrorsTask
    )
}
