import sbt._
import Import._
import complete.DefaultParsers._
import Keys._
import AddSettings._

object B extends Build
  // This configuration is added to 'sub' only.
  //  This verifies that delegation works when a configuration is not defined in the project that is being delegated to
  val newConfig = config("sample")

  val sample = SettingKey[Int]("sample")
  val check = TaskKey[Unit]("check")

  lazy val root = Project("root", file("."), settings = Nil).settingSets()
  lazy val sub = Project(
      "sub",
      file("."),
      delegates = root :: Nil,
      configurations = newConfig :: Nil,
      settings = incSample :: checkTask(4) :: Nil).settingSets(buildScalaFiles)
  override lazy val settings =
    (sample in newConfig := 3) :: checkTask(3) :: Nil

  def incSample = sample <<= sample in newConfig apply (_ + 1)
  def checkTask(expected: Int) =
    check <<= sample in newConfig map
    (i => assert(i == expected, "Expected " + expected + ", got " + i))
