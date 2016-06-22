import sbt._
import Import._
import Keys._
import classpath.ClasspathUtilities

object TestProject extends Build {
  lazy val root =
    Project("root", file(".")) settings
      (ivyPaths <<= (baseDirectory, target)((dir, t) =>
                new IvyPaths(dir, Some(t / "ivy-cache"))),
          libraryDependencies +=
            "slinky" % "slinky" % "2.1" % "test" from "http://slinky2.googlecode.com/svn/artifacts/2.1/slinky.jar",
          TaskKey[Unit]("check-in-test") <<= checkClasspath(Test),
          TaskKey[Unit]("check-in-compile") <<= checkClasspath(Compile))

  private def checkClasspath(conf: Configuration) =
    fullClasspath in conf map { cp =>
      try {
        val loader = ClasspathUtilities.toLoader(cp.files)
        Class.forName("slinky.http.Application", false, loader)
        ()
      } catch {
        case _: ClassNotFoundException =>
          sys.error("Dependency not downloaded.")
      }
    }
}
