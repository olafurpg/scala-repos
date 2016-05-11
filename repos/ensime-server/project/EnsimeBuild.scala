import SonatypeSupport._
import com.typesafe.sbt.SbtScalariform._
import java.io._
import org.ensime.EnsimePlugin.JdkDir
import org.ensime.Imports.EnsimeKeys
import sbt.{ IntegrationTest => It, _ }
import sbt.Keys._
import sbtassembly.{ AssemblyKeys, MergeStrategy, PathList }
import sbtassembly.AssemblyKeys._
import scala.util.{ Properties, Try }
import org.ensime.EnsimePlugin.JdkDir
import sbtbuildinfo.BuildInfoPlugin, BuildInfoPlugin.autoImport._

object EnsimeBuild extends Build {
  lazy override val settings = super.settings ++ Seq(
    scalaVersion := "2.11.8",
    organization := "org.ensime",
    version := "0.9.10-SNAPSHOT"
  )

  lazy val commonSettings = Sensible.settings ++ Seq(
    libraryDependencies ++= Sensible.testLibs() ++ Sensible.logback,

    dependencyOverrides ++= Set(
       "com.typesafe.akka" %% "akka-actor" % Sensible.akkaVersion,
       "com.typesafe.akka" %% "akka-testkit" % Sensible.akkaVersion,
       "io.spray" %% "spray-json" % "1.3.2"
    ),

    // WORKAROUND https://github.com/ensime/ensime-emacs/issues/327
    fullResolvers += Resolver.jcenterRepo,

    resolvers += Resolver.sonatypeRepo("snapshots"),

    // disabling shared memory gives a small performance boost to tests
    javaOptions ++= Seq("-XX:+PerfDisableSharedMem"),

    javaOptions in Test ++= Seq(
      "-Dlogback.configurationFile=../logback-test.xml"
    ),

    dependencyOverrides ++= Set(
      "org.apache.lucene" % "lucene-core" % luceneVersion
    ),

    EnsimeKeys.scalariform := ScalariformKeys.preferences.value

  // https://github.com/sbt/sbt/issues/2459 --- misses shapeless in core/it:test
  // updateOptions := updateOptions.value.withCachedResolution(true)
  ) ++ sonatype("ensime", "ensime-server", GPL3)

  lazy val commonItSettings = inConfig(It)(
    Defaults.testSettings ++ Sensible.testSettings
  ) ++ scalariformSettingsWithIt ++ Seq(
      javaOptions in It ++= Seq(
        "-Dlogback.configurationFile=../logback-it.xml"
      )
    )

  lazy val JavaTools: File = JdkDir / "lib/tools.jar"

  ////////////////////////////////////////////////
  // modules
  lazy val monkeys = Project("monkeys", file("monkeys")) settings (commonSettings) settings (
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.apache.commons" % "commons-vfs2" % "2.0" exclude ("commons-logging", "commons-logging")
    )
  )

  lazy val util = Project("util", file("util")) settings (commonSettings) settings (
    libraryDependencies ++= List(
      "org.apache.commons" % "commons-vfs2" % "2.0" exclude ("commons-logging", "commons-logging")
    ) ++ Sensible.guava
  )

  lazy val testutil = Project("testutil", file("testutil")) settings (commonSettings) dependsOn (
    util, api
  ) settings (
      libraryDependencies += "commons-io" % "commons-io" % "2.4",
      libraryDependencies ++= Sensible.testLibs("compile")
    )

  lazy val s_express = Project("s-express", file("s-express")) settings (commonSettings) dependsOn (
    util,
    testutil % "test"
  ) settings (
      libraryDependencies ++= Seq(
        "org.parboiled" %% "parboiled" % "2.1.2"
      ) ++ Sensible.shapeless(scalaVersion.value)
    )

  lazy val api = Project("api", file("api")) settings (commonSettings) settings (
    libraryDependencies ++= Seq(
      "org.scalariform" %% "scalariform" % "0.1.8"
    ),
      licenses := Seq(Apache2)
  )

  // the JSON protocol
  lazy val jerky = Project("jerky", file("protocol-jerky")) settings (commonSettings) dependsOn (
    util,
    api,
    testutil % "test",
    api % "test->test" // for the test data
  ) settings (
      libraryDependencies ++= Seq(
        "com.github.fommil" %% "spray-json-shapeless" % "1.2.0",
        "com.typesafe.akka" %% "akka-slf4j" % Sensible.akkaVersion
      ) ++ Sensible.shapeless(scalaVersion.value)
    )

  // the S-Exp protocol
  lazy val swanky = Project("swanky", file("protocol-swanky")) settings (commonSettings) dependsOn (
    api,
    testutil % "test",
    api % "test->test", // for the test data
    s_express
  ) settings (
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-slf4j" % Sensible.akkaVersion
      ) ++ Sensible.shapeless(scalaVersion.value)
    )

  lazy val core = Project("core", file("core")).dependsOn(
    api, s_express, monkeys,
    api % "test->test", // for the interpolator
    testutil % "test,it",
    // depend on "it" dependencies in "test" or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    testingEmpty % "test,it",
    testingSimple % "test,it",
    // test config needed to get the test jar
    testingSimpleJar % "test,it->test",
    testingTiming % "test,it",
    testingDebug % "test,it",
    testingJava % "test,it"
  ).configs(It).settings(
      commonSettings, commonItSettings
    ).settings(
      unmanagedJars in Compile += JavaTools,
      EnsimeKeys.unmanagedSourceArchives += file(".").getCanonicalFile / "openjdk-langtools/openjdk6-langtools-src.zip",
      libraryDependencies ++= Seq(
        "com.h2database" % "h2" % "1.4.190",
        "com.typesafe.slick" %% "slick" % "3.1.1",
        "com.zaxxer" % "HikariCP-java6" % "2.3.13",
        // Netbeans 7.4+ needs Java 7 (7.3 only needs it at runtime)
        "org.netbeans.api" % "org-netbeans-api-java" % "RELEASE731",
        "org.netbeans.api" % "org-netbeans-modules-java-source" % "RELEASE731",
        // lucene 4.8+ needs Java 7: http://www.gossamer-threads.com/lists/lucene/general/225300
        "org.apache.lucene" % "lucene-core" % luceneVersion,
        "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion,
        "org.ow2.asm" % "asm-commons" % "5.0.4",
        "org.ow2.asm" % "asm-util" % "5.0.4",
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scala-lang" % "scalap" % scalaVersion.value,
        "com.typesafe.akka" %% "akka-actor" % Sensible.akkaVersion,
        "com.typesafe.akka" %% "akka-slf4j" % Sensible.akkaVersion,
        "org.scala-refactoring" %% "org.scala-refactoring.library" % "0.9.1-SNAPSHOT",
        "commons-lang" % "commons-lang" % "2.6",
        "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
      ) ++ Sensible.testLibs("it,test") ++ Sensible.shapeless(scalaVersion.value)
    ) enablePlugins BuildInfoPlugin settings (
        buildInfoPackage := organization.value,
        buildInfoKeys += BuildInfoKey.action("gitSha")(Try("git rev-parse --verify HEAD".!! dropRight 1) getOrElse "n/a"),
        buildInfoOptions += BuildInfoOption.BuildTime
      )

  val luceneVersion = "4.7.2"
  val streamsVersion = "1.0"
  lazy val server = Project("server", file("server")).dependsOn(
    core, swanky, jerky,
    s_express % "test->test",
    swanky % "test->test",
    // depend on "it" dependencies in "test" or sbt adds them to the release deps!
    // https://github.com/sbt/sbt/issues/1888
    core % "test->test",
    core % "it->it",
    testingDocs % "test,it"
  ).configs(It).settings(
      commonSettings ++ commonItSettings
    ).settings(
        libraryDependencies ++= Seq(
          "com.typesafe.akka" %% "akka-stream-experimental" % streamsVersion,
          "com.typesafe.akka" %% "akka-http-core-experimental" % streamsVersion,
          "com.typesafe.akka" %% "akka-http-experimental" % streamsVersion,
          "com.typesafe.akka" %% "akka-http-spray-json-experimental" % streamsVersion,
          "com.typesafe.akka" %% "akka-http-xml-experimental" % streamsVersion,
          "com.typesafe.akka" %% "akka-http-testkit-experimental" % streamsVersion % "test,it"
        ) ++ Sensible.testLibs("it,test") ++ Sensible.shapeless(scalaVersion.value)
      )

  // testing modules
  lazy val testingEmpty = Project("testingEmpty", file("testing/empty"))

  lazy val testingSimple = Project("testingSimple", file("testing/simple")) settings (
    scalacOptions in Compile := Seq(),
    libraryDependencies += "org.scalatest" %% "scalatest" % Sensible.scalatestVersion % "test" intransitive ()
  )

  lazy val testingSimpleJar = Project("testingSimpleJar", file("testing/simpleJar")).settings(
    exportJars := true,
    EnsimeKeys.useTarget in Compile := Some((artifactPath in (Compile, packageBin)).value),
    EnsimeKeys.useTarget in Test := Some((artifactPath in (Test, packageBin)).value)
  )

  lazy val testingImplicits = Project("testingImplicits", file("testing/implicits")) settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % Sensible.scalatestVersion % "test" intransitive ()
  )

  lazy val testingTiming = Project("testingTiming", file("testing/timing"))

  lazy val testingDebug = Project("testingDebug", file("testing/debug")).settings(
    scalacOptions in Compile := Seq()
  )

  lazy val testingDocs = Project("testingDocs", file("testing/docs")).settings(
    libraryDependencies ++= Seq(
      // specifically using ForecastIOLib version 1.5.1 for javadoc 1.8 output
      "com.github.dvdme" % "ForecastIOLib" % "1.5.1" intransitive (),
      "com.google.guava" % "guava" % Sensible.guavaVersion intransitive (),
      "commons-io" % "commons-io" % "2.4" intransitive ()
    )
  )

  // java project with no scala-library
  lazy val testingJava = Project("testingJava", file("testing/java")).settings(
    crossPaths := false,
    autoScalaLibrary := false
  )

  // manual root project so we can exclude the testing projects from publication
  lazy val root = Project(id = "ensime", base = file(".")) settings (commonSettings) aggregate (
    api, monkeys, util, testutil, s_express, jerky, swanky, core, server
  ) dependsOn (server) settings (
      // e.g. `sbt ++2.11.8 ensime/assembly`
      test in assembly := {},
      aggregate in assembly := false,
      assemblyMergeStrategy in assembly := {
        case PathList("META-INF", "namedservices", xs @ _*) => MergeStrategy.filterDistinctLines
        case "META-INF/netbeans/translate.names" => MergeStrategy.filterDistinctLines
        case "META-INF/namedservices.index" => MergeStrategy.filterDistinctLines
        case "META-INF/generated-layer.xml" => MergeStrategy.rename
        case PathList("org", "apache", "commons", "vfs2", xs @ _*) => MergeStrategy.first // assumes our classpath is setup correctly
        case other => MergeStrategy.defaultMergeStrategy(other)
      },
      assemblyExcludedJars in assembly <<= (fullClasspath in assembly).map { everything =>
        everything.filter { attr =>
          val n = attr.data.getName
          n.startsWith("scala-library") | n.startsWith("scala-compiler") |
            n.startsWith("scala-reflect") | n.startsWith("scalap")
        } :+ Attributed.blank(JavaTools)
      },
      assemblyJarName in assembly := s"ensime_${scalaBinaryVersion.value}-${version.value}-assembly.jar"
    )
}
