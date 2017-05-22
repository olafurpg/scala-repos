import sbt._
import Import._
import Keys._

object MultiPublishTest extends Build
  override lazy val settings =
    super.settings ++ Seq(
        organization := "A",
        version := "1.0",
        ivyPaths <<=
          baseDirectory(dir => new IvyPaths(dir, Some(dir / "ivy" / "cache"))),
        externalResolvers <<= baseDirectory map  base =>
          Resolver.file("local", base / "ivy" / "local" asFile)(
              Resolver.ivyStylePatterns) :: Nil
    )

  lazy val root =
    Project("root", file(".")) settings
    (name := "Retrieve Test", mavenStyle, libraryDependencies <<=
          publishMavenStyle  style =>
          if (style) mavenStyleDependencies else ivyStyleDependencies
        )

  lazy val mavenStyle =
    publishMavenStyle <<= baseDirectory  base =>
      (base / "mavenStyle") exists

  def ivyStyleDependencies =
    parentDep("A") :: subDep("A") :: subDep("B") :: parentDep("D") :: Nil
  def mavenStyleDependencies =
    parentDep("B") :: parentDep("C") :: subDep("C") :: subDep("D") :: Nil

  def parentDep(org: String) = org %% "publish-test" % "1.0"
  def subDep(org: String) = org %% "sub-project" % "1.0"
