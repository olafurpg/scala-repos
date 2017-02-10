import sbt._
import Keys._

import scala.annotation.tailrec

import bintray.Plugin.bintrayPublishSettings
import bintray.Keys.{repository, bintrayOrganization, bintray}

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.{previousArtifact, binaryIssueFilters}

import java.io.{BufferedOutputStream, FileOutputStream, BufferedWriter, FileWriter}

import scala.collection.mutable
import scala.util.Properties

import org.scalajs.core.ir
import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.sbtplugin._
import org.scalajs.jsenv.{JSEnv, RetryingComJSEnv}
import org.scalajs.jsenv.rhino.RhinoJSEnv
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.phantomjs.PhantomJSEnv
import ScalaJSPlugin.autoImport._
import ExternalCompile.scalaJSExternalCompileSettings
import Implicits._

import org.scalajs.core.tools.io.MemVirtualJSFile
import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.core.tools.linker.backend.OutputMode

import sbtassembly.AssemblyPlugin.autoImport._

object Build extends sbt.Build {

  val isGeneratingEclipse =
    Properties.envOrElse("GENERATING_ECLIPSE", "false").toBoolean

  val fetchScalaSource =
    taskKey[File]("Fetches the scala source for the current scala version")
  val shouldPartest = settingKey[Boolean](
      "Whether we should partest the current scala version (and fail if we can't)")

  val previousVersion = "0.6.8"
  val previousSJSBinaryVersion =
    ScalaJSCrossVersion.binaryScalaJSVersion(previousVersion)
  val previousBinaryCrossVersion =
    CrossVersion.binaryMapped(v => s"sjs${previousSJSBinaryVersion}_$v")

  val scalaVersionsUsedForPublishing: Set[String] = Set(
      "2.10.6", "2.11.8", "2.12.0-M3")
  val newScalaBinaryVersionsInThisRelease: Set[String] = Set()

  val javaVersion = settingKey[Int](
      "The major Java SDK version that should be assumed for compatibility. " +
      "Defaults to what sbt is running with.")

  val javaDocBaseURL: String = "http://docs.oracle.com/javase/8/docs/api/"

  // set scalaJSSemantics in someProject ~= makeCompliant
  val makeCompliant: Semantics => Semantics = { semantics =>
    semantics
      .withAsInstanceOfs(CheckedBehavior.Compliant)
      .withModuleInit(CheckedBehavior.Compliant)
      .withStrictFloats(true)
  }

  // set postLinkJSEnv in someProject := NodeJSEnv(args = ES6NodeArgs).value
  val ES6NodeArgs = Seq("--harmony-rest-parameters", "--harmony-spreadcalls")

  val previousArtifactSetting: Setting[_] = {
    previousArtifact := {
      val scalaV = scalaVersion.value
      val scalaBinaryV = scalaBinaryVersion.value
      if (!scalaVersionsUsedForPublishing.contains(scalaV)) {
        // This artifact will not be published. Binary compatibility is irrelevant.
        None
      } else if (newScalaBinaryVersionsInThisRelease.contains(scalaBinaryV)) {
        // New in this release, no binary compatibility to comply to
        None
      } else {
        val thisProjectID = projectID.value
        val previousCrossVersion = thisProjectID.crossVersion match {
          case ScalaJSCrossVersion.binary => previousBinaryCrossVersion
          case crossVersion => crossVersion
        }
        /* Filter out e:info.apiURL as it expects 0.6.7-SNAPSHOT, whereas the
         * artifact we're looking for has 0.6.6 (for example).
         */
        val prevExtraAttributes =
          thisProjectID.extraAttributes.filterKeys(_ != "e:info.apiURL")
        val prevProjectID =
          (thisProjectID.organization % thisProjectID.name % previousVersion)
            .cross(previousCrossVersion)
            .extra(prevExtraAttributes.toSeq: _*)
        Some(CrossVersion(scalaV, scalaBinaryV)(prevProjectID)
              .cross(CrossVersion.Disabled))
      }
    }
  }

  val commonSettings =
    Seq(
        scalaVersion := "2.11.8",
        organization := "org.scala-js",
        version := scalaJSVersion,
        normalizedName ~= {
          _.replace("scala.js", "scalajs").replace("scala-js", "scalajs")
        },
        homepage := Some(url("http://scala-js.org/")),
        licenses +=
        ("BSD New",
            url("https://github.com/scala-js/scala-js/blob/master/LICENSE")),
        scmInfo :=
          Some(ScmInfo(url("https://github.com/scala-js/scala-js"),
                       "scm:git:git@github.com:scala-js/scala-js.git",
                       Some("scm:git:git@github.com:scala-js/scala-js.git"))),
        shouldPartest := {
          val testListDir =
            ((resourceDirectory in (partestSuite, Test)).value / "scala" / "tools" / "partest" / "scalajs" / scalaVersion.value)
          testListDir.exists
        },
        scalacOptions ++= Seq(
            "-deprecation",
            "-unchecked",
            "-feature",
            "-encoding",
            "utf8",
        ),
        // Scaladoc linking
        apiURL := {
          val name = normalizedName.value
          Some(url(s"http://www.scala-js.org/api/$name/$scalaJSVersion/"))
        },
        autoAPIMappings := true,
        // Add Java Scaladoc mapping
        apiMappings += {
          val rtJar = {
            System
              .getProperty("sun.boot.class.path")
              .split(java.io.File.pathSeparator)
              .find(_.endsWith(java.io.File.separator + "rt.jar"))
              .get
          }

          file(rtJar) -> url(javaDocBaseURL)
        },
        /* Patch the ScalaDoc we generate.
         *
         *  After executing the normal doc command, copy everything to the
         *  `patched-api` directory (same internal directory structure) while
         *  patching the following:
         *
         *  - Append `additional-doc-styles.css` to `lib/template.css`
         *  - Fix external links to the JavaDoc, i.e. change
         *    `${javaDocBaseURL}index.html#java.lang.String` to
         *    `${javaDocBaseURL}index.html?java/lang/String.html`
         */
        doc in Compile := {
          // Where to store the patched docs
          val outDir = crossTarget.value / "patched-api"

          // Find all files in the current docs
          val docPaths = {
            val docDir = (doc in Compile).value
            Path.selectSubpaths(docDir, new SimpleFileFilter(_.isFile)).toMap
          }

          /* File with our CSS styles (needs to be canonical so that the
           * comparison below works)
           */
          val additionalStylesFile =
            (root.base / "assets/additional-doc-styles.css").getCanonicalFile

          // Regex and replacement function for JavaDoc linking
          val javadocAPIRe =
            s"""\"(\\Q${javaDocBaseURL}index.html\\E)#([^"]*)\"""".r

          val logger = streams.value.log
          val errorsSeen = mutable.Set.empty[String]

          val fixJavaDocLink = {
            (m: scala.util.matching.Regex.Match) =>
              val frag = m.group(2)

              // Fail when encountering links to class members
              if (frag.contains("@") && !errorsSeen.contains(frag)) {
                errorsSeen += frag
                logger.error(s"Cannot fix JavaDoc link to member: $frag")
              }

              m.group(1) + "?" + frag.replace('.', '/') + ".html"
          }

          FileFunction.cached(streams.value.cacheDirectory,
                              FilesInfo.lastModified,
                              FilesInfo.exists) {
            files =>
              for {
                file <- files if file != additionalStylesFile
              } yield {
                val relPath = docPaths(file)
                val outFile = outDir / relPath

                if (relPath == "lib/template.css") {
                  val styles = IO.read(additionalStylesFile)
                  IO.copyFile(file, outFile)
                  IO.append(outFile, styles)
                } else if (relPath.endsWith(".html")) {
                  val content = IO.read(file)
                  val patched =
                    javadocAPIRe.replaceAllIn(content, fixJavaDocLink)
                  IO.write(outFile, patched)
                } else {
                  IO.copyFile(file, outFile)
                }

                outFile
              }
          }(docPaths.keySet + additionalStylesFile)

          if (errorsSeen.size > 0) sys.error("ScalaDoc patching had errors")
          else outDir
        },
    ) ++ mimaDefaultSettings

  val noClassFilesSettings: Setting[_] =
    (scalacOptions in (Compile, compile) ++= {
          if (isGeneratingEclipse) Seq()
          else Seq("-Yskip:cleanup,icode,jvm")
        })

  val publishSettings = Seq(
      publishMavenStyle := true,
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      pomExtra := (<developers>
            <developer>
              <id>sjrd</id>
              <name>Sébastien Doeraene</name>
              <url>https://github.com/sjrd/</url>
            </developer>
            <developer>
              <id>gzm0</id>
              <name>Tobias Schlatter</name>
              <url>https://github.com/gzm0/</url>
            </developer>
            <developer>
              <id>nicolasstucki</id>
              <name>Nicolas Stucki</name>
              <url>https://github.com/nicolasstucki/</url>
            </developer>
          </developers>),
      pomIncludeRepository := { _ =>
        false
      },
  )

  val fatalWarningsSettings = Seq(
      // The pattern matcher used to exceed its analysis budget before 2.11.5
      scalacOptions ++= {
        scalaVersion.value.split('.') match {
          case Array("2", "10", _) => Nil
          case Array("2", "11", x) if x.takeWhile(_.isDigit).toInt <= 4 => Nil
          case _ => Seq("-Xfatal-warnings")
        }
      },
      scalacOptions in (Compile, doc) := {
        val baseOptions = (scalacOptions in (Compile, doc)).value

        /* - need JDK7 to link the doc to java.nio.charset.StandardCharsets
         * - in Scala 2.10, some ScalaDoc links fail
         */
        val fatalInDoc =
          javaVersion.value >= 7 && scalaBinaryVersion.value != "2.10"

        if (fatalInDoc) baseOptions
        else baseOptions.filterNot(_ == "-Xfatal-warnings")
      },
  )

  private def publishToScalaJSRepoSettings = Seq(
      publishTo := {
        Seq("PUBLISH_USER", "PUBLISH_PASS").map(Properties.envOrNone) match {
          case Seq(Some(user), Some(pass)) =>
            val snapshotsOrReleases =
              if (scalaJSIsSnapshotVersion) "snapshots" else "releases"
            Some(
                Resolver.sftp(
                    s"scala-js-$snapshotsOrReleases",
                    "repo.scala-js.org",
                    s"/home/scalajsrepo/www/repo/$snapshotsOrReleases")(
                    Resolver.ivyStylePatterns) as (user, pass))
          case _ =>
            None
        }
      },
  )

  private def publishToBintraySettings = (bintrayPublishSettings) ++ Seq(
      repository in bintray := "scala-js-releases",
      bintrayOrganization in bintray := Some("scala-js"),
  )

  val publishIvySettings =
    (if (Properties.envOrNone("PUBLISH_TO_BINTRAY") == Some("true"))
       publishToBintraySettings
     else publishToScalaJSRepoSettings) ++ Seq(
        publishMavenStyle := false,
    )

  val myScalaJSSettings =
    ScalaJSPluginInternal.scalaJSAbstractSettings ++ Seq(
        autoCompilerPlugins := true,
        scalaJSOptimizerOptions ~= (_.withCheckScalaJSIR(true)),
        testFrameworks +=
          TestFramework("org.scalajs.jasminetest.JasmineFramework"),
        // Link source maps
        scalacOptions ++= {
          if (isGeneratingEclipse) Seq()
          else if (scalaJSIsSnapshotVersion) Seq()
          else
            Seq(
                // Link source maps to github sources
                "-P:scalajs:mapSourceURI:" + root.base.toURI +
                "->https://raw.githubusercontent.com/scala-js/scala-js/v" +
                scalaJSVersion + "/",
            )
        },
    )

  implicit class ProjectOps(val project: Project) extends AnyVal {

    /** Uses the Scala.js compiler plugin. */
    def withScalaJSCompiler: Project =
      if (isGeneratingEclipse) project
      else project.dependsOn(compiler % "plugin")

    /** Depends on library as if (exportJars in library) was set to false. */
    def dependsOnLibraryNoJar: Project = {
      if (isGeneratingEclipse) {
        project.dependsOn(library)
      } else {
        project.settings(
            internalDependencyClasspath in Compile ++= {
              val prods = (products in (library, Compile)).value
              val analysis = (compile in (library, Compile)).value
              prods.map(p => Classpaths.analyzed(p, analysis))
            },
        )
      }
    }

    /** Depends on the sources of another project. */
    def dependsOnSource(dependency: Project): Project = {
      if (isGeneratingEclipse) {
        project.dependsOn(dependency)
      } else {
        project.settings(
            unmanagedSourceDirectories in Compile +=
            (scalaSource in (dependency, Compile)).value,
        )
      }
    }
  }

  override lazy val settings =
    (super.settings ++ inScope(Global)(ScalaJSPlugin.globalSettings)) ++ Seq(
        // Most of the projects cross-compile
        crossScalaVersions := Seq(
            "2.10.2",
            "2.10.3",
            "2.10.4",
            "2.10.5",
            "2.10.6",
            "2.11.0",
            "2.11.1",
            "2.11.2",
            "2.11.4",
            "2.11.5",
            "2.11.6",
            "2.11.7",
            "2.11.8",
            "2.12.0-M3",
        ),
        // JDK version we are running with
        javaVersion in Global := {
          val v = System.getProperty("java.version")
          v.substring(0, 3) match {
            case "1.8" => 8
            case "1.7" => 7
            case "1.6" => 6

            case _ =>
              sLog.value.warn(s"Unknown JDK version $v. Assuming max compat.")
              Int.MaxValue
          }
        },
    )

  lazy val root: Project = Project(
      id = "scalajs",
      base = file("."),
      settings = commonSettings ++ Seq(
            name := "Scala.js",
            publishArtifact in Compile := false,
            clean := clean
              .dependsOn(clean in compiler,
                         clean in irProject,
                         clean in irProjectJS,
                         clean in tools,
                         clean in toolsJS,
                         clean in jsEnvs,
                         clean in testAdapter,
                         clean in plugin,
                         clean in javalanglib,
                         clean in javalib,
                         clean in scalalib,
                         clean in libraryAux,
                         clean in library,
                         clean in javalibEx,
                         clean in stubs,
                         clean in cli,
                         clean in testInterface,
                         clean in jasmineTestFramework,
                         clean in jUnitRuntime,
                         clean in jUnitPlugin,
                         clean in examples,
                         clean in helloworld,
                         clean in reversi,
                         clean in testingExample,
                         clean in testSuite,
                         clean in testSuiteJVM,
                         clean in noIrCheckTest,
                         clean in javalibExTestSuite,
                         clean in partest,
                         clean in partestSuite)
              .value,
            publish := {},
            publishLocal := {},
        ),
  )

  val commonIrProjectSettings =
    (commonSettings ++ publishSettings ++ fatalWarningsSettings) ++ Seq(
        name := "Scala.js IR",
        previousArtifactSetting,
        binaryIssueFilters ++= BinaryIncompatibilities.IR,
        exportJars := true // required so ScalaDoc linking works
    )

  lazy val irProject: Project = Project(
      id = "ir",
      base = file("ir"),
      settings = commonIrProjectSettings,
  )

  lazy val irProjectJS: Project = Project(
      id = "irJS",
      base = file("ir/.js"),
      settings = commonIrProjectSettings ++ myScalaJSSettings ++ Seq(
            crossVersion := ScalaJSCrossVersion.binary,
            unmanagedSourceDirectories in Compile +=
            (scalaSource in Compile in irProject).value,
        ),
  ).withScalaJSCompiler.dependsOn(javalibEx)

  lazy val compiler: Project = Project(
      id = "compiler",
      base = file("compiler"),
      settings = commonSettings ++ publishSettings ++ Seq(
            name := "Scala.js compiler",
            crossVersion := CrossVersion.full, // because compiler api is not binary compatible
            libraryDependencies ++= Seq(
                "org.scala-lang" % "scala-compiler" % scalaVersion.value,
                "org.scala-lang" % "scala-reflect" % scalaVersion.value,
                "com.novocode" % "junit-interface" % "0.9" % "test",
            ),
            testOptions += Tests.Setup { () =>
            val testOutDir =
              (streams.value.cacheDirectory / "scalajs-compiler-test")
            IO.createDirectory(testOutDir)
            sys.props("scala.scalajs.compiler.test.output") = testOutDir.getAbsolutePath
            sys.props("scala.scalajs.compiler.test.scalajslib") =
            (packageBin in (library, Compile)).value.getAbsolutePath
            sys.props("scala.scalajs.compiler.test.scalalib") = {

              def isScalaLib(att: Attributed[File]) = {
                att.metadata.get(moduleID.key).exists { mId =>
                  mId.organization == "org.scala-lang" &&
                  mId.name == "scala-library" &&
                  mId.revision == scalaVersion.value
                }
              }

              val lib = (managedClasspath in Test).value.find(isScalaLib)
              lib.map(_.data.getAbsolutePath).getOrElse {
                streams.value.log.error(
                    "Couldn't find Scala library on the classpath. CP: " +
                    (managedClasspath in Test).value); ""
              }
            }
          },
            exportJars := true,
        ),
  ).dependsOnSource(irProject)

  val commonToolsSettings =
    (commonSettings ++ publishSettings ++ fatalWarningsSettings) ++ Seq(
        name := "Scala.js tools",
        unmanagedSourceDirectories in Compile +=
          baseDirectory.value.getParentFile / "shared/src/main/scala",
        sourceGenerators in Compile <+= Def.task {
          ScalaJSEnvGenerator.generateEnvHolder(
              baseDirectory.value.getParentFile,
              (sourceManaged in Compile).value)
        },
        previousArtifactSetting,
        binaryIssueFilters ++= BinaryIncompatibilities.Tools,
        exportJars := true // required so ScalaDoc linking works
    )

  lazy val tools: Project = Project(
      id = "tools",
      base = file("tools/jvm"),
      settings = commonToolsSettings ++ Seq(
            libraryDependencies ++= Seq(
                "com.google.javascript" % "closure-compiler" % "v20130603",
                "com.googlecode.json-simple" % "json-simple" % "1.1.1" exclude
                ("junit", "junit"),
                "com.novocode" % "junit-interface" % "0.9" % "test",
            ),
        ),
  ).dependsOn(irProject)

  lazy val toolsJS: Project = Project(
      id = "toolsJS",
      base = file("tools/js"),
      settings = myScalaJSSettings ++ commonToolsSettings ++ Seq(
            crossVersion := ScalaJSCrossVersion.binary,
        ) ++ inConfig(Test) {
        // Redefine test to run Node.js and link HelloWorld
        test := {
          val jsEnv = resolvedJSEnv.value
          if (!jsEnv.isInstanceOf[NodeJSEnv])
            error("toolsJS/test must be run with Node.js")

          /* Collect IR relevant files from the classpath
           * We assume here that the classpath is valid. This is checked by the
           * the scalaJSIR task.
           */
          val cp = Attributed.data((fullClasspath in Test).value)

          // Files must be Jars, non-files must be dirs
          val (jars, dirs) = cp.filter(_.exists).partition(_.isFile)
          val irFiles = dirs.flatMap(dir => (dir ** "*.sjsir").get)

          val irPaths = {
            for (f <- jars ++ irFiles) yield
              s""""${escapeJS(f.getAbsolutePath)}""""
          }

          val code = {
            s"""
            var linker = scalajs.QuickLinker();
            var lib = linker.linkTestSuiteNode(${irPaths.mkString(", ")});

            var __ScalaJSEnv = null;

            eval("(function() { 'use strict'; " +
              lib + ";" +
              "scalajs.TestRunner().runTests();" +
            "}).call(this);");
            """
          }

          val launcher =
            new MemVirtualJSFile("Generated launcher file").withContent(code)

          val linked = scalaJSLinkedFile.value
          val libs =
            resolvedJSDependencies.value.data :+ ResolvedJSDependency.minimal(
                linked)
          val runner = jsEnv.jsRunner(libs, launcher)

          runner.run(streams.value.log, scalaJSConsole.value)
        }
      },
  ).withScalaJSCompiler
    .dependsOn(javalibEx, testSuite % "test->test", irProjectJS)

  lazy val jsEnvs: Project = Project(
      id = "jsEnvs",
      base = file("js-envs"),
      settings = (commonSettings ++ publishSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js JS Envs",
            libraryDependencies ++= Seq(
                "io.apigee" % "rhino" % "1.7R5pre4",
                "org.webjars" % "envjs" % "1.2",
                "com.novocode" % "junit-interface" % "0.9" % "test",
            ) ++ ScalaJSPluginInternal.phantomJSJettyModules.map(
                _ % "provided"),
            previousArtifactSetting,
            binaryIssueFilters ++= BinaryIncompatibilities.JSEnvs,
        ),
  ).dependsOn(tools)

  lazy val testAdapter = Project(
      id = "testAdapter",
      base = file("test-adapter"),
      settings = (commonSettings ++ publishSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js sbt test adapter",
            libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0",
            previousArtifactSetting,
            binaryIssueFilters ++= BinaryIncompatibilities.TestAdapter,
        ),
  ).dependsOn(jsEnvs)

  lazy val plugin: Project = Project(
      id = "sbtPlugin",
      base = file("sbt-plugin"),
      settings = (commonSettings ++ publishIvySettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js sbt plugin",
            normalizedName := "sbt-scalajs",
            name in bintray := "sbt-scalajs-plugin", // "sbt-scalajs" was taken
            sbtPlugin := true,
            scalaBinaryVersion :=
              CrossVersion.binaryScalaVersion(scalaVersion.value),
            previousArtifactSetting,
            binaryIssueFilters ++= BinaryIncompatibilities.SbtPlugin,
            // Add API mappings for sbt (seems they don't export their API URL)
            apiMappings ++= {
            val deps = (externalDependencyClasspath in Compile).value

            val sbtJars =
              deps filter { attributed =>
                val p = attributed.data.getPath
                p.contains("/org.scala-sbt/") && p.endsWith(".jar")
              }

            val docUrl =
              url(s"http://www.scala-sbt.org/${sbtVersion.value}/api/")

            sbtJars.map(_.data -> docUrl).toMap
          },
        ),
  ).dependsOn(tools, jsEnvs, testAdapter)

  lazy val delambdafySetting = {
    scalacOptions ++=
    (if (isGeneratingEclipse) Seq()
     else if (scalaBinaryVersion.value == "2.10") Seq()
     else Seq("-Ydelambdafy:method"))
  }

  private def serializeHardcodedIR(base: File,
                                   infoAndTree: (ir.Infos.ClassInfo,
                                   ir.Trees.ClassDef)): File = {
    // We assume that there are no weird characters in the full name
    val fullName = ir.Definitions.decodeClassName(infoAndTree._1.encodedName)
    val output = base / (fullName.replace('.', '/') + ".sjsir")

    if (!output.exists()) {
      IO.createDirectory(output.getParentFile)
      val stream = new BufferedOutputStream(new FileOutputStream(output))
      try {
        ir.InfoSerializers.serialize(stream, infoAndTree._1)
        ir.Serializers.serialize(stream, infoAndTree._2)
      } finally {
        stream.close()
      }
    }
    output
  }

  lazy val javalanglib: Project = Project(
      id = "javalanglib",
      base = file("javalanglib"),
      settings = (commonSettings ++ myScalaJSSettings ++ fatalWarningsSettings) ++ Seq(
            name := "java.lang library for Scala.js",
            publishArtifact in Compile := false,
            delambdafySetting,
            noClassFilesSettings,
            resourceGenerators in Compile <+= Def.task {
            val base = (resourceManaged in Compile).value
            Seq(
                serializeHardcodedIR(base, JavaLangObject.InfoAndTree),
                serializeHardcodedIR(base, JavaLangString.InfoAndTree),
            )
          },
        ) ++ (scalaJSExternalCompileSettings),
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val javalib: Project = Project(
      id = "javalib",
      base = file("javalib"),
      settings = (commonSettings ++ myScalaJSSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Java library for Scala.js",
            publishArtifact in Compile := false,
            delambdafySetting,
            noClassFilesSettings,
        ) ++ (scalaJSExternalCompileSettings),
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val scalalib: Project = Project(
      id = "scalalib",
      base = file("scalalib"),
      settings = commonSettings ++ Seq(
            /* Link source maps to the GitHub sources of the original scalalib
             * #2195 This must come *before* the option added by myScalaJSSettings
             * because mapSourceURI works on a first-match basis.
             */
            scalacOptions += {
            "-P:scalajs:mapSourceURI:" + (artifactPath in fetchScalaSource).value.toURI +
            "->https://raw.githubusercontent.com/scala/scala/v" +
            scalaVersion.value + "/src/library/"
          },
        ) ++ myScalaJSSettings ++ Seq(
            name := "Scala library for Scala.js",
            publishArtifact in Compile := false,
            delambdafySetting,
            noClassFilesSettings,
            // The Scala lib is full of warnings we don't want to see
            scalacOptions ~=
            (_.filterNot(
                    Set("-deprecation", "-unchecked", "-feature") contains _)),
            // Tell the plugin to hack-fix bad classOf trees
            scalacOptions += "-P:scalajs:fixClassOf",
            artifactPath in fetchScalaSource :=
              target.value / "scalaSources" / scalaVersion.value,
            fetchScalaSource := {
            val s = streams.value
            val cacheDir = s.cacheDirectory
            val ver = scalaVersion.value
            val trgDir = (artifactPath in fetchScalaSource).value

            val report = updateClassifiers.value
            val scalaLibSourcesJar = report
              .select(configuration = Set("compile"),
                      module = moduleFilter(name = "scala-library"),
                      artifact = artifactFilter(`type` = "src"))
              .headOption
              .getOrElse {
                sys.error(
                    s"Could not fetch scala-library sources for version $ver")
              }

            FileFunction.cached(cacheDir / s"fetchScalaSource-$ver",
                                FilesInfo.lastModified,
                                FilesInfo.exists) { dependencies =>
              s.log.info(s"Unpacking Scala library sources to $trgDir...")

              if (trgDir.exists) IO.delete(trgDir)
              IO.createDirectory(trgDir)
              IO.unzip(scalaLibSourcesJar, trgDir)
            }(Set(scalaLibSourcesJar))

            trgDir
          },
            unmanagedSourceDirectories in Compile := {
            // Calculates all prefixes of the current Scala version
            // (including the empty prefix) to construct override
            // directories like the following:
            // - override-2.10.2-RC1
            // - override-2.10.2
            // - override-2.10
            // - override-2
            // - override
            val ver = scalaVersion.value
            val base = baseDirectory.value
            val parts = ver.split(Array('.', '-'))
            val verList = parts.inits.map { ps =>
              val len = ps.mkString(".").length
              // re-read version, since we lost '.' and '-'
              ver.substring(0, len)
            }
            def dirStr(v: String) =
              if (v.isEmpty) "overrides" else s"overrides-$v"
            val dirs = verList.map(base / dirStr(_)).filter(_.exists)
            dirs.toSeq // most specific shadow less specific
          },
            // Compute sources
            // Files in earlier src dirs shadow files in later dirs
            sources in Compile := {
            // Sources coming from the sources of Scala
            val scalaSrcDir = fetchScalaSource.value

            // All source directories (overrides shadow scalaSrcDir)
            val sourceDirectories =
              (unmanagedSourceDirectories in Compile).value :+ scalaSrcDir

            // Filter sources with overrides
            def normPath(f: File): String =
              f.getPath.replace(java.io.File.separator, "/")

            val sources = mutable.ListBuffer.empty[File]
            val paths = mutable.Set.empty[String]

            for {
              srcDir <- sourceDirectories
              normSrcDir = normPath(srcDir)
              src <- (srcDir ** "*.scala").get
            } {
              val normSrc = normPath(src)
              val path = normSrc.substring(normSrcDir.length)
              val useless =
                path.contains("/scala/collection/parallel/") ||
                path.contains("/scala/util/parsing/")
              if (!useless) {
                if (paths.add(path)) sources += src
                else streams.value.log.debug(s"not including $src")
              }
            }

            sources.result()
          },
            // Continuation plugin (when using 2.10.x)
            autoCompilerPlugins := true,
            libraryDependencies ++= {
            val ver = scalaVersion.value
            if (ver.startsWith("2.10."))
              Seq(compilerPlugin(
                      "org.scala-lang.plugins" % "continuations" % ver))
            else Nil
          },
            scalacOptions ++= {
            if (scalaVersion.value.startsWith("2.10."))
              Seq("-P:continuations:enable")
            else Nil
          },
        ) ++ (scalaJSExternalCompileSettings),
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val libraryAux: Project = Project(
      id = "libraryAux",
      base = file("library-aux"),
      settings = (commonSettings ++ myScalaJSSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js aux library",
            publishArtifact in Compile := false,
            delambdafySetting,
            noClassFilesSettings,
        ) ++ (scalaJSExternalCompileSettings),
  ).withScalaJSCompiler.dependsOnLibraryNoJar

  lazy val library: Project = Project(
      id = "library",
      base = file("library"),
      settings = (commonSettings ++ publishSettings ++ myScalaJSSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js library",
            delambdafySetting,
            scalacOptions in (Compile, doc) ++= Seq("-implicits", "-groups"),
            exportJars := !isGeneratingEclipse,
            previousArtifactSetting,
            binaryIssueFilters ++= BinaryIncompatibilities.Library,
            libraryDependencies +=
              "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
        ) ++ (scalaJSExternalCompileSettings) ++ inConfig(Compile)(Seq(
                /* Add the .sjsir files from other lib projects
                 * (but not .class files)
                 */
                mappings in packageBin := {
            /* From library, we must take everyting, except the
             * java.nio.TypedArrayBufferBridge object, whose actual
             * implementation is in javalib.
             */
            val superMappings = (mappings in packageBin).value
            val libraryMappings = superMappings.filter(
                _._2.replace('\\', '/') != "scala/scalajs/js/typedarray/TypedArrayBufferBridge$.sjsir")

            val filter = ("*.sjsir": NameFilter)

            val javalibProducts = (products in javalib).value
            val javalibMappings = javalibProducts.flatMap(
                base => Path.selectSubpaths(base, filter))
            val javalibFilteredMappings = javalibMappings.filter(
                _._2.replace('\\', '/') != "java/lang/MathJDK8Bridge$.sjsir")

            val otherProducts =
              ((products in javalanglib).value ++ (products in scalalib).value ++
                  (products in libraryAux).value)
            val otherMappings =
              otherProducts.flatMap(base => Path.selectSubpaths(base, filter))

            libraryMappings ++ otherMappings ++ javalibFilteredMappings
          },
            )),
  ).withScalaJSCompiler

  lazy val javalibEx: Project = Project(
      id = "javalibEx",
      base = file("javalib-ex"),
      settings = (commonSettings ++ publishSettings ++ myScalaJSSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js JavaLib Ex",
            delambdafySetting,
            noClassFilesSettings,
            exportJars := true,
            jsDependencies +=
              "org.webjars" % "jszip" % "2.4.0" / "jszip.min.js" commonJSName "JSZip",
        ) ++ (scalaJSExternalCompileSettings),
  ).withScalaJSCompiler.dependsOn(library)

  lazy val stubs: Project = Project(
      id = "stubs",
      base = file("stubs"),
      settings = commonSettings ++ publishSettings ++ Seq(
            name := "Scala.js Stubs",
            libraryDependencies +=
              "org.scala-lang" % "scala-reflect" % scalaVersion.value,
            previousArtifactSetting,
        ),
  )

  // Scala.js command line interface
  lazy val cli: Project = Project(
      id = "cli",
      base = file("cli"),
      settings = (commonSettings ++ publishSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js CLI",
            libraryDependencies ++= Seq(
                "com.github.scopt" %% "scopt" % "3.2.0",
            ),
            previousArtifactSetting,
            binaryIssueFilters ++= BinaryIncompatibilities.CLI,
            // assembly options
            mainClass in assembly := None, // don't want an executable JAR
            assemblyOption in assembly ~= { _.copy(includeScala = false) },
            assemblyJarName in assembly :=
              s"${normalizedName.value}-assembly_${scalaBinaryVersion.value}-${version.value}.jar",
        ),
  ).dependsOn(tools)

  // Test framework
  lazy val testInterface = Project(
      id = "testInterface",
      base = file("test-interface"),
      settings = (commonSettings ++ publishSettings ++ myScalaJSSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js test interface",
            delambdafySetting,
            previousArtifactSetting,
            binaryIssueFilters ++= BinaryIncompatibilities.TestInterface,
        ),
  ).withScalaJSCompiler.dependsOn(library)

  lazy val jasmineTestFramework = Project(
      id = "jasmineTestFramework",
      base = file("jasmine-test-framework"),
      settings = (commonSettings ++ myScalaJSSettings ++ fatalWarningsSettings) ++ Seq(
            name := "Scala.js jasmine test framework",
            jsDependencies ++= Seq(
                ProvidedJS / "jasmine-polyfills.js",
                "org.webjars" % "jasmine" % "1.3.1" / "jasmine.js" dependsOn "jasmine-polyfills.js",
            ),
        ),
  ).withScalaJSCompiler.dependsOn(library, testInterface)

  lazy val jUnitRuntime = Project(
      id = "jUnitRuntime",
      base = file("junit-runtime"),
      settings = commonSettings ++ publishSettings ++ myScalaJSSettings ++ fatalWarningsSettings ++ Seq(
            name := "Scala.js JUnit test runtime"),
  ).withScalaJSCompiler.dependsOn(testInterface)

  lazy val jUnitPlugin = Project(
      id = "jUnitPlugin",
      base = file("junit-plugin"),
      settings = commonSettings ++ publishSettings ++ fatalWarningsSettings ++ Seq(
            name := "Scala.js JUnit test plugin",
            crossVersion := CrossVersion.full,
            libraryDependencies +=
              "org.scala-lang" % "scala-compiler" % scalaVersion.value,
            exportJars := true,
        ),
  )

  // Examples

  lazy val examples: Project = Project(
      id = "examples",
      base = file("examples"),
      settings = commonSettings ++ Seq(
            name := "Scala.js examples",
        ),
  ).aggregate(helloworld, reversi, testingExample)

  lazy val exampleSettings =
    commonSettings ++ myScalaJSSettings ++ fatalWarningsSettings

  lazy val helloworld: Project = Project(
      id = "helloworld",
      base = file("examples") / "helloworld",
      settings = exampleSettings ++ Seq(
            name := "Hello World - Scala.js example",
            moduleName := "helloworld",
            persistLauncher := true,
        ),
  ).withScalaJSCompiler.dependsOn(library)

  lazy val reversi = Project(
      id = "reversi",
      base = file("examples") / "reversi",
      settings = exampleSettings ++ Seq(
            name := "Reversi - Scala.js example",
            moduleName := "reversi",
        ),
  ).withScalaJSCompiler.dependsOn(library)

  lazy val testingExample = Project(
      id = "testingExample",
      base = file("examples") / "testing",
      settings = exampleSettings ++ Seq(
            name := "Testing - Scala.js example",
            moduleName := "testing",
            jsDependencies ++= Seq(
                RuntimeDOM % "test",
                "org.webjars" % "jquery" % "1.10.2" / "jquery.js" % "test",
            ),
        ),
  ).withScalaJSCompiler.dependsOn(library, jasmineTestFramework % "test")

  // Testing

  val testTagSettings = {
    val testOptionTags = TaskKey[Seq[String]](
        "testOptionTags",
        "Task that lists all test options for javaOptions and testOptions.",
        KeyRanks.Invisible)

    Seq(
        testOptionTags := {
          @tailrec
          def envTagsFor(env: JSEnv): Seq[String] = env match {
            case env: RhinoJSEnv =>
              val baseArgs = Seq("rhino")
              if (env.sourceMap) baseArgs :+ "source-maps"
              else baseArgs

            case env: NodeJSEnv =>
              val baseArgs = Seq("nodejs", "typedarray")
              if (env.sourceMap) {
                if (!env.hasSourceMapSupport) {
                  sys.error("You must install Node.js source map support to " +
                      "run the full Scala.js test suite (npm install " +
                      "source-map-support). To deactivate source map " +
                      "tests, do: set jsEnv in " + thisProject.value.id +
                      " := NodeJSEnv().value.withSourceMap(false)")
                }
                baseArgs :+ "source-maps"
              } else {
                baseArgs
              }

            case _: PhantomJSEnv =>
              Seq("phantomjs")

            case env: RetryingComJSEnv =>
              envTagsFor(env.baseEnv)

            case _ =>
              throw new AssertionError(
                  s"Unknown JSEnv of class ${env.getClass.getName}: " +
                  "don't know what tags to specify for the test suite")
          }

          val envTags = envTagsFor((resolvedJSEnv in Test).value)

          val stage = (scalaJSStage in Test).value

          val sems = stage match {
            case FastOptStage => (scalaJSSemantics in (Test, fastOptJS)).value
            case FullOptStage => (scalaJSSemantics in (Test, fullOptJS)).value
          }

          val semTags =
            (if (sems.asInstanceOfs == CheckedBehavior.Compliant)
               Seq("compliant-asinstanceofs")
             else Seq()) ++
            (if (sems.moduleInit == CheckedBehavior.Compliant)
               Seq("compliant-moduleinit")
             else Seq()) ++
            (if (sems.strictFloats) Seq("strict-floats")
             else Seq()) ++
            (if (sems.productionMode) Seq("production-mode")
             else Seq("development-mode"))

          val stageTag = stage match {
            case FastOptStage => "fastopt-stage"
            case FullOptStage => "fullopt-stage"
          }

          envTags ++ (semTags :+ stageTag)
        },
        javaOptions in Test ++= {
          def scalaJSProp(name: String): String =
            s"-Dscalajs.$name=true"

          testOptionTags.value.map(scalaJSProp) :+ "-Dscalajs.testsuite.testtag=testtag.value"
        },
        testOptions in Test ++= {
          def testArgument(arg: String): Tests.Argument =
            Tests.Argument("-t" + arg)

          testOptionTags.value.map(testArgument)
        },
    )
  }

  def testSuiteCommonSettings(isJSTest: Boolean): Seq[Setting[_]] = Seq(
      publishArtifact in Compile := false,
      scalacOptions ~= (_.filter(_ != "-deprecation")),
      // Need reflect for typechecking macros
      libraryDependencies +=
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-a"),
      unmanagedSourceDirectories in Test ++= {
        def includeIf(testDir: File, condition: Boolean): List[File] =
          if (condition) List(testDir)
          else Nil

        val testDir = (sourceDirectory in Test).value
        val sharedTestDir =
          testDir.getParentFile.getParentFile.getParentFile / "shared/src/test"

        List(sharedTestDir / "scala") ++ includeIf(
            sharedTestDir / "require-jdk7",
            javaVersion.value >= 7) ++ includeIf(
            sharedTestDir / "require-jdk8", javaVersion.value >= 8)
      },
      sources in Test ++= {
        /* Can't add require-sam as unmanagedSourceDirectories because of the use
         * of scalacOptions. Hence sources are added individually.
         * Note that a testSuite/test will not trigger a compile when sources are
         * modified in require-sam
         */
        if (isJSTest && scalaBinaryVersion.value != "2.10" &&
            scalacOptions.value.contains("-Xexperimental")) {
          val sourceDir = (sourceDirectory in Test).value / "require-sam"
          (sourceDir ** "*.scala").get
        } else {
          Nil
        }
      },
  )

  lazy val testSuite: Project = Project(
      id = "testSuite",
      base = file("test-suite/js"),
      settings = commonSettings ++ myScalaJSSettings ++ testTagSettings ++ testSuiteCommonSettings(
            isJSTest = true) ++ Seq(
            name := "Scala.js test suite",
            jsDependencies +=
              ProvidedJS / "ScalaJSDefinedTestNatives.js" % "test",
            scalaJSSemantics ~=
            (_.withRuntimeClassName(_.fullName match {
                case "org.scalajs.testsuite.compiler.ReflectionTest$RenamedTestClass" =>
                  "renamed.test.Class"
                case fullName =>
                  fullName
              })),
            /* Generate a scala source file that throws exceptions in
             * various places (while attaching the source line to the
             * exception). When we catch the exception, we can then
             * compare the attached source line and the source line
             * calculated via the source maps.
             *
             * see test-suite/src/test/resources/SourceMapTestTemplate.scala
             */
            sourceGenerators in Test <+= Def.task {
            val dir = (sourceManaged in Test).value
            IO.createDirectory(dir)

            val template = IO.read(
                (resourceDirectory in Test).value / "SourceMapTestTemplate.scala")

            def lineNo(cs: CharSequence) =
              (0 until cs.length).count(i => cs.charAt(i) == '\n') + 1

            var i = 0
            val pat = "/\\*{2,3}/".r
            val replaced = pat.replaceAllIn(template, {
              mat =>
                val lNo = lineNo(mat.before)
                val res =
                  if (mat.end - mat.start == 5)
                    // matching a /***/
                    s"if (TC.is($i)) { throw new TestException($lNo) } else "
                  else
                    // matching a /**/
                    s"; if (TC.is($i)) { throw new TestException($lNo) } ;"

                i += 1

                res
            })

            val outFile = dir / "SourceMapTest.scala"
            val unitTests = (0 until i)
              .map(i => s"@Test def workTest$i(): Unit = test($i)")
              .mkString("; ")
            IO.write(outFile,
                     replaced.replace(
                         "@Test def workTest(): Unit = sys.error(\"stubs\")",
                         unitTests))
            Seq(outFile)
          },
            scalacOptions in Test ++= {
            if (isGeneratingEclipse) {
              Seq.empty
            } else {
              val jar = (packageBin in (jUnitPlugin, Compile)).value
              Seq(s"-Xplugin:$jar")
            }
          },
        ),
  ).withScalaJSCompiler.dependsOn(
      library,
      jUnitRuntime,
      jasmineTestFramework % "test",
  )

  lazy val testSuiteJVM: Project = Project(
      id = "testSuiteJVM",
      base = file("test-suite/jvm"),
      settings = commonSettings ++ testSuiteCommonSettings(isJSTest = false) ++ Seq(
            name := "Scala.js test suite on JVM",
            libraryDependencies +=
              "com.novocode" % "junit-interface" % "0.11" % "test",
        ),
  )

  lazy val noIrCheckTest: Project = Project(
      id = "noIrCheckTest",
      base = file("no-ir-check-test"),
      settings = commonSettings ++ myScalaJSSettings ++ testTagSettings ++ Seq(
            name := "Scala.js not IR checked tests",
            scalaJSOptimizerOptions ~=
            (_.withCheckScalaJSIR(false).withBypassLinkingErrors(true)),
            publishArtifact in Compile := false,
        ),
  ).withScalaJSCompiler.dependsOn(library, jasmineTestFramework % "test")

  lazy val javalibExTestSuite: Project = Project(
      id = "javalibExTestSuite",
      base = file("javalib-ex-test-suite"),
      settings = (commonSettings ++ myScalaJSSettings ++ testTagSettings) ++ Seq(
            name := "JavaLib Ex Test Suite",
            publishArtifact in Compile := false,
            scalacOptions in Test ~= (_.filter(_ != "-deprecation")),
        ),
  ).withScalaJSCompiler.dependsOn(javalibEx, jasmineTestFramework % "test")

  lazy val partest: Project = Project(
      id = "partest",
      base = file("partest"),
      settings = commonSettings ++ fatalWarningsSettings ++ Seq(
            name := "Partest for Scala.js",
            moduleName := "scalajs-partest",
            resolvers += Resolver.typesafeIvyRepo("releases"),
            artifactPath in fetchScalaSource :=
              baseDirectory.value / "fetchedSources" / scalaVersion.value,
            fetchScalaSource := {
            import org.eclipse.jgit.api._

            val s = streams.value
            val ver = scalaVersion.value
            val trgDir = (artifactPath in fetchScalaSource).value

            if (!trgDir.exists) {
              s.log.info(s"Fetching Scala source version $ver")

              // Make parent dirs and stuff
              IO.createDirectory(trgDir)

              // Clone scala source code
              new CloneCommand()
                .setDirectory(trgDir)
                .setURI("https://github.com/scala/scala.git")
                .call()
            }

            // Checkout proper ref. We do this anyway so we fail if
            // something is wrong
            val git = Git.open(trgDir)
            s.log.info(s"Checking out Scala source version $ver")
            git.checkout().setName(s"v$ver").call()

            trgDir
          },
            libraryDependencies ++= {
            if (shouldPartest.value)
              Seq(
                  "org.scala-sbt" % "sbt" % sbtVersion.value,
                  "org.scala-lang.modules" %% "scala-partest" % "1.0.9",
                  "com.google.javascript" % "closure-compiler" % "v20130603",
                  "io.apigee" % "rhino" % "1.7R5pre4",
                  "com.googlecode.json-simple" % "json-simple" % "1.1.1" exclude
                  ("junit", "junit"),
              )
            else Seq()
          },
            sources in Compile := {
            if (shouldPartest.value) {
              // Partest sources and some sources of sbtplugin (see above)
              val baseSrcs = (sources in Compile).value
              // Sources for tools (and hence IR)
              val toolSrcs = (sources in (tools, Compile)).value
              // Sources for js-envs
              val jsenvSrcs = {
                val jsenvBase =
                  ((scalaSource in (jsEnvs, Compile)).value / "org/scalajs/jsenv")

                val scalaFilter: FileFilter = "*.scala"
                val files =
                  ((jsenvBase * scalaFilter) +++
                      (jsenvBase / "nodejs" ** scalaFilter) +++
                      (jsenvBase / "rhino" ** scalaFilter))

                files.get
              }
              toolSrcs ++ baseSrcs ++ jsenvSrcs
            } else Seq()
          },
        ),
  ).dependsOn(compiler)

  lazy val partestSuite: Project = Project(
      id = "partestSuite",
      base = file("partest-suite"),
      settings = commonSettings ++ fatalWarningsSettings ++ Seq(
            name := "Scala.js partest suite",
            fork in Test := true,
            javaOptions in Test += "-Xmx1G",
            // Override the dependency of partest - see #1889
            dependencyOverrides +=
              "org.scala-lang" % "scala-library" % scalaVersion.value % "test",
            testFrameworks ++= {
            if (shouldPartest.value)
              Seq(new TestFramework("scala.tools.partest.scalajs.Framework"))
            else Seq()
          },
            definedTests in Test <++= Def.taskDyn[Seq[sbt.TestDefinition]] {
            if (shouldPartest.value)
              Def.task {
                val _ = (fetchScalaSource in partest).value
                Seq(new sbt.TestDefinition(
                        s"partest-${scalaVersion.value}",
                        // marker fingerprint since there are no test classes
                        // to be discovered by sbt:
                        new sbt.testing.AnnotatedFingerprint {
                      def isModule = true
                      def annotationName = "partest"
                    },
                        true,
                        Array(),
                    ))
              } else {
              Def.task(Seq())
            }
          },
        ),
  ).dependsOn(partest % "test", library)
}
