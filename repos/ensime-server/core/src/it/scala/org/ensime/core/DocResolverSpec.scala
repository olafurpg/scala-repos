// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import akka.actor.ActorSystem
import akka.testkit.TestActorRef
import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec

class DocResolverSpec extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture {

  val original = EnsimeConfigFixture.DocsTestProject

  def resolver(java: Option[String] = None)(implicit c: EnsimeConfig, s: ActorSystem) =
    TestActorRef[DocResolver](DocResolver(java = java)).underlyingActor

  "DocResolver" should "support a wide range of queries" in withEnsimeConfig { implicit c =>
    withTestKit { tk =>
      import tk._

      val serv = resolver()

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("map[B](f:A=>B):Option[B]")),
        DocSig(DocFqn("scala", "Some"), Some("map(scala.Function1)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@map[B](f:A=>B):Option[B]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Boolean"), None),
        DocSig(DocFqn("", "boolean"), None)
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Boolean")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Option"), Some("isDefined:Boolean")),
        DocSig(DocFqn("scala", "Option"), Some("isDefined"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Option@isDefined:Boolean")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("flatMap[B](f:A=>Option[B]):Option[B]")),
        DocSig(DocFqn("scala", "Some"), Some("flatMap(scala.Function1)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@flatMap[B](f:A=>Option[B]):Option[B]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("flatten[B](implicitev:<:<[A,Option[B]]):Option[B]")),
        DocSig(DocFqn("scala", "Some"), Some("flatten(scala.Predef.<:<)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@flatten[B](implicitev:<:<[A,Option[B]]):Option[B]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("fold[B](ifEmpty:=>B)(f:A=>B):B")),
        DocSig(DocFqn("scala", "Some"), Some("fold(scala.<byname>, scala.Function1)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@fold[B](ifEmpty:=>B)(f:A=>B):B")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("mkString(start:String,sep:String,end:String):String")),
        DocSig(DocFqn("scala", "Some"), Some("mkString(java.lang.String, java.lang.String, java.lang.String)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@mkString(start:String,sep:String,end:String):String")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("mkString:String")),
        DocSig(DocFqn("scala", "Some"), Some("mkString"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@mkString:String")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("mkString(sep:String):String")),
        DocSig(DocFqn("scala", "Some"), Some("mkString(java.lang.String)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@mkString(sep:String):String")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("getOrElse[B>:A](default:=>B):B")),
        DocSig(DocFqn("scala", "Some"), Some("getOrElse(scala.<byname>)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@getOrElse[B>:A](default:=>B):B")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("grouped(size:Int):Iterator[Repr]")),
        DocSig(DocFqn("scala", "Some"), Some("grouped(int)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@grouped(size:Int):Iterator[Repr]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala.collection.immutable", "List$"), Some("empty[A]:List[A]")),
        DocSig(DocFqn("scala.collection.immutable", "List"), Some("empty"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.collection.immutable.List$@empty[A]:List[A]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("move(x$1:java.io.File,x$2:java.io.File):Unit")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("move(java.io.File, java.io.File)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#move(java.io.File, java.io.File)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("asByteSource(x$1:java.io.File):com.google.common.io.ByteSource")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("asByteSource(java.io.File)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#asByteSource(java.io.File)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("map(x$1:java.io.File,x$2:java.nio.channels.FileChannel.MapMode):java.nio.MappedByteBuffer")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("map(java.io.File, java.nio.channels.FileChannel.MapMode)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#map(java.io.File, java.nio.channels.FileChannel.MapMode)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("map(x$1:java.io.File,x$2:java.nio.channels.FileChannel.MapMode,x$3:Long):java.nio.MappedByteBuffer")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("map(java.io.File, java.nio.channels.FileChannel.MapMode, long)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#map(java.io.File, java.nio.channels.FileChannel.MapMode, long)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("write(x$1:Array[Byte],x$2:java.io.File):Unit")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("write(byte[], java.io.File)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#write(byte[], java.io.File)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), None),
        DocSig(DocFqn("scala", "Some"), None)
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Int"), None),
        DocSig(DocFqn("", "int"), None)
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Int")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Predef$$DummyImplicit$"), None),
        DocSig(DocFqn("scala", "Predef.DummyImplicit"), None)
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Predef$$DummyImplicit$")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala.collection", ".package"), None),
        DocSig(DocFqn("scala.collection", "package"), None)
      )) shouldBe None

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "package"), Some("Exception=Exception")),
        DocSig(DocFqn("scala", "package"), Some("Exception"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.package@Exception=Exception")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala", "Some"), Some("++[B>:A,That](that:scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That")),
        DocSig(DocFqn("scala", "Some"), Some("++(scala.collection.GenTraversableOnce, scala.collection.generic.CanBuildFrom)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.Some@++[B](that:scala.collection.GenTraversableOnce[B]):Option[B]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala.collection.immutable", "List"), Some("flatMap[B,That](f:A=>scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That")),
        DocSig(DocFqn("scala.collection.immutable", "List"), Some("flatMap(scala.Function1, scala.collection.generic.CanBuildFrom)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.collection.immutable.List@flatMap[B](f:A=>scala.collection.GenTraversableOnce[B]):List[B]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("scala.collection.immutable", "List"), Some("collect[B,That](pf:PartialFunction[A,B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That")),
        DocSig(DocFqn("scala.collection.immutable", "List"), Some("collect(scala.PartialFunction, scala.collection.generic.CanBuildFrom)"))
      )) shouldBe Some("docs/scala-library-" + c.scalaVersion + "-javadoc.jar/index.html#scala.collection.immutable.List@collect[B](pf:PartialFunction[A,B]):List[B]")

      serv.resolve(DocSigPair(
        DocSig(DocFqn("com.google.common.io", "Files$"), Some("simplifyPath(x$1:String):String")),
        DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)"))
      )) shouldBe Some("docs/guava-18.0-javadoc.jar/com/google/common/io/Files.html#simplifyPath(java.lang.String)")
    }
  }

  it should "support Java 6 online docs" in withEnsimeConfig { implicit config =>
    withTestKit { tk =>
      import tk._

      val serv = resolver(Some("1.6"))

      serv.resolve(
        DocSig(DocFqn("java.io", "File"), None)
      ) shouldBe Some("http://docs.oracle.com/javase/6/docs/api/java/io/File.html")

      serv.resolve(
        DocSig(DocFqn("java.util", "Map.Entry"), None)
      ) shouldBe Some("http://docs.oracle.com/javase/6/docs/api/java/util/Map.Entry.html")

      serv.resolve(
        DocSig(DocFqn("java.util", "package"), None)
      ) shouldBe Some("http://docs.oracle.com/javase/6/docs/api/java/util/package-summary.html")

    }
  }
  it should "support Java 8 docs" in withEnsimeConfig { implicit config =>
    withTestKit { tk =>
      import tk._

      val serv = resolver(Some("1.8"))

      // a local java 8 javadoc
      serv.resolve(
        DocSig(DocFqn("com.github.dvdme.ForecastIOLib", "ForecastIO"), Some("getForecast(com.eclipsesource.json.JsonObject)"))
      ) shouldBe Some("docs/ForecastIOLib-1.5.1-javadoc.jar/com/github/dvdme/ForecastIOLib/ForecastIO.html#getForecast-com.eclipsesource.json.JsonObject-")

      serv.resolve(
        DocSig(DocFqn("java.io", "File"), Some("delete()"))
      ) shouldBe Some("http://docs.oracle.com/javase/8/docs/api/java/io/File.html#delete--")

      serv.resolve(
        DocSig(DocFqn("java.lang", "Math"), Some("max(int, int)"))
      ) shouldBe Some("http://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#max-int-int-")

      serv.resolve(
        DocSig(DocFqn("java.util", "Arrays"), Some("binarySearch(int[], int)"))
      ) shouldBe Some("http://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#binarySearch-int:A-int-")
    }
  }
}
