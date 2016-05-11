// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.http.scaladsl.model.MediaTypes
import akka.util.ByteString

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport

import concurrent.Future

import akka.actor.ActorRef
import akka.event.slf4j.SLF4JLogging
import akka.http.scaladsl.testkit._
import akka.stream.ActorMaterializer
import akka.testkit._
import org.scalatest._
import akka.http.scaladsl.model.StatusCodes
import scala.xml.NodeSeq

import spray.json._

import org.ensime.api._

import org.ensime.util.file._

class WebServerSpec extends HttpFlatSpec with WebServer {

  import SprayJsonSupport._

  def restHandler(in: RpcRequest): Future[EnsimeServerMessage] =
    Future.successful(SendBackgroundMessageEvent("hello"))

  val expected = """{"typehint":"SendBackgroundMessageEvent","detail":"hello","code":105}""".parseJson

  val probe = TestProbe()
  def websocketHandler(target: ActorRef): ActorRef = probe.ref

  def docJarContent(filename: String, entry: String): Option[ByteString] =
    if (filename != "foo-1.0-javadoc.jar" || entry != "bar/Baz.html") None
    else Some(ByteString("hello"))

  def docJars(): Set[File] = Set(File("foo-javadoc.jar"), File("bar-javadoc.jar"))

  "WebServer" should "respond to REST queries" in {
    Post("/rpc", """{"typehint":"ConnectionInfoReq"}""".parseJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[JsValue] shouldBe expected
    }
  }

  it should "error to bad REST queries" in {
    Get("/rpc") ~> route ~> check {
      status shouldBe StatusCodes.MethodNotAllowed
    }
  }

  it should "respond to WebSocket queries" ignore {
    // https://github.com/akka/akka/issues/17914
    fail("no test framework yet")
  }

  it should "serve contents of documentation archives" in {
    Get("/docs/foo-1.0-javadoc.jar/bar/Baz.html#thingy()") ~> route ~> check {
      status shouldBe StatusCodes.OK
      mediaType shouldBe MediaTypes.`text/html`
      responseAs[String] shouldBe "hello"
    }

    Get("/docs/foo-1.0-javadoc.jar/bar/Bag.html#thingy()") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "provide a list of available documentation" in {
    Get("/docs") ~> route ~> check {
      status shouldBe StatusCodes.OK
      mediaType shouldBe MediaTypes.`text/xml` // hmm

      import ScalaXmlSupport._
      import StreamlinedXmlEquality._

      // err, the Equality[NodeSeq] doesn't seem to work... reformatting breaks it
      responseAs[NodeSeq] shouldEqual
        <html>
          <head/>
          <body>
            <h1>ENSIME: Your Project's Documentation</h1>
            <ul>
              <li>
                <a href="docs/bar-javadoc.jar/index.html">bar-javadoc.jar</a>
              </li>
              <li>
                <a href="docs/foo-javadoc.jar/index.html">foo-javadoc.jar</a>
              </li>
            </ul>
          </body>
        </html>
    }
  }

}

/**
 * Equivalent for akka-http-testkit use (non-trivial ordering of mixins)
 * http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0/scala/http/routing-dsl/testkit.html
 */
abstract class HttpFlatSpec
    extends FlatSpecLike with BeforeAndAfterAll
    with ScalatestRouteTest
    with TestKitBase with DefaultTimeout with ImplicitSender
    with Matchers with SLF4JLogging {
  def actorRefFactory = system
  implicit val routeTimeout: RouteTestTimeout = RouteTestTimeout(timeout.duration.dilated)
  implicit val mat = ActorMaterializer()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
  }
  override protected def afterAll(): Unit = {
    super.afterAll()
    TestKit.shutdownActorSystem(system)
  }

}
