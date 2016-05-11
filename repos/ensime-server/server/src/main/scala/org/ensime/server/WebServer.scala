// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.io.File

import akka.actor._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport
import akka.http.scaladsl.model.{ HttpEntity, _ }
import akka.http.scaladsl.server._
import akka.stream._
import akka.util.{ ByteString, Timeout }
import com.google.common.io.Files
import org.ensime.api._
import org.ensime.jerk._

import scala.concurrent.Future

trait WebServer {
  implicit def system: ActorSystem
  implicit def timeout: Timeout
  implicit def mat: Materializer

  def restHandler(in: RpcRequest): Future[EnsimeServerMessage]

  def websocketHandler(target: ActorRef): ActorRef

  /**
   * @param filename of the javadoc archive
   * @param entry of the file within the archive
   * @return the contents of the entry in filename
   */
  def docJarContent(filename: String, entry: String): Option[ByteString]

  /**
   * @return all documentation jars that are available to be served.
   */
  def docJars(): Set[File]

  import Directives._
  import JerkEnvelopeFormats._
  import JerkFormats._
  import Route._
  import ScalaXmlSupport._
  import SprayJsonSupport._
  import WebSocketBoilerplate._

  val route = seal {
    path("rpc") {
      post {
        entity(as[RpcRequest]) { request =>
          complete {
            restHandler(request)
          }
        }
      }
    } ~ path("docs") {
      complete {
        <html>
          <head></head>
          <body>
            <h1>ENSIME: Your Project's Documentation</h1>
            <ul>{
              docJars().toList.map(_.getName).sorted.map { f =>
                <li><a href={ s"docs/$f/index.html" }>{ f }</a> </li>
              }
            }</ul>
          </body>
        </html>
      }
    } ~ path("docs" / """[^/]+\.jar""".r / Rest) { (filename, entry) =>
      rejectEmptyResponse {
        complete {
          for {
            media <- MediaTypes.forExtension(Files.getFileExtension(entry))
            content <- docJarContent(filename, entry)
          } yield {
            HttpResponse(entity = HttpEntity(ContentType(media, None), content))
          }
        }
      }
    } ~ path("jerky") {
      get {
        jsonWebsocket[RpcRequestEnvelope, RpcResponseEnvelope](websocketHandler)
      }
    }
  }

}
