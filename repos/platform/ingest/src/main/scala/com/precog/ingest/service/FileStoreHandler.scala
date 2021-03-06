/*
 *  ____    ____    _____    ____    ___     ____ 
 * |  _ \  |  _ \  | ____|  / ___|  / _/    / ___|        Precog (R)
 * | |_) | | |_) | |  _|   | |     | |  /| | |  _         Advanced Analytics Engine for NoSQL Data
 * |  __/  |  _ <  | |___  | |___  |/ _| | | |_| |        Copyright (C) 2010 - 2013 SlamData, Inc.
 * |_|     |_| \_\ |_____|  \____|   /__/   \____|        All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the 
 * GNU Affero General Public License as published by the Free Software Foundation, either version 
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See 
 * the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this 
 * program. If not, see <http://www.gnu.org/licenses/>.
 *
 */
package com.precog.ingest
package service

import com.precog.common.Path
import com.precog.common.client._
import com.precog.common.ingest._
import com.precog.common.jobs._
import com.precog.common.security._
import com.precog.common.services.ServiceLocation
import com.precog.common.services.ServiceHandlerUtil._

import blueeyes.core.data.ByteChunk
import blueeyes.core.http._
import blueeyes.core.http.HttpStatusCodes._
import blueeyes.core.http.HttpHeaders._
import blueeyes.core.service._
import blueeyes.json._
import blueeyes.util.Clock

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext
import akka.util.Timeout

import java.net.URLEncoder
import java.util.UUID

import scalaz._
import scalaz.EitherT._
import scalaz.std.string._
import scalaz.std.option._
import scalaz.syntax.monad._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.semigroup._
import com.weiglewilczek.slf4s._

class FileStoreHandler(serviceLocation: ServiceLocation,
                       val permissionsFinder: PermissionsFinder[Future],
                       jobManager: JobManager[Response],
                       val clock: Clock,
                       eventStore: EventStore[Future],
                       ingestTimeout: Timeout)(
    implicit val M: Monad[Future], executor: ExecutionContext)
    extends CustomHttpService[
        ByteChunk, (APIKey, Path) => Future[HttpResponse[JValue]]]
    with IngestSupport with Logging {

  private val baseURI = serviceLocation.toURI

  private def validateWriteMode(
      contentType: MimeType,
      method: HttpMethod): Validation[String, WriteMode] = {
    import FileContent._
    (contentType, method) match {
      case (_, HttpMethods.POST) => Success(AccessMode.Create)
      case (_, HttpMethods.PUT) => Success(AccessMode.Replace)
      case (otherType, otherMethod) =>
        Failure(
            "Content-Type %s is not supported for use with method %s.".format(
                otherType, otherMethod))
    }
  }

  private def validateFileName(
      fileName: Option[String],
      storeMode: WriteMode): Validation[String, Path => Path] = {
    (storeMode: @unchecked) match {
      case AccessMode.Create =>
        for {
          fn0 <- fileName.toSuccess("X-File-Name header must be provided.")
          // if the filename after URL encoding is the same as before, accept it.
          _ <- (URLEncoder.encode(fn0, "UTF-8") == fn0)
            .unlessM[({ type λ[α] = Validation[String, α] })#λ, Unit] {
            Failure(
                "\"%s\" is not a valid file name; please do not use characters which require URL encoding."
                  .format(fn0))
          }
        } yield { (dir: Path) =>
          dir / Path(fn0)
        }

      case AccessMode.Replace =>
        fileName
          .toFailure(())
          .leftMap(_ =>
                "X-File-Name header not respected for PUT requests; please specify the resource to update via the URL.") map {
          _ => (resource: Path) =>
            resource
        }
    }
  }

  val service: HttpRequest[ByteChunk] => Validation[
      NotServed, (APIKey, Path) => Future[HttpResponse[JValue]]] =
    (request: HttpRequest[ByteChunk]) =>
      {
        val contentType0 = request.headers
          .header[`Content-Type`]
          .flatMap(_.mimeTypes.headOption)
          .toSuccess("Unable to determine content type for file create.")
        val storeMode0 =
          contentType0 flatMap {
            validateWriteMode(_: MimeType, request.method)
          }
        val pathf0 =
          storeMode0 flatMap {
            validateFileName(request.headers.get("X-File-Name"), _: WriteMode)
          }

        (pathf0.toValidationNel |@| contentType0.toValidationNel |@| storeMode0.toValidationNel) {
          (pathf, contentType, storeMode) => (apiKey: APIKey, path: Path) =>
            {
              val timestamp = clock.now()
              val fullPath = pathf(path)

              findRequestWriteAuthorities(
                  request, apiKey, fullPath, Some(timestamp.toInstant)) {
                authorities =>
                  request.content map { content =>
                    (for {
                      jobId <- jobManager
                        .createJob(apiKey,
                                   "ingest-" + path,
                                   "ingest",
                                   None,
                                   Some(timestamp))
                        .map(_.id)
                        .leftMap { errors =>
                          logger.error(
                              "File creation failed due to errors in job service: " +
                              errors)
                          serverError(errors)
                        }
                      bytes <- EitherT {
                        // FIXME: This should only read at most approximately the upload limit,
                        // so we don't read GB of data into memory from users.
                        ByteChunk.forceByteArray(content) map {
                          // TODO: Raise/remove this limit
                          case b if b.length > 614400 =>
                            logger.error(
                                "Rejecting excessive file upload of size %d"
                                  .format(b.length))
                            -\/(badRequest(
                                    "File uploads are currently limited to 600KB"))

                          case b =>
                            \/-(b)
                        }
                      }
                      storeFile = StoreFile(apiKey,
                                            fullPath,
                                            Some(authorities),
                                            jobId,
                                            FileContent(bytes, contentType),
                                            timestamp.toInstant,
                                            StreamRef.forWriteMode(storeMode,
                                                                   true))
                      _ <- right(eventStore.save(storeFile, ingestTimeout))
                    } yield {
                      val resultsPath = (baseURI.path |+| Some("/data/fs/" +
                              fullPath.path)).map(_.replaceAll("//", "/"))
                      val locationHeader =
                        Location(baseURI.copy(path = resultsPath))
                      HttpResponse[JValue](
                          Accepted,
                          headers = HttpHeaders(List(locationHeader)))
                    }).fold(e => e, x => x)
                  } getOrElse {
                    Promise successful HttpResponse[JValue](HttpStatus(
                            BadRequest,
                            "Attempt to create a file without body content."))
                  }
              }
            }
        } leftMap { errors =>
          DispatchError(BadRequest, errors.list.mkString("; "))
        }
    }

  val metadata = NoMetadata
}
