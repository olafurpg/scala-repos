/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.it.http

import play.api.inject.guice.GuiceApplicationBuilder

import scala.concurrent.duration._

import play.api.mvc._
import play.api.test._
import play.api.libs.ws._
import play.it._

object NettyFormFieldOrderSpec
    extends FormFieldOrderSpec
    with NettyIntegrationSpecification
object AkkaHttpFormFieldOrderSpec
    extends FormFieldOrderSpec
    with AkkaHttpIntegrationSpecification

trait FormFieldOrderSpec
    extends PlaySpecification
    with ServerIntegrationSpecification {

  "Play' form URL Decoding " should {

    val urlEncoded =
      "One=one&Two=two&Three=three&Four=four&Five=five&Six=six&Seven=seven"
    val contentType = "application/x-www-form-urlencoded"

    val fakeApp = GuiceApplicationBuilder().routes {
      case ("POST", "/") =>
        Action { request: Request[AnyContent] =>
          // Check precondition. This needs to be an x-www-form-urlencoded request body
          request.contentType must beSome(contentType)
          // The following just ingests the request body and converts it to a sequnce of strings of the form name=value
          val pairs: Seq[String] = {
            request.body.asFormUrlEncoded map {
              params: Map[String, Seq[String]] =>
                {
                  for ((key: String, value: Seq[String]) <- params)
                    yield key + "=" + value.mkString
                }.toSeq
            }
          }.getOrElse(Seq.empty[String])
          // And now this just puts it all back into one string separated by & to reincarnate, hopefully, the
          // original url_encoded string
          val reencoded = pairs.mkString("&")
          // Return the re-encoded body as the result body for comparison below
          Results.Ok(reencoded)
        }
    }.build()

    "preserve form field order" in new WithServer(fakeApp) {

      import scala.concurrent.Future

      val future: Future[WSResponse] = WS
        .url("http://localhost:" + port + "/")
        .withHeaders("Content-Type" -> contentType)
        .withRequestTimeout(10000.millis)
        .post(urlEncoded)

      val response = await(future)
      response.status must equalTo(OK)
      // Above the response to the request caused the body to be reconstituted as the url_encoded string.
      // Validate that this is in fact the case, which is the point of this test.
      response.body must equalTo(urlEncoded)
    }
  }
}
