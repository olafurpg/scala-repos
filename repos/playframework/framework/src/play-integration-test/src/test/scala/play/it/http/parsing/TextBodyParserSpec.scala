/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.it.http.parsing

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.test._
import play.api.mvc.{BodyParser, BodyParsers}

object TextBodyParserSpec extends PlaySpecification {

  "The text body parser" should {

    def parse(text: String,
              contentType: Option[String],
              encoding: String,
              bodyParser: BodyParser[String] = BodyParsers.parse.tolerantText)(
        implicit mat: Materializer) = {
      await(
          bodyParser(FakeRequest().withHeaders(
                  contentType.map(CONTENT_TYPE -> _).toSeq: _*))
            .run(Source.single(ByteString(text, encoding)))
      )
    }

    "parse text bodies" in new WithApplication() {
      parse("bar", Some("text/plain"), "utf-8") must beRight("bar")
    }

    "honour the declared charset" in new WithApplication() {
      parse("bär", Some("text/plain; charset=utf-8"), "utf-8") must beRight(
          "bär")
      parse("bär", Some("text/plain; charset=utf-16"), "utf-16") must beRight(
          "bär")
      parse("bär", Some("text/plain; charset=iso-8859-1"), "iso-8859-1") must beRight(
          "bär")
    }

    "default to iso-8859-1 encoding" in new WithApplication() {
      parse("bär", Some("text/plain"), "iso-8859-1") must beRight("bär")
      parse("bär", None, "iso-8859-1") must beRight("bär")
    }

    "accept text/plain content type" in new WithApplication() {
      parse("bar", Some("text/plain"), "utf-8", BodyParsers.parse.text) must beRight(
          "bar")
    }

    "reject non text/plain content types" in new WithApplication() {
      parse("bar", Some("application/xml"), "utf-8", BodyParsers.parse.text) must beLeft
      parse("bar", None, "utf-8", BodyParsers.parse.text) must beLeft
    }
  }
}
