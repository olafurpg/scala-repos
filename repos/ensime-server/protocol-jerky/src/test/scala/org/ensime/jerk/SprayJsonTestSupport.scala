// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.jerk

import org.scalatest._
import spray.json._

trait SprayJsonTestSupport
  this: Matchers =>

  def roundtrip[T : JsonFormat](value: T, via: Option[String] = None): Unit =
    val json = value.toJson

    via match
      case None =>
        println(
            s"check and add the following assertion: $value = ${json.prettyPrint}")
      case Some(expected) => json shouldBe expected.parseJson

    val recovered = json.convertTo[T]
    recovered shouldBe value

  def roundtrip[T : JsonFormat](value: T, via: String): Unit =
    roundtrip(value, Some(via))
