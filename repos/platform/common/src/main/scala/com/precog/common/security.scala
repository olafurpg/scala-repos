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
package com.precog.common

import blueeyes.json._
import blueeyes.json.serialization.{Extractor, Decomposer}
import blueeyes.json.serialization.DefaultSerialization.{
  DateTimeDecomposer => _, DateTimeExtractor => _, _
}
import blueeyes.json.serialization.Extractor._

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scalaz._
import scalaz.syntax.bifunctor._

package object security {
  type APIKey = String
  type GrantId = String

  private val isoFormat = ISODateTimeFormat.dateTime

  implicit val TZDateTimeDecomposer: Decomposer[DateTime] =
    new Decomposer[DateTime] {
      override def decompose(d: DateTime): JValue = JString(isoFormat.print(d))
    }

  implicit val TZDateTimeExtractor: Extractor[DateTime] =
    new Extractor[DateTime] {
      override def validated(obj: JValue): Validation[Error, DateTime] =
        obj match {
          case JString(dt) =>
            (Thrown.apply _) <-: Validation.fromTryCatch(
                isoFormat.parseDateTime(dt))
          case _ =>
            Failure(Invalid("Date time must be represented as JSON string"))
        }
    }
}
