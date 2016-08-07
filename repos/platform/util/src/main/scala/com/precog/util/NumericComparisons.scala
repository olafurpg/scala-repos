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
package com.precog.util

import org.joda.time.DateTime

object NumericComparisons {

  @inline def compare(a: Long, b: Long): Int =
    if (a < b) -1 else if (a == b) 0 else 1

  @inline def compare(a: Long, b: Double): Int = -compare(b, a)

  @inline def compare(a: Long, b: BigDecimal): Int = BigDecimal(a) compare b

  def compare(a: Double, bl: Long): Int = {
    val b = bl.toDouble
    if (b.toLong == bl) {
      if (a < b) -1 else if (a == b) 0 else 1
    } else {
      val error = math.abs(b * 2.220446049250313E-16)
      if (a < b - error) -1 else if (a > b + error) 1 else bl.signum
    }
  }

  @inline def compare(a: Double, b: Double): Int =
    if (a < b) -1 else if (a == b) 0 else 1

  @inline def compare(a: Double, b: BigDecimal): Int = BigDecimal(a) compare b

  @inline def compare(a: BigDecimal, b: Long): Int = a compare BigDecimal(b)

  @inline def compare(a: BigDecimal, b: Double): Int = a compare BigDecimal(b)

  @inline def compare(a: BigDecimal, b: BigDecimal): Int = a compare b

  @inline def compare(a: DateTime, b: DateTime): Int = {
    val res: Int = a compareTo b
    if (res < 0) -1
    else if (res > 0) 1
    else 0
  }

  @inline def eps(b: Double): Double = math.abs(b * 2.220446049250313E-16)

  def approxCompare(a: Double, b: Double): Int = {
    val aError = eps(a)
    val bError = eps(b)
    if (a + aError < b - bError) -1 else if (a - aError > b + bError) 1 else 0
  }

  import scalaz.Ordering.{LT, GT, EQ}

  @inline def order(a: Long, b: Long): scalaz.Ordering =
    if (a < b) LT else if (a == b) EQ else GT

  @inline def order(a: Double, b: Double): scalaz.Ordering =
    if (a < b) LT else if (a == b) EQ else GT

  @inline def order(a: Long, b: Double): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))

  @inline def order(a: Double, b: Long): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))

  @inline def order(a: Long, b: BigDecimal): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))

  @inline def order(a: Double, b: BigDecimal): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))

  @inline def order(a: BigDecimal, b: Long): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))

  @inline def order(a: BigDecimal, b: Double): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))

  @inline def order(a: BigDecimal, b: BigDecimal): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))

  @inline def order(a: DateTime, b: DateTime): scalaz.Ordering =
    scalaz.Ordering.fromInt(compare(a, b))
}
