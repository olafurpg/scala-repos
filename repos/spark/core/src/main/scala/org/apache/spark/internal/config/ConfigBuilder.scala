/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.spark.internal.config

import java.util.concurrent.TimeUnit

import org.apache.spark.network.util.{ByteUnit, JavaUtils}

private object ConfigHelpers {

  def toNumber[T](s: String,
                  converter: String => T,
                  key: String,
                  configType: String): T = {
    try {
      converter(s)
    } catch {
      case _: NumberFormatException =>
        throw new IllegalArgumentException(
            s"$key should be $configType, but was $s")
    }
  }

  def toBoolean(s: String, key: String): Boolean = {
    try {
      s.toBoolean
    } catch {
      case _: IllegalArgumentException =>
        throw new IllegalArgumentException(
            s"$key should be boolean, but was $s")
    }
  }

  def stringToSeq[T](str: String, converter: String => T): Seq[T] = {
    str.split(",").map(_.trim()).filter(_.nonEmpty).map(converter)
  }

  def seqToString[T](v: Seq[T], stringConverter: T => String): String = {
    v.map(stringConverter).mkString(",")
  }

  def timeFromString(str: String, unit: TimeUnit): Long =
    JavaUtils.timeStringAs(str, unit)

  def timeToString(v: Long, unit: TimeUnit): String =
    TimeUnit.MILLISECONDS.convert(v, unit) + "ms"

  def byteFromString(str: String, unit: ByteUnit): Long = {
    val (input, multiplier) = if (str.length() > 0 && str.charAt(0) == '-') {
      (str.substring(1), -1)
    } else {
      (str, 1)
    }
    multiplier * JavaUtils.byteStringAs(input, unit)
  }

  def byteToString(v: Long, unit: ByteUnit): String =
    unit.convertTo(v, ByteUnit.BYTE) + "b"
}

/**
  * A type-safe config builder. Provides methods for transforming the input data (which can be
  * used, e.g., for validation) and creating the final config entry.
  *
  * One of the methods that return a [[ConfigEntry]] must be called to create a config entry that
  * can be used with [[SparkConf]].
  */
private[spark] class TypedConfigBuilder[T](val parent: ConfigBuilder,
                                           val converter: String => T,
                                           val stringConverter: T => String) {

  import ConfigHelpers._

  def this(parent: ConfigBuilder, converter: String => T) = {
    this(parent, converter, Option(_).map(_.toString).orNull)
  }

  def transform(fn: T => T): TypedConfigBuilder[T] = {
    new TypedConfigBuilder(parent, s => fn(converter(s)), stringConverter)
  }

  def checkValues(validValues: Set[T]): TypedConfigBuilder[T] = {
    transform { v =>
      if (!validValues.contains(v)) {
        throw new IllegalArgumentException(
            s"The value of ${parent.key} should be one of ${validValues
              .mkString(", ")}, but was $v")
      }
      v
    }
  }

  def toSequence: TypedConfigBuilder[Seq[T]] = {
    new TypedConfigBuilder(parent,
                           stringToSeq(_, converter),
                           seqToString(_, stringConverter))
  }

  /** Creates a [[ConfigEntry]] that does not require a default value. */
  def optional: OptionalConfigEntry[T] = {
    new OptionalConfigEntry[T](parent.key,
                               converter,
                               stringConverter,
                               parent._doc,
                               parent._public)
  }

  /** Creates a [[ConfigEntry]] that has a default value. */
  def withDefault(default: T): ConfigEntry[T] = {
    val transformedDefault = converter(stringConverter(default))
    new ConfigEntryWithDefault[T](parent.key,
                                  transformedDefault,
                                  converter,
                                  stringConverter,
                                  parent._doc,
                                  parent._public)
  }

  /**
    * Creates a [[ConfigEntry]] that has a default value. The default value is provided as a
    * [[String]] and must be a valid value for the entry.
    */
  def withDefaultString(default: String): ConfigEntry[T] = {
    val typedDefault = converter(default)
    new ConfigEntryWithDefault[T](parent.key,
                                  typedDefault,
                                  converter,
                                  stringConverter,
                                  parent._doc,
                                  parent._public)
  }
}

/**
  * Basic builder for Spark configurations. Provides methods for creating type-specific builders.
  *
  * @see TypedConfigBuilder
  */
private[spark] case class ConfigBuilder(key: String) {

  import ConfigHelpers._

  var _public = true
  var _doc = ""

  def internal: ConfigBuilder = {
    _public = false
    this
  }

  def doc(s: String): ConfigBuilder = {
    _doc = s
    this
  }

  def intConf: TypedConfigBuilder[Int] = {
    new TypedConfigBuilder(this, toNumber(_, _.toInt, key, "int"))
  }

  def longConf: TypedConfigBuilder[Long] = {
    new TypedConfigBuilder(this, toNumber(_, _.toLong, key, "long"))
  }

  def doubleConf: TypedConfigBuilder[Double] = {
    new TypedConfigBuilder(this, toNumber(_, _.toDouble, key, "double"))
  }

  def booleanConf: TypedConfigBuilder[Boolean] = {
    new TypedConfigBuilder(this, toBoolean(_, key))
  }

  def stringConf: TypedConfigBuilder[String] = {
    new TypedConfigBuilder(this, v => v)
  }

  def timeConf(unit: TimeUnit): TypedConfigBuilder[Long] = {
    new TypedConfigBuilder(this,
                           timeFromString(_, unit),
                           timeToString(_, unit))
  }

  def bytesConf(unit: ByteUnit): TypedConfigBuilder[Long] = {
    new TypedConfigBuilder(this,
                           byteFromString(_, unit),
                           byteToString(_, unit))
  }

  def fallbackConf[T](fallback: ConfigEntry[T]): ConfigEntry[T] = {
    new FallbackConfigEntry(key, _doc, _public, fallback)
  }
}
