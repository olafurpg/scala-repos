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

package org.apache.spark.sql

/**
  * Runtime configuration interface for Spark. To access this, use `SparkSession.conf`.
  *
  * @since 2.0.0
  */
abstract class RuntimeConfig

  /**
    * Sets the given Spark runtime configuration property.
    *
    * @since 2.0.0
    */
  def set(key: String, value: String): RuntimeConfig

  /**
    * Sets the given Spark runtime configuration property.
    *
    * @since 2.0.0
    */
  def set(key: String, value: Boolean): RuntimeConfig

  /**
    * Sets the given Spark runtime configuration property.
    *
    * @since 2.0.0
    */
  def set(key: String, value: Long): RuntimeConfig

  /**
    * Returns the value of Spark runtime configuration property for the given key.
    *
    * @throws NoSuchElementException if the key is not set and does not have a default value
    * @since 2.0.0
    */
  @throws[NoSuchElementException]("if the key is not set")
  def get(key: String): String

  /**
    * Returns the value of Spark runtime configuration property for the given key.
    *
    * @since 2.0.0
    */
  def getOption(key: String): Option[String]

  /**
    * Resets the configuration property for the given key.
    *
    * @since 2.0.0
    */
  def unset(key: String): Unit

  /**
    * Sets the given Hadoop configuration property. This is passed directly to Hadoop during I/O.
    *
    * @since 2.0.0
    */
  def setHadoop(key: String, value: String): RuntimeConfig

  /**
    * Returns the value of the Hadoop configuration property.
    *
    * @throws NoSuchElementException if the key is not set
    * @since 2.0.0
    */
  @throws[NoSuchElementException]("if the key is not set")
  def getHadoop(key: String): String

  /**
    * Returns the value of the Hadoop configuration property.
    *
    * @since 2.0.0
    */
  def getHadoopOption(key: String): Option[String]

  /**
    * Resets the Hadoop configuration property for the given key.
    *
    * @since 2.0.0
    */
  def unsetHadoop(key: String): Unit
