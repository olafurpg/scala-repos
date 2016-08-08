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

package org.apache.spark.sql.sources

import org.apache.spark.SparkFunSuite
import org.apache.spark.sql.execution.datasources.DataSource

class ResolvedDataSourceSuite extends SparkFunSuite {
  private def getProvidingClass(name: String): Class[_] =
    DataSource(sqlContext = null, className = name).providingClass

  test("jdbc") {
    assert(
        getProvidingClass("jdbc") === classOf[
            org.apache.spark.sql.execution.datasources.jdbc.DefaultSource])
    assert(
        getProvidingClass("org.apache.spark.sql.execution.datasources.jdbc") === classOf[
            org.apache.spark.sql.execution.datasources.jdbc.DefaultSource])
    assert(
        getProvidingClass("org.apache.spark.sql.jdbc") === classOf[
            org.apache.spark.sql.execution.datasources.jdbc.DefaultSource])
  }

  test("json") {
    assert(
        getProvidingClass("json") === classOf[
            org.apache.spark.sql.execution.datasources.json.DefaultSource])
    assert(
        getProvidingClass("org.apache.spark.sql.execution.datasources.json") === classOf[
            org.apache.spark.sql.execution.datasources.json.DefaultSource])
    assert(
        getProvidingClass("org.apache.spark.sql.json") === classOf[
            org.apache.spark.sql.execution.datasources.json.DefaultSource])
  }

  test("parquet") {
    assert(
        getProvidingClass("parquet") === classOf[
            org.apache.spark.sql.execution.datasources.parquet.DefaultSource])
    assert(getProvidingClass(
        "org.apache.spark.sql.execution.datasources.parquet") === classOf[
        org.apache.spark.sql.execution.datasources.parquet.DefaultSource])
    assert(
        getProvidingClass("org.apache.spark.sql.parquet") === classOf[
            org.apache.spark.sql.execution.datasources.parquet.DefaultSource])
  }

  test("error message for unknown data sources") {
    val error1 = intercept[ClassNotFoundException] {
      getProvidingClass("avro")
    }
    assert(error1.getMessage.contains("spark-packages"))

    val error2 = intercept[ClassNotFoundException] {
      getProvidingClass("com.databricks.spark.avro")
    }
    assert(error2.getMessage.contains("spark-packages"))

    val error3 = intercept[ClassNotFoundException] {
      getProvidingClass("asfdwefasdfasdf")
    }
    assert(error3.getMessage.contains("spark-packages"))
  }
}
