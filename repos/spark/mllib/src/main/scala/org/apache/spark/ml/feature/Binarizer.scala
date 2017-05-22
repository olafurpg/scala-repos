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

package org.apache.spark.ml.feature

import scala.collection.mutable.ArrayBuilder

import org.apache.spark.annotation.{Experimental, Since}
import org.apache.spark.ml.Transformer
import org.apache.spark.ml.attribute.BinaryAttribute
import org.apache.spark.ml.param._
import org.apache.spark.ml.param.shared.{HasInputCol, HasOutputCol}
import org.apache.spark.ml.util._
import org.apache.spark.mllib.linalg._
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._

/**
  * :: Experimental ::
  * Binarize a column of continuous features given a threshold.
  */
@Experimental
final class Binarizer(override val uid: String)
    extends Transformer with HasInputCol with HasOutputCol
    with DefaultParamsWritable

  def this() = this(Identifiable.randomUID("binarizer"))

  /**
    * Param for threshold used to binarize continuous features.
    * The features greater than the threshold, will be binarized to 1.0.
    * The features equal to or less than the threshold, will be binarized to 0.0.
    * Default: 0.0
    * @group param
    */
  val threshold: DoubleParam = new DoubleParam(
      this, "threshold", "threshold used to binarize continuous features")

  /** @group getParam */
  def getThreshold: Double = $(threshold)

  /** @group setParam */
  def setThreshold(value: Double): this.type = set(threshold, value)

  setDefault(threshold -> 0.0)

  /** @group setParam */
  def setInputCol(value: String): this.type = set(inputCol, value)

  /** @group setParam */
  def setOutputCol(value: String): this.type = set(outputCol, value)

  override def transform(dataset: DataFrame): DataFrame =
    val outputSchema = transformSchema(dataset.schema, logging = true)
    val schema = dataset.schema
    val inputType = schema($(inputCol)).dataType
    val td = $(threshold)

    val binarizerDouble = udf  in: Double =>
      if (in > td) 1.0 else 0.0
    val binarizerVector = udf  (data: Vector) =>
      val indices = ArrayBuilder.make[Int]
      val values = ArrayBuilder.make[Double]

      data.foreachActive  (index, value) =>
        if (value > td)
          indices += index
          values += 1.0

      Vectors.sparse(data.size, indices.result(), values.result()).compressed

    val metadata = outputSchema($(outputCol)).metadata

    inputType match
      case DoubleType =>
        dataset.select(
            col("*"),
            binarizerDouble(col($(inputCol))).as($(outputCol), metadata))
      case _: VectorUDT =>
        dataset.select(
            col("*"),
            binarizerVector(col($(inputCol))).as($(outputCol), metadata))

  override def transformSchema(schema: StructType): StructType =
    val inputType = schema($(inputCol)).dataType
    val outputColName = $(outputCol)

    val outCol: StructField = inputType match
      case DoubleType =>
        BinaryAttribute.defaultAttr.withName(outputColName).toStructField()
      case _: VectorUDT =>
        new StructField(outputColName, new VectorUDT, true)
      case other =>
        throw new IllegalArgumentException(
            s"Data type $other is not supported.")

    if (schema.fieldNames.contains(outputColName))
      throw new IllegalArgumentException(
          s"Output column $outputColName already exists.")
    StructType(schema.fields :+ outCol)

  override def copy(extra: ParamMap): Binarizer = defaultCopy(extra)

@Since("1.6.0")
object Binarizer extends DefaultParamsReadable[Binarizer]

  @Since("1.6.0")
  override def load(path: String): Binarizer = super.load(path)
