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

import org.apache.hadoop.fs.Path

import org.apache.spark.annotation.{Experimental, Since}
import org.apache.spark.ml._
import org.apache.spark.ml.param._
import org.apache.spark.ml.param.shared._
import org.apache.spark.ml.util._
import org.apache.spark.mllib.feature
import org.apache.spark.mllib.linalg._
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.{StructField, StructType}

/**
  * Params for [[PCA]] and [[PCAModel]].
  */
private[feature] trait PCAParams
    extends Params
    with HasInputCol
    with HasOutputCol {

  /**
    * The number of principal components.
    * @group param
    */
  final val k: IntParam = new IntParam(
      this, "k", "the number of principal components")

  /** @group getParam */
  def getK: Int = $(k)
}

/**
  * :: Experimental ::
  * PCA trains a model to project vectors to a low-dimensional space using PCA.
  */
@Experimental
class PCA(override val uid: String)
    extends Estimator[PCAModel]
    with PCAParams
    with DefaultParamsWritable {

  def this() = this(Identifiable.randomUID("pca"))

  /** @group setParam */
  def setInputCol(value: String): this.type = set(inputCol, value)

  /** @group setParam */
  def setOutputCol(value: String): this.type = set(outputCol, value)

  /** @group setParam */
  def setK(value: Int): this.type = set(k, value)

  /**
    * Computes a [[PCAModel]] that contains the principal components of the input vectors.
    */
  override def fit(dataset: DataFrame): PCAModel = {
    transformSchema(dataset.schema, logging = true)
    val input =
      dataset.select($(inputCol)).rdd.map { case Row(v: Vector) => v }
    val pca = new feature.PCA(k = $(k))
    val pcaModel = pca.fit(input)
    copyValues(
        new PCAModel(uid, pcaModel.pc, pcaModel.explainedVariance)
          .setParent(this))
  }

  override def transformSchema(schema: StructType): StructType = {
    val inputType = schema($(inputCol)).dataType
    require(inputType.isInstanceOf[VectorUDT],
            s"Input column ${$(inputCol)} must be a vector column")
    require(!schema.fieldNames.contains($(outputCol)),
            s"Output column ${$(outputCol)} already exists.")
    val outputFields =
      schema.fields :+ StructField($(outputCol), new VectorUDT, false)
    StructType(outputFields)
  }

  override def copy(extra: ParamMap): PCA = defaultCopy(extra)
}

@Since("1.6.0")
object PCA extends DefaultParamsReadable[PCA] {

  @Since("1.6.0")
  override def load(path: String): PCA = super.load(path)
}

/**
  * :: Experimental ::
  * Model fitted by [[PCA]].
  *
  * @param pc A principal components Matrix. Each column is one principal component.
  * @param explainedVariance A vector of proportions of variance explained by
  *                          each principal component.
  */
@Experimental
class PCAModel private[ml](override val uid: String,
                           val pc: DenseMatrix,
                           val explainedVariance: DenseVector)
    extends Model[PCAModel]
    with PCAParams
    with MLWritable {

  import PCAModel._

  /** @group setParam */
  def setInputCol(value: String): this.type = set(inputCol, value)

  /** @group setParam */
  def setOutputCol(value: String): this.type = set(outputCol, value)

  /**
    * Transform a vector by computed Principal Components.
    * NOTE: Vectors to be transformed must be the same length
    * as the source vectors given to [[PCA.fit()]].
    */
  override def transform(dataset: DataFrame): DataFrame = {
    transformSchema(dataset.schema, logging = true)
    val pcaModel = new feature.PCAModel($(k), pc, explainedVariance)
    val pcaOp = udf { pcaModel.transform _ }
    dataset.withColumn($(outputCol), pcaOp(col($(inputCol))))
  }

  override def transformSchema(schema: StructType): StructType = {
    val inputType = schema($(inputCol)).dataType
    require(inputType.isInstanceOf[VectorUDT],
            s"Input column ${$(inputCol)} must be a vector column")
    require(!schema.fieldNames.contains($(outputCol)),
            s"Output column ${$(outputCol)} already exists.")
    val outputFields =
      schema.fields :+ StructField($(outputCol), new VectorUDT, false)
    StructType(outputFields)
  }

  override def copy(extra: ParamMap): PCAModel = {
    val copied = new PCAModel(uid, pc, explainedVariance)
    copyValues(copied, extra).setParent(parent)
  }

  @Since("1.6.0")
  override def write: MLWriter = new PCAModelWriter(this)
}

@Since("1.6.0")
object PCAModel extends MLReadable[PCAModel] {

  private[PCAModel] class PCAModelWriter(instance: PCAModel) extends MLWriter {

    private case class Data(pc: DenseMatrix, explainedVariance: DenseVector)

    override protected def saveImpl(path: String): Unit = {
      DefaultParamsWriter.saveMetadata(instance, path, sc)
      val data = Data(instance.pc, instance.explainedVariance)
      val dataPath = new Path(path, "data").toString
      sqlContext
        .createDataFrame(Seq(data))
        .repartition(1)
        .write
        .parquet(dataPath)
    }
  }

  private class PCAModelReader extends MLReader[PCAModel] {

    private val className = classOf[PCAModel].getName

    /**
      * Loads a [[PCAModel]] from data located at the input path. Note that the model includes an
      * `explainedVariance` member that is not recorded by Spark 1.6 and earlier. A model
      * can be loaded from such older data but will have an empty vector for
      * `explainedVariance`.
      *
      * @param path path to serialized model data
      * @return a [[PCAModel]]
      */
    override def load(path: String): PCAModel = {
      val metadata = DefaultParamsReader.loadMetadata(path, sc, className)

      // explainedVariance field is not present in Spark <= 1.6
      val versionRegex = "([0-9]+)\\.([0-9]+).*".r
      val hasExplainedVariance = metadata.sparkVersion match {
        case versionRegex(major, minor) =>
          (major.toInt >= 2 || (major.toInt == 1 && minor.toInt > 6))
        case _ => false
      }

      val dataPath = new Path(path, "data").toString
      val model =
        if (hasExplainedVariance) {
          val Row(pc: DenseMatrix, explainedVariance: DenseVector) =
            sqlContext.read
              .parquet(dataPath)
              .select("pc", "explainedVariance")
              .head()
          new PCAModel(metadata.uid, pc, explainedVariance)
        } else {
          val Row(pc: DenseMatrix) =
            sqlContext.read.parquet(dataPath).select("pc").head()
          new PCAModel(
              metadata.uid,
              pc,
              Vectors.dense(Array.empty[Double]).asInstanceOf[DenseVector])
        }
      DefaultParamsReader.getAndSetParams(model, metadata)
      model
    }
  }

  @Since("1.6.0")
  override def read: MLReader[PCAModel] = new PCAModelReader

  @Since("1.6.0")
  override def load(path: String): PCAModel = super.load(path)
}
