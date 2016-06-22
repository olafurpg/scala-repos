package org.template.recommendation

import io.prediction.controller.PPreparator

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

class Preparator extends PPreparator[TrainingData, PreparedData] {

  def prepare(sc: SparkContext, trainingData: TrainingData): PreparedData = {
    new PreparedData(items = trainingData.items,
                     ratings = trainingData.ratings)
  }
}

class PreparedData(
    val items: RDD[Item],
    val ratings: RDD[Rating]
) extends Serializable
