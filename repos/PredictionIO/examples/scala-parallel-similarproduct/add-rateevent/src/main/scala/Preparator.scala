package org.template.similarproduct

import io.prediction.controller.PPreparator

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

class Preparator extends PPreparator[TrainingData, PreparedData] {

  def prepare(sc: SparkContext, trainingData: TrainingData): PreparedData = {
    new PreparedData(users = trainingData.users,
                     items = trainingData.items,
                     rateEvents = trainingData.rateEvents)
  }
}

class PreparedData(
    val users: RDD[(String, User)],
    val items: RDD[(String, Item)],
    val rateEvents: RDD[RateEvent],
)
    extends Serializable
