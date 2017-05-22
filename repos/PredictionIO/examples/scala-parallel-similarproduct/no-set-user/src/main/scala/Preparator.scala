package org.template.similarproduct

import io.prediction.controller.PPreparator

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD

class Preparator extends PPreparator[TrainingData, PreparedData]

  def prepare(sc: SparkContext, trainingData: TrainingData): PreparedData =
    new PreparedData(
        items = trainingData.items, viewEvents = trainingData.viewEvents)

class PreparedData(
    val items: RDD[(String, Item)],
    val viewEvents: RDD[ViewEvent]
)
    extends Serializable
