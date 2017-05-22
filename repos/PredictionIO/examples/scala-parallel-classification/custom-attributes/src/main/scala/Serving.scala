package com.test1

import io.prediction.controller.LServing

class Serving extends LServing[Query, PredictedResult]

  override def serve(
      query: Query,
      predictedResults: Seq[PredictedResult]): PredictedResult =
    predictedResults.head
