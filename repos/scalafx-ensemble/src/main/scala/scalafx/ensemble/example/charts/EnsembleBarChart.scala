/*
 * Copyright (c) 2012-2015, ScalaFX Ensemble Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the ScalaFX Project nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE SCALAFX PROJECT OR ITS CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scalafx.ensemble.example.charts

import scalafx.collections.ObservableBuffer
import scalafx.ensemble.commons.EnsembleExample
import scalafx.scene.chart.BarChart
import scalafx.scene.chart.CategoryAxis
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart

/**
  * A chart that displays rectangular bars with heights indicating data values
  * for categories. Used for displaying information when at least one axis has
  * discontinuous or discrete data.
  *
  * @see scalafx.scene.chart.BarChart
  * @see scalafx.scene.chart.Chart
  * @see scalafx.scene.chart.Axis
  * @see scalafx.scene.chart.CategoryAxis
  * @see scalafx.scene.chart.NumberAxis
  *
  */
class EnsembleBarChart extends EnsembleExample {
  def getContent = {
    val years = ObservableBuffer("2007", "2008", "2009")

    val xAxis = CategoryAxis(years)
    val yAxis = NumberAxis("Units Sold", 0.0d, 3000.0d, 1000.0d)

    def xyData(ys: Seq[Number]) =
      ObservableBuffer(years.zip(ys).map(xy => XYChart.Data(xy._1, xy._2)))

    val series1 = XYChart.Series("Apples", xyData(Seq(567d, 1292d, 1292d)))
    val series2 = XYChart.Series("Lemons", xyData(Seq(956, 1665, 2559)))
    val series3 = XYChart.Series("Oranges", xyData(Seq(1154, 1927, 2774)))

    new BarChart[String, Number](xAxis, yAxis) {
      data = Seq(series1, series2, series3)
      categoryGap = 25.0d
    }
  }
}
