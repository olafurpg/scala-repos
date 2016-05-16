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

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.ensemble.commons.EnsembleExample
import scalafx.scene.chart.BarChart
import scalafx.scene.chart.CategoryAxis
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.XYChart

/** An advanced bar chart.
  *
  * @see scalafx.scene.chart.BarChart
  * @see scalafx.scene.chart.Chart
  * @see scalafx.scene.chart.NumberAxis
  * @see scalafx.scene.chart.XYChart
  */
class EnsembleAdvancedBarChart extends EnsembleExample {

  def getContent = {
    // Category/x values
    val years = Seq("2007", "2008", "2009")

    val xAxis = new CategoryAxis {
      label = "Year"
      categories = ObservableBuffer(years)
    }

    val yAxis = new NumberAxis {
      label = "Price"
      tickLabelFormatter = NumberAxis.DefaultFormatter(this, "$", "")
    }

    //
    // Create data series.
    // To illustrate different possibilities, each series data is created using different approach.
    //

    val series1 = new XYChart.Series[String, Number] {
      name = "Data Series 1"
      // Example of assigning data directly
      data() += XYChart.Data[String, Number](years(0), 567)
      data() += XYChart.Data[String, Number](years(1), 1292)
      data() += XYChart.Data[String, Number](years(2), 2180)
    }

    val series2 = new XYChart.Series[String, Number] {
      name = "Data Series 2"
      // Example of assigning data using a container
      data = ObservableBuffer(
          XYChart.Data[String, Number](years(0), 956),
          XYChart.Data[String, Number](years(1), 1665),
          XYChart.Data[String, Number](years(2), 2450)
      )
    }

    val series3 = new XYChart.Series[String, Number] {
      name = "Data Series 3"
      // Assign data by mapping x and y values to XYChart.Data
      val prices = Seq(800, 1000, 2000)
      data =
        years zip prices map {
          case (x, y) => XYChart.Data[String, Number](x, y)
        }
    }

    // Assign data using a helper function
    def xyData(ys: Seq[Number]) =
      ObservableBuffer(years zip ys map (xy => XYChart.Data(xy._1, xy._2)))
    val series4 = XYChart.Series("Data Series 4", xyData(Seq(786, 2100, 450)))

    // Setup chart
    new BarChart(xAxis, yAxis) {
      barGap = 5
      categoryGap = 12
      title = "Advanced Bar Chart"
      data() ++= Seq(series1, series2, series3, series4)
    }
  }
}
