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

package scalafx.ensemble.example.controls

import scalafx.collections.ObservableBuffer
import scalafx.ensemble.commons.EnsembleExample
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.control.ListView
import scalafx.scene.layout.StackPane

/** A sample showing an implementation of the ListView control, in which a list
  * of items is displayed in a horizontal row. ListView is a powerful multirow
  * control, in which each of a virtually unlimited number of horizontal or
  * vertical rows is defined as a cell. The control also supports dynamically
  * variable nonhomogenous row heights.
  *
  * @see scalafx.scene.control.ListView
  * @see scalafx.scene.control.SelectionModel
  * @related controls/SimpleListView
  */
class EnsembleHorizontalListView extends EnsembleExample {

  // @stage-property width = 500

  def getContent = {
    val seq = Seq("Row 1",
                  "Row 2",
                  "Long Row 3",
                  "Row 4",
                  "Row 5",
                  "Row 6",
                  "Row 7",
                  "Row 8",
                  "Row 9",
                  "Row 10",
                  "Row 11",
                  "Row 12",
                  "Row 13",
                  "Row 14",
                  "Row 15",
                  "Row 16",
                  "Row 17",
                  "Row 18",
                  "Row 19",
                  "Row 20")

    val listView = new ListView[String] {
      items = ObservableBuffer(seq)
      orientation = Orientation.HORIZONTAL
    }

    new StackPane {
      padding = Insets(10)
      children = listView
    }
  }
}
