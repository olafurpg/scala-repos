/*
 * Copyright (c) 2011-2015, ScalaFX Project
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

package scalafx.colorselector

import scalafx.Includes._
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.beans.property.{BooleanProperty, DoubleProperty, StringProperty}
import scalafx.scene.control.{CheckBox, Label, Slider}
import scalafx.scene.input.ScrollEvent
import scalafx.scene.layout.{HBox, Priority}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, FontWeight}

/**
  * @author Rafael
  *
  */
class SliderControl(title: String) extends HBox {

  private val strBackground = "-fx-background-color: rgb(%d, %d, %d);"
  private val strForeground = "-fx-text-fill: rgb(%d, %d, %d);"

  private val cssBackground = new StringProperty()
  private val cssForeground = new StringProperty()

  val realValue = new DoubleProperty()

  def value = this.realValue

  def value_=(d: Double) {
    if (d < Min) {
      value() = Min
    } else if (d > Max) {
      value() = Max
    } else {
      value() = d
    }
  }

  val selectedControl = new BooleanProperty()

  val chbSelected = new CheckBox {
    id = "chbSelected"
    selected <==> selectedControl
  }

  val lblTitle = new Label {
    id = "lblTitle"
    text = title
    style <== cssForeground
  }
  lblTitle.font =
    Font.font(lblTitle.font().family, FontWeight.Bold, lblTitle.font().size)

  val sldValue = new Slider {
    id = "sldValue"
    blockIncrement = 1.0
    majorTickUnit = 50.0
    max = Max
    min = Min
    minorTickCount = 4
    showTickLabels = true
    showTickMarks = true
    //    style = ".slider .axis {-fx-tick-label-fill: green;}"
    hgrow = Priority.Always
    style <== cssForeground
    value <==> realValue
  }

  val lblValue = new Label {
    id = "lblValue"
    text <== realValue.asString("%03.0f")
    hgrow = Priority.Never
    style <== cssForeground
  }
  lblValue.font =
    Font.font(lblValue.font().family, FontWeight.Bold, lblValue.font().size)

  children = List(chbSelected, lblTitle, sldValue, lblValue)

  padding = insets

  style <== cssBackground

  onScroll = (event: ScrollEvent) => {
    if (event.eventType == ScrollEvent.Scroll) {
      val multiplier = if (event.isControlDown) 10 else 1
      val delta = -(event.getDeltaY.toInt / 10)

      value = (value.get + multiplier * delta)
    }
  }

  def changeColor(backgroundColor: Color, foregroundColor: Color) {
    this.cssBackground() = strBackground.format(
        doubleToInt(backgroundColor.red),
        doubleToInt(backgroundColor.green),
        doubleToInt(backgroundColor.blue))
    this.cssForeground() = strForeground.format(
        doubleToInt(foregroundColor.red),
        doubleToInt(foregroundColor.green),
        doubleToInt(foregroundColor.blue))
  }

  override def toString =
    "%s[%s, %b]".format(title, lblValue.text.get, selectedControl.value)
}
