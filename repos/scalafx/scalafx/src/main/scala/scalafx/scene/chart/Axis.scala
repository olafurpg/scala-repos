/*
 * Copyright (c) 2011-2014, ScalaFX Project
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
package scalafx.scene.chart

import javafx.scene.{chart => jfxsc, paint => jfxsp, text => jfxst}
import javafx.{geometry => jfxg}

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import scala.language.implicitConversions
import scalafx.Includes._
import scalafx.beans.binding._
import scalafx.beans.property.{BooleanProperty, DoubleProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.delegate.SFXDelegate
import scalafx.geometry.Side
import scalafx.scene.layout.Region
import scalafx.scene.paint.Paint
import scalafx.scene.text.Font

object Axis {
  implicit def sfxAxis2jfx[T](v: Axis[T]): jfxsc.Axis[T] =
    if (v != null) v.delegate else null

  object TickMark {
    implicit def sfxTickMark2jfx[T](
        v: Axis.TickMark[T]): jfxsc.Axis.TickMark[T] =
      if (v != null) v.delegate else null
  }

  class TickMark[T](
      override val delegate: jfxsc.Axis.TickMark[T] =
        new jfxsc.Axis.TickMark[T]())
      extends SFXDelegate[jfxsc.Axis.TickMark[T]] {

    def label: StringExpression = delegate.labelProperty
    def label_=(value: String) {
      delegate.setLabel(value)
    }

    def position: NumberExpression = delegate.positionProperty
    def position_=(value: Double) {
      delegate.setPosition(value)
    }

    def value: ObjectExpression[T] = delegate.valueProperty
    def value_=(value: T) {
      delegate.setValue(value)
    }

    def textVisible = delegate.isTextVisible
    def textVisible_=(v: Boolean) {
      delegate.setTextVisible(v)
    }
  }
}

abstract class Axis[T](override val delegate: jfxsc.Axis[T])
    extends Region(delegate)
    with SFXDelegate[jfxsc.Axis[T]] {

  def animated: BooleanProperty = delegate.animatedProperty
  def animated_=(v: Boolean) {
    animated() = v
  }

  def autoRanging: BooleanProperty = delegate.autoRangingProperty
  def autoRanging_=(v: Boolean) {
    autoRanging() = v
  }

  def label: ObjectProperty[java.lang.String] = delegate.labelProperty
  def label_=(v: String) {
    label() = v
  }

  def side: ObjectProperty[jfxg.Side] = delegate.sideProperty
  def side_=(v: Side) {
    side() = v
  }

  def tickLabelFill: ObjectProperty[jfxsp.Paint] =
    delegate.tickLabelFillProperty
  def tickLabelFill_=(v: Paint) {
    tickLabelFill() = v
  }

  def tickLabelFont: ObjectProperty[jfxst.Font] =
    delegate.tickLabelFontProperty
  def tickLabelFont_=(v: Font) {
    tickLabelFont() = v
  }

  def tickLabelGap: DoubleProperty = delegate.tickLabelGapProperty
  def tickLabelGap_=(v: Double) {
    tickLabelGap() = v
  }

  def tickLabelRotation: DoubleProperty = delegate.tickLabelRotationProperty
  def tickLabelRotation_=(v: Double) {
    tickLabelRotation() = v
  }

  def tickLabelsVisible: BooleanProperty = delegate.tickLabelsVisibleProperty
  def tickLabelsVisible_=(v: Boolean) {
    tickLabelsVisible() = v
  }

  def tickLength: DoubleProperty = delegate.tickLengthProperty
  def tickLength_=(v: Double) {
    tickLength() = v
  }

  def tickMarkVisible: BooleanProperty = delegate.tickMarkVisibleProperty
  def tickMarkVisible_=(v: Boolean) {
    tickMarkVisible() = v
  }

  def displayPosition(value: T) = delegate.getDisplayPosition(value)

  def tickMarks: ObservableBuffer[jfxsc.Axis.TickMark[T]] =
    delegate.getTickMarks

  def valueForDisplay(displayPosition: Double): T =
    delegate.getValueForDisplay(displayPosition)

  def zeroPosition = delegate.getZeroPosition

  def invalidateRange(data: Buffer[T]) {
    delegate.invalidateRange(data)
  }

  def isValueOnAxis(value: T) = delegate.isValueOnAxis(value)

  def requestAxisLayout() {
    delegate.requestAxisLayout()
  }

  def requestLayout() {
    delegate.requestLayout()
  }

  def toNumericValue(value: T) = delegate.toNumericValue(value)

  def toRealValue(value: Double): T = delegate.toRealValue(value)
}
