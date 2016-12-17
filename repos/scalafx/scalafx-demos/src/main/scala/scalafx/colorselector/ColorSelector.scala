/*
 * Copyright (c) 2011-2016, ScalaFX Project
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

import scala.collection.Seq
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.{DoubleProperty, ObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer.{Add, Change, Remove}
import scalafx.colorselector
import scalafx.event.ActionEvent
import scalafx.geometry.{HPos, Pos, VPos}
import scalafx.scene.Scene
import scalafx.scene.control.{CheckBox, ComboBox, Label, TextField}
import scalafx.scene.effect.Reflection
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text.TextAlignment
import scalafx.util.StringConverter

object ColorSelector extends JFXApp {

  // VAL'S DEFINITION - BEGIN

  lazy val allControls =
    List(controlRed, controlGreen, controlBlue, controlAlpha)

  val currentColor = ObjectProperty(this, "Color", Color.White)
  currentColor.onChange(colorChanged())

  val synchronizedValue = new DoubleProperty()

  val synchronizedControls = new ObservableBuffer[SliderControl]
  synchronizedControls.onChange((buffer, changes) =>
    synchronizeValues(buffer, changes))

  // METHODS - BEGIN

  private def controlSelected(control: SliderControl) {
    if (control.selectedControl.value) {
      synchronizedControls.add(control)
    } else {
      synchronizedControls.remove(control)
    }
  }

  private def changeColor() {
    val newAlphaValue =
      if (controlAlpha.disabled.value) 1.0
      else (controlAlpha.value.toDouble / colorselector.Max)

    this.currentColor() = Color.rgb(controlRed.value.toInt,
                                    controlGreen.value.toInt,
                                    controlBlue.value.toInt,
                                    newAlphaValue)
  }

  private def synchronizeValues(buffer: ObservableBuffer[SliderControl],
                                changes: Seq[Change[SliderControl]]) {
    changes.head match {
      case Add(pos, added) =>
        val media = buffer.map(_.value.value).sum / buffer.size
        added.last.value <==> synchronizedValue
        buffer.foreach(_.value = media)
      case Remove(pos, removed) =>
        removed.last.value unbind synchronizedValue
      case _ @otherChange =>
        throw new UnsupportedOperationException(
          "Only add and remove defined for the ColorSelector SliderControl sync")
    }
  }

  private def randomizeColors() {
    if (synchronizedControls.nonEmpty) {
      this.synchronizedValue() = math.random * colorselector.Max
    }
    if (synchronizedControls.size < 4) {
      this.allControls
        .filterNot(_.selectedControl.value)
        .filterNot(_.disabled.value)
        .foreach(_.value() = math.random * colorselector.Max)
    }
  }

  private def colorChanged() {
    formatColor()
    verifyWebColor()
  }

  private def formatColor() {
    this.txfColorValue.text() = this.cmbColorFormat.value.value
      .format(this.currentColor(), !this.chbDisableAlpha.selected.value)
  }

  private def getForegroundColor(d: Double) =
    if (d > Max / 2) Color.Black else Color.White

  private def verifyWebColor() {
    cmbWebColor.value() =
      WebColor.colors.find(_.sameColor(currentColor())).orNull
  }

  private def webColorSelected() {
    if (this.cmbWebColor.value.value != null) {
      val color = this.cmbWebColor.value.value.color
      controlRed.value() = doubleToInt(color.red)
      controlGreen.value() = doubleToInt(color.green)
      controlBlue.value() = doubleToInt(color.blue)
    }
  }

  // METHODS - END

  val rectangleRegion = new Region {
    effect = new Reflection {
      fraction = 0.45
    }
    onMouseClicked = (event: MouseEvent) => {
      if ((event.getClickCount == 2) && (event.button == MouseButton.Primary)) {
        randomizeColors()
      }
    }
  }

  currentColor.onChange(
    rectangleRegion.setStyle("-fx-background-color: " + RgbFormatter
      .format(currentColor(), !this.chbDisableAlpha.selected.value)))

  val controlRed = new SliderControl("R") {
    value = 255
  }
  controlRed.value.onChange({
    changeColor()
    controlRed.changeColor(Color.rgb(controlRed.value.value.toInt, 0, 0),
                           getForegroundColor(controlRed.value.value))
  })
  controlRed.selectedControl.onChange(controlSelected(controlRed))
  controlRed.changeColor(Color.rgb(controlRed.value.value.toInt, 0, 0),
                         getForegroundColor(controlRed.value.value))

  val controlGreen = new SliderControl("G") {
    value = 255
  }
  controlGreen.value.onChange({
    changeColor()
    controlGreen.changeColor(Color.rgb(0, controlGreen.value.value.toInt, 0),
                             getForegroundColor(controlGreen.value.value))
  })
  controlGreen.selectedControl.onChange(controlSelected(controlGreen))
  controlGreen.changeColor(Color.rgb(0, controlGreen.value.value.toInt, 0),
                           getForegroundColor(controlGreen.value.value))

  val controlBlue = new SliderControl("B") {
    value = 255
  }
  controlBlue.value.onChange({
    changeColor()
    controlBlue.changeColor(Color.rgb(0, 0, controlBlue.value.value.toInt),
                            Color.White)
  })
  controlBlue.selectedControl.onChange(controlSelected(controlBlue))
  controlBlue.changeColor(Color.rgb(0, 0, controlBlue.value.value.toInt),
                          Color.White)

  val controlAlpha = new SliderControl("A") {
    value = 255
  }
  controlAlpha.value.onChange(changeColor())
  controlAlpha.selectedControl.onChange(controlSelected(controlAlpha))
  controlAlpha.disable.onChange({
    if (controlAlpha.selectedControl.value) {
      if (controlAlpha.disable.value) synchronizedControls.remove(controlAlpha)
      else synchronizedControls.add(controlAlpha)
    }
    changeColor()
    formatColor()
  })

  val cmbWebColor = new ComboBox[WebColor](WebColor.colors) {
    promptText = "Web Color"
    converter = StringConverter.toStringConverter((wc: WebColor) => wc.name)
    onAction = (event: ActionEvent) => webColorSelected()
  }

  val txfColorValue = new TextField {
    promptText = "Color Value"
    editable = false
    alignment = Pos.CenterLeft
    hgrow = Priority.Never
    style = "-fx-font-family: Consolas;"
  }

  val cmbColorFormat = new ComboBox[Formatter](Formatter.formatters) {
    promptText = "Color Format"
    converter =
      StringConverter.toStringConverter((f: Formatter) => f.description)
    value = RgbFormatter
    onAction = (event: ActionEvent) => formatColor()
  }

  val chbDisableAlpha = new CheckBox {
    selected <==> controlAlpha.disable
  }

  val rectangleRowsConstraint = new RowConstraints {
    vgrow = Priority.Always
    prefHeight = Region.USE_COMPUTED_SIZE
  }
  val otherRowsConstraint = new RowConstraints {
    vgrow = Priority.Never
    valignment = VPos.Top
  }
  val column0Constraint = new ColumnConstraints {
    fillWidth = true
    halignment = HPos.Center
    hgrow = Priority.Always
    minWidth = 300
  }
  val column1Constraint = new ColumnConstraints {
    halignment = HPos.Right
    hgrow = Priority.Never
    minWidth = 80
    maxWidth = 100
  }
  val column2Constraint = new ColumnConstraints {
    halignment = HPos.Left
    hgrow = Priority.Sometimes
    minWidth = 200
  }
  val pnlMain = new GridPane {
    hgap = 5.0
    vgap = 5.0
    rowConstraints = List(rectangleRowsConstraint,
                          otherRowsConstraint,
                          otherRowsConstraint,
                          otherRowsConstraint,
                          otherRowsConstraint)
    columnConstraints = List(column0Constraint, column1Constraint)
    padding = colorselector.insets

    add(rectangleRegion, 0, 0, 3, 1)

    add(controlRed, 0, 1)
    add(new Label {
      alignmentInParent = Pos.TopRight
      labelFor = cmbWebColor
      text = "Web Color"
      textAlignment = TextAlignment.Right
      wrapText = true
    }, 1, 1)
    add(cmbWebColor, 2, 1)

    add(controlGreen, 0, 2)
    add(new Label {
      alignmentInParent = Pos.TopRight
      labelFor = txfColorValue
      text = "Color Value"
      textAlignment = TextAlignment.Right
      wrapText = true
    }, 1, 2)
    add(txfColorValue, 2, 2)

    add(controlBlue, 0, 3)
    add(new Label {
      alignmentInParent = Pos.TopRight
      labelFor = cmbColorFormat
      text = "Color Format"
      textAlignment = TextAlignment.Right
      wrapText = true
    }, 1, 3)
    add(cmbColorFormat, 2, 3)

    add(controlAlpha, 0, 4)
    add(new Label {
      alignmentInParent = Pos.TopRight
      labelFor = chbDisableAlpha
      text = "Disable Alpha"
      textAlignment = TextAlignment.Right
      wrapText = true
    }, 1, 4)
    add(chbDisableAlpha, 2, 4)
  }

  val pnlMain0 = new AnchorPane {
    children = List(pnlMain)
    prefHeight = 500
    prefWidth = 800
  }
  AnchorPane.setAnchors(pnlMain, 0, 0, 0, 0)

  val mainScene = new Scene {
    content = List(pnlMain0)
  }
  pnlMain0.prefWidth <== mainScene.width
  pnlMain0.prefHeight <== mainScene.height

  stage = new PrimaryStage {
    title = "Color Selector"
    width = 600
    height = 400
    scene = mainScene
  }

  // Initialization
  changeColor()
  chbDisableAlpha.selected = true
  formatColor()
  webColorSelected()
}
