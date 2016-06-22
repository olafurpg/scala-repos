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

package scalafx.controls

import javafx.{geometry => jfxg}

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.controls.controls._
import scalafx.geometry._
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.paint.Color

object SeparatorDemo extends JFXApp {

  val separator = new Separator

  val pnlSeparator = new FlowPane {
    children = List(new Button { text = "Button 1" }, separator, new Button {
      text = "Button 2"
    })
    minHeight = 100
    prefHeight = 100
    minWidth = 100
    prefWidth = 300
  }

  val controlsPane = new VBox {
    spacing = 5
    fillWidth = true
    alignment = Pos.Center
    alignmentInParent = Pos.TopCenter
    hgrow = Priority.Always
    children =
      List(new SeparatorControls(separator), new ControlControls(separator))
  }

  val mainPane = new BorderPane {
    top = pnlSeparator
    center = controlsPane
  }

  stage = new PrimaryStage {
    title = "Tooltip Test"
    width = 300
    height = 600
    scene = new Scene {
      fill = Color.White
      content = mainPane
    }
  }
}

class SeparatorControls(target: Separator)
    extends PropertiesNodes[Separator](target, "Separator Controls") {

  val chbHPos = new ChoiceBox[jfxg.HPos] {
    items = ObservableBuffer(HPos.Center, HPos.Left, HPos.Right)
    value <==> target.halignment
  }

  val chbOrientation = new ChoiceBox[jfxg.Orientation] {
    items = ObservableBuffer(Orientation.Horizontal, Orientation.Vertical)
    value <==> target.orientation
  }

  // NOTE: The type of ChoiceBox is using javafx.geometry.VPos due to current limitations of binding implementation
  val chbVPos = new ChoiceBox[jfxg.VPos] {
    items = ObservableBuffer(VPos.Baseline, VPos.Bottom, VPos.Center, VPos.Top)
    value <==> target.valignment
  }

  super.addNode("HPos", chbHPos)
  super.addNode("VPos", chbVPos)
  super.addNode("Orientation", chbOrientation)
}
