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

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{MenuItem, SplitMenuButton}
import scalafx.scene.layout.VBox

object SplitMenuButtonDemo extends JFXApp

  stage = new PrimaryStage
    scene = new Scene(200, 200)
      content = new VBox
        padding = Insets(10)
        spacing = 10
        children = List(
            new SplitMenuButton
              text = "SplitMenuButton 1"
              onAction =  ae: ActionEvent =>
                { println(ae.eventType + " occurred on SplitMenuButton") }
              items = List(
                  new MenuItem("MenuItem A")
                    onAction =  ae: ActionEvent =>
                      { println(ae.eventType + " occurred on Menu Item A") }
                  ,
                  new MenuItem("MenuItem B")
              )
            ,
            // Use varargs constructor
            new SplitMenuButton(
                new MenuItem("MenuItem C")
                  onAction = (ae: ActionEvent) =>
                    println(ae.eventType + " occurred on Menu Item C")
                ,
                new MenuItem("MenuItem D"),
                new MenuItem("MenuItem E")
            )
        )
