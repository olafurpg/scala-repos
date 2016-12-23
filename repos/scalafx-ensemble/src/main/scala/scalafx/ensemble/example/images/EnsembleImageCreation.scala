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

package scalafx.ensemble.example.images

import scalafx.ensemble.commons.EnsembleExample
import scalafx.geometry.Insets
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.{Priority, VBox}

/**
  * A sample that demonstrates the use of two different constructors in the Image
  * class.
  *
  * @related images/ImageProperties
  * @see scalafx.scene.image.Image
  * @see scalafx.scene.image.ImageView
  * @resource /scalafx/ensemble/images/icon-48x48.png
  * @resource /scalafx/ensemble/images/scala-logo.png
  */
class EnsembleImageCreation extends EnsembleExample {
  def getContent = new VBox {
    vgrow = Priority.Always
    hgrow = Priority.Always
    spacing = 10
    padding = Insets(20)
    children = List(
      new ImageView {
        image = new Image(
          this.getClass
            .getResourceAsStream("/scalafx/ensemble/images/icon-48x48.png"))
        fitHeight = 50
        fitWidth = 150
        preserveRatio = true
        smooth = true
      },
      new ImageView {
        // image = new Image("/scalafx/ensemble/images/scala-logo.png")
        image =
          new Image("http://www.scala-lang.org/resources/img/scala-logo.png")
      }
    )
  }
}
