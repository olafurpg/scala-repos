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

package scalafx.controls.tableview

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.{TableCell, TableColumn, TableView}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

/** Illustrates use of TableColumn CellFactory to do custom rendering of a TableCell. */
object TableWithCustomCellDemo extends JFXApp {

  val characters = ObservableBuffer[Person](
      new Person("Peggy", "Sue", "555-6798", Color.Violet),
      new Person("Rocky", "Raccoon", "555-6798", Color.GreenYellow),
      new Person("Bungalow ", "Bill", "555-9275", Color.DarkSalmon),
  )

  stage = new PrimaryStage {
    title = "TableView with custom color cell"
    scene = new Scene {
      content = new TableView[Person](characters) {
        columns ++= List(
            new TableColumn[Person, String] {
              text = "First Name"
              cellValueFactory = { _.value.firstName }
              prefWidth = 100
            },
            new TableColumn[Person, String]() {
              text = "Last Name"
              cellValueFactory = { _.value.lastName }
              prefWidth = 100
            },
            new TableColumn[Person, Color] {
              text = "Favorite Color"
              cellValueFactory = { _.value.favoriteColor }
              // Render the property value when it changes, including initial assignment
              cellFactory = { _ =>
                new TableCell[Person, Color] {
                  item.onChange { (_, _, newColor) =>
                    graphic = new Circle { fill = newColor; radius = 8 }
                  }
                }
              }
              prefWidth = 100
            },
        )
      }
    }
  }
}
