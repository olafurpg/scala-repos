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

package scalafx.ensemble.commons

import scalafx.ensemble.stage.{DashboardPage, EnsembleTabbedPage}
import scalafx.scene.Node
import scalafx.scene.layout.{Priority, VBox}

/**
  * the class that updates tabbed view or dashboard view
  * based on the TreeItem selected from left pane
  */
object PageDisplayer

  def choosePage(value: String = "dashBoard"): Node =
    value match
      case "dashBoard" => displayPage(new DashboardPage())
      case _ =>
        if (value.startsWith("dashBoard - "))
          displayPage(new DashboardPage(value.split("-")(1).trim()))
        else
          displayPage(
              EnsembleTabbedPage.buildTab(
                  value.split(">")(1).trim(), value.split(">")(0).trim()))

  private def displayPage(nodeToAdd: DisplayablePage): Node =
    new VBox
      vgrow = Priority.Always
      hgrow = Priority.Always
      children = nodeToAdd.getPage
