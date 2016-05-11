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

package scalafx.geometry

import javafx.{geometry => jfxg}

import scalafx.delegate.{SFXEnumDelegate, SFXEnumDelegateCompanion}
import scalafx.geometry.GeometryIncludes.{jfxHPos2sfx, jfxVPos2sfx}

/** Wrapper for [[http://docs.oracle.com/javase/8/javafx/api/javafx/geometry/Pos.html javafx.geometry.Pos]] */
object Pos extends SFXEnumDelegateCompanion[jfxg.Pos, Pos] {

  val BaselineCenter = new Pos(jfxg.Pos.BASELINE_CENTER)
  @deprecated(
      "Use BaselineCenter; BASELINE_CENTER will be removed in a future release",
      "2.2.60")
  val BASELINE_CENTER = BaselineCenter
  val BaselineLeft = new Pos(jfxg.Pos.BASELINE_LEFT)
  @deprecated(
      "Use BaselineLeft; BASELINE_LEFT will be removed in a future release",
      "2.2.60")
  val BASELINE_LEFT = BaselineLeft
  val BaselineRight = new Pos(jfxg.Pos.BASELINE_RIGHT)
  @deprecated(
      "Use BaselineRight; BASELINE_RIGHT will be removed in a future release",
      "2.2.60")
  val BASELINE_RIGHT = BaselineRight
  val BottomCenter = new Pos(jfxg.Pos.BOTTOM_CENTER)
  @deprecated(
      "Use BottomCenter; BOTTOM_CENTER will be removed in a future release",
      "2.2.60")
  val BOTTOM_CENTER = BottomCenter
  val BottomLeft = new Pos(jfxg.Pos.BOTTOM_LEFT)
  @deprecated(
      "Use BottomLeft; BOTTOM_LEFT will be removed in a future release",
      "2.2.60")
  val BOTTOM_LEFT = BottomLeft
  val BottomRight = new Pos(jfxg.Pos.BOTTOM_RIGHT)
  @deprecated(
      "Use BottomRight; BOTTOM_RIGHT will be removed in a future release",
      "2.2.60")
  val BOTTOM_RIGHT = BottomRight
  val Center = new Pos(jfxg.Pos.CENTER)
  @deprecated(
      "Use Center; CENTER will be removed in a future release", "2.2.60")
  val CENTER = Center
  val CenterLeft = new Pos(jfxg.Pos.CENTER_LEFT)
  @deprecated(
      "Use CenterLeft; CENTER_LEFT will be removed in a future release",
      "2.2.60")
  val CENTER_LEFT = CenterLeft
  val CenterRight = new Pos(jfxg.Pos.CENTER_RIGHT)
  @deprecated(
      "Use CenterRight; CENTER_RIGHT will be removed in a future release",
      "2.2.60")
  val CENTER_RIGHT = CenterRight
  val TopCenter = new Pos(jfxg.Pos.TOP_CENTER)
  @deprecated("Use TopCenter; TOP_CENTER will be removed in a future release",
              "2.2.60")
  val TOP_CENTER = TopCenter
  val TopLeft = new Pos(jfxg.Pos.TOP_LEFT)
  @deprecated(
      "Use TopLeft; TOP_LEFT will be removed in a future release", "2.2.60")
  val TOP_LEFT = TopLeft
  val TopRight = new Pos(jfxg.Pos.TOP_RIGHT)
  @deprecated(
      "Use TopRight; TOP_RIGHT will be removed in a future release", "2.2.60")
  val TOP_RIGHT = TopRight

  protected override def unsortedValues: Array[Pos] = Array(
      TopLeft,
      TopCenter,
      TopRight,
      CenterLeft,
      Center,
      CenterRight,
      BottomLeft,
      BottomCenter,
      BottomRight,
      BaselineLeft,
      BaselineCenter,
      BaselineRight
  )
}

sealed case class Pos(override val delegate: jfxg.Pos)
    extends SFXEnumDelegate[jfxg.Pos] {

  /** Returns the horizontal positioning/alignment. */
  def hpos: HPos = delegate.getHpos

  /** Returns the vertical positioning/alignment. **/
  def vpos: VPos = delegate.getVpos
}
