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

import scalafx.delegate._

/** Wrapper for [[http://docs.oracle.com/javase/8/javafx/api/javafx/geometry/HorizontalDirection.html javafx.geometry.HorizontalDirection]] */
object HorizontalDirection
    extends SFXEnumDelegateCompanion[
        jfxg.HorizontalDirection, HorizontalDirection]

  /** A direction to the left. */
  val Left = new HorizontalDirection(jfxg.HorizontalDirection.LEFT)
  @deprecated(
      "Use Left; LEFT will be removed in a future release", "8.0.60-R10")
  val LEFT = Left

  /** A direction to the right. */
  val Right = new HorizontalDirection(jfxg.HorizontalDirection.RIGHT)
  @deprecated(
      "Use Right; RIGHT will be removed in a future release", "8.0.60-R10")
  val RIGHT = Right

  protected override def unsortedValues: Array[HorizontalDirection] =
    Array(Left, Right)

sealed case class HorizontalDirection(
    override val delegate: jfxg.HorizontalDirection)
    extends SFXEnumDelegate[jfxg.HorizontalDirection]
