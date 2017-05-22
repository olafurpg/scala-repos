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
package scalafx.scene.control

import javafx.scene.{control => jfxsc}

import scalafx.delegate.{SFXEnumDelegate, SFXEnumDelegateCompanion}

/** Wrapper for [[scalafx.scene.control.OverrunStyle]] */
object OverrunStyle
    extends SFXEnumDelegateCompanion[jfxsc.OverrunStyle, OverrunStyle]

  val CenterEllipsis = new OverrunStyle(jfxsc.OverrunStyle.CENTER_ELLIPSIS)
  @deprecated(
      "Use CenterEllipsis; CENTER_ELLIPSIS will be removed in a future release",
      "2.2.60")
  val CENTER_ELLIPSIS = CenterEllipsis
  val CenterWordEllipsis = new OverrunStyle(
      jfxsc.OverrunStyle.CENTER_WORD_ELLIPSIS)
  @deprecated(
      "Use CenterWordEllipsis; CENTER_WORD_ELLIPSIS will be removed in a future release",
      "2.2.60")
  val CENTER_WORD_ELLIPSIS = CenterWordEllipsis
  val Clip = new OverrunStyle(jfxsc.OverrunStyle.CLIP)
  @deprecated("Use Clip; CLIP will be removed in a future release", "2.2.60")
  val CLIP = Clip
  val Ellipsis = new OverrunStyle(jfxsc.OverrunStyle.ELLIPSIS)
  @deprecated(
      "Use Ellipsis; ELLIPSIS will be removed in a future release", "2.2.60")
  val ELLIPSIS = Ellipsis
  val LeadingEllipsis = new OverrunStyle(jfxsc.OverrunStyle.LEADING_ELLIPSIS)
  @deprecated(
      "Use LeadingEllipsis; LEADING_ELLIPSIS will be removed in a future release",
      "2.2.60")
  val LEADING_ELLIPSIS = LeadingEllipsis
  val LeadingWordEllipsis = new OverrunStyle(
      jfxsc.OverrunStyle.LEADING_WORD_ELLIPSIS)
  @deprecated(
      "Use LeadingWordEllipsis; LEADING_WORD_ELLIPSIS will be removed in a future release",
      "2.2.60")
  val LEADING_WORD_ELLIPSIS = LeadingWordEllipsis
  val WordEllipsis = new OverrunStyle(jfxsc.OverrunStyle.WORD_ELLIPSIS)
  @deprecated(
      "Use WordEllipsis; WORD_ELLIPSIS will be removed in a future release",
      "2.2.60")
  val WORD_ELLIPSIS = WordEllipsis

  protected override def unsortedValues: Array[OverrunStyle] = Array(
      Clip,
      Ellipsis,
      WordEllipsis,
      CenterEllipsis,
      CenterWordEllipsis,
      LeadingEllipsis,
      LeadingWordEllipsis
  )

sealed case class OverrunStyle(override val delegate: jfxsc.OverrunStyle)
    extends SFXEnumDelegate[jfxsc.OverrunStyle]
