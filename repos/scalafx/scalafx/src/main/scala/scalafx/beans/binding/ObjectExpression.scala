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
package scalafx.beans.binding

import javafx.beans.value.ObservableObjectValue
import javafx.beans.{binding => jfxbb}

import scala.language.implicitConversions
import scalafx.beans.value.ObservableValue

object ObjectExpression
  implicit def sfxObjectExpression2jfx[J](
      oe: ObjectExpression[J]): jfxbb.ObjectExpression[J] =
    if (oe != null) oe.delegate else null

class ObjectExpression[J](val delegate: jfxbb.ObjectExpression[J])
  def ===(v: Null) = delegate.isNull
  def ===(v: ObservableObjectValue[_]) = delegate.isEqualTo(v)
  // explicit conversion needed due to AnyRef typed method
  def ===[T](v: ObservableValue[T, T]) =
    delegate.isEqualTo(ObservableValue.sfxObservableValue2jfxObjectValue[T](v))
  def ===(v: AnyRef) = delegate.isEqualTo(v)

  def =!=(v: Null) = delegate.isNotNull
  def =!=(v: ObservableObjectValue[_]) = delegate.isNotEqualTo(v)
  // explicit conversion needed due to AnyRef typed method
  def =!=[T](v: ObservableValue[T, T]) =
    delegate.isNotEqualTo(
        ObservableValue.sfxObservableValue2jfxObjectValue[T](v))
  def =!=(v: AnyRef) = delegate.isNotEqualTo(v)

  def selectDouble(s: String) = jfxbb.Bindings.selectDouble(this.delegate, s)
  def selectBoolean(s: String) = jfxbb.Bindings.selectBoolean(this.delegate, s)
  def selectFloat(s: String) = jfxbb.Bindings.selectFloat(this.delegate, s)
  def selectInteger(s: String) = jfxbb.Bindings.selectInteger(this.delegate, s)
  def selectLong(s: String) = jfxbb.Bindings.selectLong(this.delegate, s)
  def selectString(s: String) = jfxbb.Bindings.selectString(this.delegate, s)
  def select[T](s: String) = jfxbb.Bindings.select[T](this.delegate, s)
