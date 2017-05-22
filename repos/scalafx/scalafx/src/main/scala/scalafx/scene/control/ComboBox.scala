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
import javafx.{collections => jfxc, scene => jfxs, util => jfxu}

import scala.language.implicitConversions
import scalafx.Includes._
import scalafx.beans.property.{IntegerProperty, ObjectProperty, ReadOnlyObjectProperty}
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer._
import scalafx.delegate.SFXDelegate
import scalafx.scene.Node
import scalafx.util.StringConverter
import scalafx.util.StringConverter._

object ComboBox
  implicit def sfxComboBox2jfx[T](cb: ComboBox[T]): jfxsc.ComboBox[T] =
    if (cb != null) cb.delegate else null

class ComboBox[T](
    override val delegate: jfxsc.ComboBox[T] = new jfxsc.ComboBox[T])
    extends ComboBoxBase(delegate) with SFXDelegate[jfxsc.ComboBox[T]]

  /**
    * Creates a default ComboBox instance from a [[scalafx.collections.ObservableBuffer]]
    * with the provided items list and a default selection model.
    */
  def this(items: ObservableBuffer[T]) = this(new jfxsc.ComboBox[T](items))

  /**
    * Creates a default ComboBox instance from a [[scala.Seq]]
    * with the provided items list and a default selection model.
    */
  def this(items: Seq[T]) =
    this(new jfxsc.ComboBox[T](ObservableBuffer(items)))

  /**
    * Providing a custom cell factory allows for complete customization of the rendering of items in the ComboBox.
    */
  def cellFactory: ObjectProperty[ListView[T] => ListCell[T]] =
    ObjectProperty(
        (view: ListView[T]) =>
          new ListCell(delegate.cellFactoryProperty.getValue.call(view)))
  def cellFactory_=(f: ListView[T] => ListCell[T])
    delegate.cellFactoryProperty.setValue(
        new jfxu.Callback[jfxsc.ListView[T], jfxsc.ListCell[T]]
      def call(v: jfxsc.ListView[T]): jfxsc.ListCell[T] =
        f(v)
    )

  /**
    * Converts the user-typed input (when the ComboBox is editable) to an object of type T, such that the input may be retrieved via the value property.
    */
  def converter: ObjectProperty[jfxu.StringConverter[T]] =
    delegate.converterProperty
  def converter_=(v: StringConverter[T])
    converter() = v

  /**
    * The list of items to show within the ComboBox popup.
    */
  def items = delegate.itemsProperty
  def items_=(v: ObservableBuffer[T])
    items() = v

  /**
    * This Node is shown to the user when the ComboBox has no content to show.
    */
  def placeholder: ObjectProperty[jfxs.Node] = delegate.placeholderProperty
  def placeholder_=(v: Node)
    ObjectProperty.fillProperty[jfxs.Node](placeholder, v)

  /**
    * The selection model for the ComboBox.
    */
  def selectionModel: ObjectProperty[jfxsc.SingleSelectionModel[T]] =
    delegate.selectionModelProperty
  def selectionModel_=(v: SingleSelectionModel[T])
    selectionModel() = v.delegate

  /**
    * The maximum number of rows to be visible in the ComboBox popup when it is showing.
    */
  def visibleRowCount: IntegerProperty = delegate.visibleRowCountProperty
  def visibleRowCount_=(v: Int)
    visibleRowCount() = v

  /**
    * The button cell is used to render what is shown in the ComboBox 'button' area.
    * If a cell is set here, it does not change the rendering of the ComboBox popup list -
    * that rendering is controlled via the cell factory API.
    *
    * @since 2.2
    */
  def buttonCell: ObjectProperty[jfxsc.ListCell[T]] =
    delegate.buttonCellProperty()
  def buttonCell_=(v: ListCell[T])
    buttonCell() = v

  /**
    * The editor for the ComboBox.
    *
    * @since 2.2
    */
  def editor: ReadOnlyObjectProperty[jfxsc.TextField] =
    delegate.editorProperty()

  /**
    * Append a item at end of list of items
    *
    * @param item Item to be added.
    * @return Combobox itself
    */
  def +=(item: T)
    this.items.get += item

  /**
    * Remove a item in list of items
    *
    * @param item Item to be removed.
    * @return Combobox itself
    */
  def -=(item: T)
    this.items.get -= item
