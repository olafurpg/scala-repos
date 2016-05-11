/*
 *
 *
 *
 *
 * Scaloid: Simpler Android
 *
 * http://scaloid.org
 *
 *
 *
 *
 *
 *
 * Copyright 2013 Sung-Ho Lee and Scaloid contributors
 *
 * Sung-Ho Lee and Scaloid contributors licenses this file to you under the Apache License,
 * version 2.0 (the "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at:
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 */

/*
 * This file is automatically generated. Any changes on this file will be OVERWRITTEN!
 * To learn how to contribute, please refer to:
 * https://github.com/pocorall/scaloid/wiki/Inside-Scaloid
 */

package org.scaloid.support.v4

import org.scaloid.common._

/**
  * Automatically generated enriching class of `[[https://developer.android.com/reference/android/support/v4/widget/ResourceCursorAdapter.html android.support.v4.widget.ResourceCursorAdapter]]`.
  */
class RichResourceCursorAdapter[
    This <: android.support.v4.widget.ResourceCursorAdapter](val basis: This)
    extends TraitResourceCursorAdapter[This]

/**
  * Automatically generated helper trait of `[[https://developer.android.com/reference/android/support/v4/widget/ResourceCursorAdapter.html android.support.v4.widget.ResourceCursorAdapter]]`. This contains several property accessors.
  */
trait TraitResourceCursorAdapter[
    This <: android.support.v4.widget.ResourceCursorAdapter]
    extends TraitCursorAdapter[This] {

  @inline
  def dropDownViewResource(implicit no: NoGetterForThisProperty): Nothing =
    throw new Error(
        "Android does not support the getter for 'dropDownViewResource'")

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/ResourceCursorAdapter.html#setDropDownViewResource(int) setDropDownViewResource(int)]]`
    */
  @inline def dropDownViewResource(p: Int) = dropDownViewResource_=(p)

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/ResourceCursorAdapter.html#setDropDownViewResource(int) setDropDownViewResource(int)]]`
    */
  @inline def dropDownViewResource_=(p: Int) = {
    basis.setDropDownViewResource(p); basis
  }

  @inline def viewResource(implicit no: NoGetterForThisProperty): Nothing =
    throw new Error("Android does not support the getter for 'viewResource'")

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/ResourceCursorAdapter.html#setViewResource(int) setViewResource(int)]]`
    */
  @inline def viewResource(p: Int) = viewResource_=(p)

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/ResourceCursorAdapter.html#setViewResource(int) setViewResource(int)]]`
    */
  @inline def viewResource_=(p: Int) = { basis.setViewResource(p); basis }
}

/**
  * Automatically generated enriching class of `[[https://developer.android.com/reference/android/support/v4/widget/CursorAdapter.html android.support.v4.widget.CursorAdapter]]`.
  */
class RichCursorAdapter[This <: android.support.v4.widget.CursorAdapter](
    val basis: This)
    extends TraitCursorAdapter[This]

/**
  * Automatically generated helper trait of `[[https://developer.android.com/reference/android/support/v4/widget/CursorAdapter.html android.support.v4.widget.CursorAdapter]]`. This contains several property accessors.
  */
trait TraitCursorAdapter[This <: android.support.v4.widget.CursorAdapter]
    extends TraitBaseAdapter[This] {

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/CursorAdapter.html#getCursor() getCursor()]]`
    */
  @inline def cursor = basis.getCursor

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/CursorAdapter.html#getFilter() getFilter()]]`
    */
  @inline def filter = basis.getFilter

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/CursorAdapter.html#getFilterQueryProvider() getFilterQueryProvider()]]`
    */
  @inline def filterQueryProvider = basis.getFilterQueryProvider

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/CursorAdapter.html#setFilterQueryProvider(android.widget.FilterQueryProvider) setFilterQueryProvider(android.widget.FilterQueryProvider)]]`
    */
  @inline def filterQueryProvider(p: android.widget.FilterQueryProvider) =
    filterQueryProvider_=(p)

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/CursorAdapter.html#setFilterQueryProvider(android.widget.FilterQueryProvider) setFilterQueryProvider(android.widget.FilterQueryProvider)]]`
    */
  @inline def filterQueryProvider_=(p: android.widget.FilterQueryProvider) = {
    basis.setFilterQueryProvider(p); basis
  }
}

/**
  * Automatically generated enriching class of `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html android.support.v4.widget.SimpleCursorAdapter]]`.
  */
class RichSimpleCursorAdapter[
    This <: android.support.v4.widget.SimpleCursorAdapter](val basis: This)
    extends TraitSimpleCursorAdapter[This]

/**
  * Automatically generated helper trait of `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html android.support.v4.widget.SimpleCursorAdapter]]`. This contains several property accessors.
  */
trait TraitSimpleCursorAdapter[
    This <: android.support.v4.widget.SimpleCursorAdapter]
    extends TraitResourceCursorAdapter[This] {

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#getCursorToStringConverter() getCursorToStringConverter()]]`
    */
  @inline def cursorToStringConverter = basis.getCursorToStringConverter

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#setCursorToStringConverter(android.support.v4.widget.SimpleCursorAdapter.CursorToStringConverter) setCursorToStringConverter(android.support.v4.widget.SimpleCursorAdapter.CursorToStringConverter)]]`
    */
  @inline
  def cursorToStringConverter(
      p: android.support.v4.widget.SimpleCursorAdapter.CursorToStringConverter) =
    cursorToStringConverter_=(p)

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#setCursorToStringConverter(android.support.v4.widget.SimpleCursorAdapter.CursorToStringConverter) setCursorToStringConverter(android.support.v4.widget.SimpleCursorAdapter.CursorToStringConverter)]]`
    */
  @inline
  def cursorToStringConverter_=(
      p: android.support.v4.widget.SimpleCursorAdapter.CursorToStringConverter) = {
    basis.setCursorToStringConverter(p); basis
  }

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#getStringConversionColumn() getStringConversionColumn()]]`
    */
  @inline def stringConversionColumn = basis.getStringConversionColumn

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#setStringConversionColumn(int) setStringConversionColumn(int)]]`
    */
  @inline def stringConversionColumn(p: Int) = stringConversionColumn_=(p)

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#setStringConversionColumn(int) setStringConversionColumn(int)]]`
    */
  @inline def stringConversionColumn_=(p: Int) = {
    basis.setStringConversionColumn(p); basis
  }

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#getViewBinder() getViewBinder()]]`
    */
  @inline def viewBinder = basis.getViewBinder

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#setViewBinder(android.support.v4.widget.SimpleCursorAdapter.ViewBinder) setViewBinder(android.support.v4.widget.SimpleCursorAdapter.ViewBinder)]]`
    */
  @inline
  def viewBinder(p: android.support.v4.widget.SimpleCursorAdapter.ViewBinder) =
    viewBinder_=(p)

  /**
    * Shortcut for `[[https://developer.android.com/reference/android/support/v4/widget/SimpleCursorAdapter.html#setViewBinder(android.support.v4.widget.SimpleCursorAdapter.ViewBinder) setViewBinder(android.support.v4.widget.SimpleCursorAdapter.ViewBinder)]]`
    */
  @inline
  def viewBinder_=(
      p: android.support.v4.widget.SimpleCursorAdapter.ViewBinder) = {
    basis.setViewBinder(p); basis
  }
}

trait WidgetImplicits {
  import scala.language.implicitConversions

  @inline implicit def resourceCursorAdapter2RichResourceCursorAdapter[
      V <: android.support.v4.widget.ResourceCursorAdapter](
      resourceCursorAdapter: V) =
    new RichResourceCursorAdapter[V](resourceCursorAdapter)
  @inline implicit def cursorAdapter2RichCursorAdapter[
      V <: android.support.v4.widget.CursorAdapter](cursorAdapter: V) =
    new RichCursorAdapter[V](cursorAdapter)
  @inline implicit def simpleCursorAdapter2RichSimpleCursorAdapter[
      V <: android.support.v4.widget.SimpleCursorAdapter](
      simpleCursorAdapter: V) =
    new RichSimpleCursorAdapter[V](simpleCursorAdapter)
}
object WidgetImplicits extends WidgetImplicits
