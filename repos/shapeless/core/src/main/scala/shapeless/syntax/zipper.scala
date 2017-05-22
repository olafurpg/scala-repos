/*
 * Copyright (c) 2012-15 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless
package syntax

object zipper
  implicit def toZipper[L <: HList](l: L) = new HListZipperOps(l)
  implicit def toZipper[C, CL <: HList](c: C)(
      implicit gen: Generic.Aux[C, CL]) = new GenericZipperOps(c)

/** Enhances values of any type with a representation via `Generic` with a method supporting conversion to a `Zipper`. */
class GenericZipperOps[C, CL <: HList](c: C)(implicit gen: Generic.Aux[C, CL])
    extends Serializable
  def toZipper = Zipper(c)

class HListZipperOps[L <: HList](val l: L) extends AnyVal with Serializable
  def toZipper = Zipper(l)
