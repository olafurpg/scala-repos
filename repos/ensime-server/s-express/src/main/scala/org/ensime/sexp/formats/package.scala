// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp

package object formats {
  def deserializationError(got: Sexp) =
    throw new DeserializationException(s"Unable to parse $got")

  def serializationError(msg: String) = throw new SerializationException(msg)
}
