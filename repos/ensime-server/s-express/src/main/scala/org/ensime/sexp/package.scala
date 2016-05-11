// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime

package sexp {
  class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)

}

package object sexp {
  implicit class EnrichedAny[T](val any: T) extends AnyVal {
    def toSexp(implicit writer: SexpWriter[T]): Sexp = writer.write(any)
  }

  implicit class EnrichedString(val string: String) extends AnyVal {
    def parseSexp: Sexp = SexpParser.parse(string)
  }
}
