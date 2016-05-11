// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.util

trait ThreadLocalSupport {
  protected def local[T](t: => T) = new ThreadLocal[T] {
    override def initialValue = t
  }
}
