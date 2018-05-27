// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server.protocol.swank

import org.ensime.sexp._
import org.ensime.util.EnsimeSpec

// copied from S-Express to avoid a dependency on sexp:test
trait FormatSpec extends EnsimeSpec {
  def assertFormat[T: SexpFormat](start: T, expect: Sexp): Unit = {
    start.toSexp should ===(expect)
    expect.convertTo[T] should ===(start)
  }
}
