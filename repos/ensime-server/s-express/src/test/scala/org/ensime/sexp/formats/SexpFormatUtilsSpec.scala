// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import scala.util._

import org.ensime.sexp._

class SexpFormatUtilsSpec extends FormatSpec with SexpFormats {
  import SexpFormatUtils._

  val foo = SexpString("foo")
  val bar = SexpSymbol("bar")

  "SexpFormatUtils" should "lift writers" in {
    val lifted = lift(new SexpWriter[SexpString] {
      def write(o: SexpString) = o
    })
    foo.toSexp(lifted) should ===(foo)
    intercept[UnsupportedOperationException] {
      foo.convertTo[SexpString](lifted)
    }
  }

  it should "lift readers" in {
    val lifted = lift(new SexpReader[SexpString] {
      def read(o: Sexp) = o.asInstanceOf[SexpString]
    })
    foo.convertTo[SexpString](lifted) should ===(foo)
    intercept[UnsupportedOperationException] {
      foo.toSexp(lifted)
    }
  }

  it should "combine readers and writers" in {
    val reader = new SexpReader[SexpString] {
      def read(o: Sexp) = o.asInstanceOf[SexpString]
    }
    val writer = new SexpWriter[SexpString] {
      def write(o: SexpString) = o
    }
    val combo = sexpFormat(reader, writer)

    foo.convertTo[SexpString](combo) should ===(foo)
    foo.toSexp(combo) should ===(foo)
  }

  it should "support lazy formats" in {
    var init = false
    val lazyF = lazyFormat {
      init = true
      SexpStringFormat
    }

    assert(!init)
    SexpString("foo").convertTo[SexpString](lazyF) should ===(SexpString("foo"))
    assert(init)
    SexpString("foo").toSexp(lazyF) should ===(SexpString("foo"))
  }

  it should "support safe readers" in {
    val safe = safeReader(
      new SexpReader[SexpString] {
        def read(value: Sexp) = value match {
          case s: SexpString => s
          case x => deserializationError(x)
        }
      }
    )

    foo.convertTo[Try[SexpString]](safe) should ===(Success(foo))
    bar.convertTo[Try[SexpString]](safe) shouldBe a[Failure[_]]
  }
}
