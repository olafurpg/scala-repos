// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import shapeless._

import org.ensime.sexp._

/**
 * Helper methods for generating wrappers for types in a family, also
 * known as "type hints".
 *
 * See https://gist.github.com/fommil/3a04661116c899056197
 *
 * Will be replaced by a port of spray-json-shapeless.
 */
trait FamilyFormats {
  case class TypeHint[T](hint: SexpSymbol)
  implicit def typehint[T](implicit t: Typeable[T]): TypeHint[T] =
    TypeHint(SexpSymbol(":" + t.describe.replaceAll("\\.type$", "")))

  // always serialises to Nil, and is differentiated by the TraitFormat
  // scala names https://github.com/milessabin/shapeless/issues/256
  implicit def singletonFormat[T <: Singleton](implicit w: Witness.Aux[T]) = new SexpFormat[T] {
    def write(t: T) = SexpNil
    def read(v: Sexp) =
      if (v == SexpNil) w.value
      else deserializationError(v)
  }

  abstract class TraitFormat[T] extends SexpFormat[T] {
    protected def wrap[E](t: E)(implicit th: TypeHint[E], sf: SexpFormat[E]): Sexp = {
      val contents = t.toSexp
      // special cases: empty case clases, and case objects (hopefully)
      if (contents == SexpNil) SexpList(th.hint)
      else SexpData(th.hint -> contents)
    }

    // implement by matching on the implementations and passing off to wrap
    // def write(t: T): Sexp

    final def read(sexp: Sexp): T = sexp match {
      case SexpList(List(hint @ SexpSymbol(_))) => read(hint, SexpNil)
      case SexpData(map) if map.size == 1 =>
        map.head match {
          case (hint, value) => read(hint, value)
        }

      case x => deserializationError(x)
    }

    // implement by matching on the hint and passing off to convertTo[Impl]
    protected def read(hint: SexpSymbol, value: Sexp): T
  }
}
