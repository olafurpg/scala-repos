// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import java.io.File
import java.net.URI
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone
import java.util.UUID

import org.ensime.sexp._
import org.ensime.sexp.util.ThreadLocalSupport

/**
 * Formats for data types that are so popular that you'd expect them
 * to "just work".
 *
 * Most people might expect `Option[T]` to output `nil` for `None` and
 * the instance for `Some`, but that doesn't round-trip for nested
 * types (think of `Option[List[T]]`). Instead we use a one-element
 * list. If you want to have the non-round-trip behaviour, mix in
 * `OptionAltFormat`.
 */
trait StandardFormats extends ThreadLocalSupport {
  implicit def optionFormat[T: SexpFormat]: SexpFormat[Option[T]] = new SexpFormat[Option[T]] {
    def write(option: Option[T]) = option match {
      case Some(x) => SexpList(x.toSexp)
      case None => SexpNil
    }
    def read(value: Sexp) = value match {
      case SexpNil => None
      case SexpList(s) => Some(s.head.convertTo[T])
      case x => deserializationError(x)
    }
  }

  import scala.util.Success
  import scala.util.Failure
  import SexpFormatUtils._
  implicit def eitherFormat[L: SexpFormat, R: SexpFormat]: SexpFormat[Either[L, R]] =
    new SexpFormat[Either[L, R]] {
      def write(either: Either[L, R]) = either match {
        case Left(b) => b.toSexp
        case Right(a) => a.toSexp
      }
      def read(value: Sexp) = (value.convertTo(safeReader[L]), value.convertTo(safeReader[R])) match {
        case (Success(l), Failure(_)) => Left(l)
        case (Failure(l), Success(r)) => Right(r)
        case (_, _) => deserializationError(value)
      }
    }

  trait ViaString[T] {
    def toSexpString(t: T): String
    def fromSexpString(s: String): T
  }
  def viaString[T](via: ViaString[T]): SexpFormat[T] = new SexpFormat[T] {
    def write(t: T): Sexp = SexpString(via.toSexpString(t))
    def read(v: Sexp): T = v match {
      case SexpString(s) => via.fromSexpString(s)
      case x => deserializationError(x)
    }
  }

  implicit val UuidFormat: SexpFormat[UUID] = viaString(new ViaString[UUID] {
    def toSexpString(uuid: UUID) = uuid.toString
    def fromSexpString(s: String) = UUID.fromString(s)
  })

  // URL is intentionally discouraged in data objects because .equals
  // calls out to the interwebz.
  // implicit val UrlFormat: SexpFormat[URL] = viaString(new ViaString[URL] {
  //   def toSexpString(url: URL) = url.toExternalForm
  //   def fromSexpString(s: String) = new URL(s)
  // })

  implicit val UriFormat: SexpFormat[URI] = viaString(new ViaString[URI] {
    def toSexpString(uri: URI) = uri.toASCIIString
    def fromSexpString(s: String) = new URI(s)
  })

  implicit val FileFormat: SexpFormat[File] = viaString(new ViaString[File] {
    def toSexpString(file: File) = file.getPath
    def fromSexpString(s: String) = new File(s)
  })

  /**
   * Uses ISO_8601 which is well supported on the emacs side (we
   * suspend belief about `Date`'s mutability). If you want to use
   * UNIX epoch time, override with your own implementation.
   */
  implicit val DateFormat: SexpFormat[Date] = viaString(new ViaString[Date] {
    private val localFormatter = local {
      // SimpleDateFormat isn't ISO_8601 compliant on Java 6, for a discussion see
      // http://stackoverflow.com/questions/2201925
      val s = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
      s.setTimeZone(TimeZone.getTimeZone("UTC"))
      s
    }
    def toSexpString(date: Date) = {
      val formatted = localFormatter.get.format(date)
      formatted.substring(0, 22) + ":" + formatted.substring(22)
    }
    def fromSexpString(s: String) = {
      val s1 = s.replace("Z", "+00:00")
      val processed = s1.substring(0, 22) + s1.substring(23)
      localFormatter.get.parse(processed)
    }
  })
}

trait OptionAltFormat {
  this: StandardFormats =>

  override implicit def optionFormat[T: SexpFormat]: SexpFormat[Option[T]] =
    new SexpFormat[Option[T]] {
      def write(option: Option[T]) = option match {
        case Some(x) => x.toSexp
        case None => SexpNil
      }
      def read(value: Sexp) = value match {
        case SexpNil => None
        case x => Some(x.convertTo[T])
      }
    }

}
