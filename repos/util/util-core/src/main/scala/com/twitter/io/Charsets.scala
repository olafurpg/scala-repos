package com.twitter.io

import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder, CodingErrorAction}
import java.util

/**
  * Provides a set of frequently used [[java.nio.charset.Charset]] instances and
  * the utilities related to them.
  */
object Charsets

  /**
    * 16-bit UTF (UCS Transformation Format) whose byte order is identified by
    * an optional byte-order mark
    */
  val Utf16: Charset = Charset.forName("UTF-16")

  /**
    * 16-bit UTF (UCS Transformation Format) whose byte order is big-endian
    */
  val Utf16BE: Charset = Charset.forName("UTF-16BE")

  /**
    * 16-bit UTF (UCS Transformation Format) whose byte order is little-endian
    */
  val Utf16LE: Charset = Charset.forName("UTF-16LE")

  /**
    * 8-bit UTF (UCS Transformation Format)
    */
  val Utf8: Charset = Charset.forName("UTF-8")

  /**
    * ISO Latin Alphabet No. 1, also known as `ISO-LATIN-1`
    */
  val Iso8859_1: Charset = Charset.forName("ISO-8859-1")

  /**
    * 7-bit ASCII, also known as ISO646-US or the Basic Latin block of the
    * Unicode character set
    */
  val UsAscii: Charset = Charset.forName("US-ASCII")

  private[this] val encoders =
    new ThreadLocal[util.Map[Charset, CharsetEncoder]]
      protected override def initialValue = new util.HashMap()

  private[this] val decoders =
    new ThreadLocal[util.Map[Charset, CharsetDecoder]]
      protected override def initialValue = new util.HashMap()

  /**
    * Returns a cached thread-local [[java.nio.charset.CharsetEncoder]] for
    * the specified `charset`.
    * @note Malformed and unmappable input is silently replaced
    *       see [[java.nio.charset.CodingErrorAction.REPLACE]]
    */
  def encoder(charset: Charset): CharsetEncoder =
    var e = encoders.get.get(charset)
    if (e == null)
      e = charset.newEncoder()
      encoders.get.put(charset, e)
    e.reset()
    e.onMalformedInput(CodingErrorAction.REPLACE)
    e.onUnmappableCharacter(CodingErrorAction.REPLACE)

  /**
    * Returns a cached thread-local [[java.nio.charset.CharsetDecoder]] for
    * the specified `charset`.
    * @note Malformed and unmappable input is silently replaced
    *       see [[java.nio.charset.CodingErrorAction.REPLACE]]
    */
  def decoder(charset: Charset): CharsetDecoder =
    var d = decoders.get.get(charset)
    if (d == null)
      d = charset.newDecoder()
      decoders.get.put(charset, d)
    d.reset()
    d.onMalformedInput(CodingErrorAction.REPLACE)
    d.onUnmappableCharacter(CodingErrorAction.REPLACE)
