package com.twitter.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import org.apache.commons.codec.binary.Base64

import com.twitter.io.StreamIO

trait StringEncoder
  def encode(bytes: Array[Byte]): String = new String(bytes)
  def decode(str: String): Array[Byte] = str.getBytes

/**
  * A utility for encoding strings and byte arrays to a base64 string, and
  * decoding from strings encoded in base64 to byte arrays.
  *
  * The encoding for strings is UTF-8.
  */
trait Base64StringEncoder extends StringEncoder
  private[this] def codec = new Base64()
  override def encode(bytes: Array[Byte]): String = codec.encodeToString(bytes)
  override def decode(str: String): Array[Byte] = codec.decode(str)

/**
  * A utility for encoding strings and byte arrays to a URL-safe base64 string,
  * and decoding from strings encoded in base64 to byte arrays.
  *
  * The encoding for strings is UTF-8.
  */
trait Base64UrlSafeStringEncoder extends StringEncoder
  // This uses a null line separator since maximum line length (0) is disabled.
  private[this] def codec = new Base64(0, null, true)

  override def encode(bytes: Array[Byte]): String = codec.encodeToString(bytes)
  override def decode(str: String): Array[Byte] = codec.decode(str)

object StringEncoder extends StringEncoder
object Base64StringEncoder extends Base64StringEncoder
object Base64UrlSafeStringEncoder extends Base64UrlSafeStringEncoder

/**
  * A collection of utilities for encoding strings and byte arrays to and
  * decoding from strings compressed from with gzip.
  *
  * This trait is thread-safe because there are no streams shared outside of
  * method scope, and therefore no contention for shared byte arrays.
  *
  * The encoding for strings is UTF-8.
  *
  * gzipping inherently includes base64 encoding (the GZIP utilities from java
  * will complain otherwise!)
  */
trait GZIPStringEncoder extends StringEncoder
  override def encode(bytes: Array[Byte]): String =
    val baos = new ByteArrayOutputStream
    val gos = new GZIPOutputStream(baos)
    try
      gos.write(bytes)
    finally
      gos.close()
    Base64StringEncoder.encode(baos.toByteArray)

  def encodeString(str: String) = encode(str.getBytes("UTF-8"))

  override def decode(str: String): Array[Byte] =
    val baos = new ByteArrayOutputStream
    val gis = new GZIPInputStream(
        new ByteArrayInputStream(Base64StringEncoder.decode(str)))
    try
      StreamIO.copy(gis, baos)
    finally
      gis.close()

    baos.toByteArray

  def decodeString(str: String): String = new String(decode(str), "UTF-8")

object GZIPStringEncoder extends GZIPStringEncoder
