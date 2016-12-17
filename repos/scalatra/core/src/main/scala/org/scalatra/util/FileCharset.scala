package org.scalatra.util

import java.io.{File, FileInputStream}
import java.nio.charset.Charset

import grizzled.slf4j.Logger
import org.mozilla.universalchardet.UniversalDetector

import scala.io.Codec

object FileCharset {

  @transient private[this] val logger: Logger = Logger(getClass)

  private val CheckByteLength = 8192

  def apply(file: File): Charset = {
    val buf = Array.ofDim[Byte](CheckByteLength)
    val detector = new UniversalDetector(null)
    try {
      using(new FileInputStream(file)) { fis =>
        var idx = fis.read(buf)
        while (idx > 0 && !detector.isDone) {
          detector.handleData(buf, 0, idx)
          idx = fis.read(buf)
        }
        detector.dataEnd()
      }
      getCharset(detector, Codec.fileEncodingCodec)
    } catch {
      case t: Throwable =>
        logger.warn("Failed to detect charset for file: " + file.getPath + ".",
                    t)
        Codec.defaultCharsetCodec.charSet
    } finally {
      detector.reset()
    }
  }

  private[this] def getCharset(detector: UniversalDetector,
                               default: Codec): Charset = {
    val cs = detector.getDetectedCharset
    if (cs == null || cs.trim().isEmpty) {
      default.charSet
    } else {
      Charset.forName(cs)
    }
  }

  def apply(barr: Array[Byte]): Charset = {
    val detector = new UniversalDetector(null)
    try {
      var idx = 0
      while (idx < barr.length && idx < CheckByteLength && !detector.isDone) {
        if (idx > 0) detector.handleData(barr, 0, idx)
        idx += 1
      }
      detector.dataEnd()
      getCharset(detector, Codec.defaultCharsetCodec)
    } catch {
      case t: Throwable =>
        logger.warn("Failed to detect charset.", t)
        Codec.defaultCharsetCodec.charSet
    } finally {
      detector.reset()
    }
  }
}
