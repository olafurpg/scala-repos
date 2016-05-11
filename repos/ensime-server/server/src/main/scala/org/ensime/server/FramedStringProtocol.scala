// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.event.slf4j.SLF4JLogging
import akka.util.ByteString
import org.ensime.core.Protocol

/**
 * FramedStringProtocol is used to support stream based messaging (e.g. SWANK or JERKY over TCP).
 * Each message consists of a 6 byte h
 */
trait FramedStringProtocol extends Protocol with SLF4JLogging {

  protected def writeString(value: String): ByteString = {
    if (value.isEmpty)
      throw new IllegalStateException("Message to send is empty")

    val data = ByteString(value, "UTF-8")
    val header = ByteString("%06x".format(data.length), "UTF-8")

    val message = header ++ data
    if (log.isTraceEnabled) {
      log.trace(message.utf8String)
    }

    message
  }

  val headerLen = 6

  protected def tryReadString(bytes: ByteString): (Option[String], ByteString) = {
    if (bytes.length < headerLen)
      (None, bytes) // header is incomplete
    else {
      val header = bytes.take(headerLen)
      val msgLen = Integer.valueOf(header.utf8String, 16).intValue()
      if (msgLen == 0)
        throw new IllegalStateException("Empty message read from socket!")

      val totalMessageBytes = headerLen + msgLen
      if (bytes.length < totalMessageBytes)
        (None, bytes) // header is complete, but not all of message is ready
      else {
        // take the header and the message and drop the header
        val (messageBytes, remainingBytes) = bytes.splitAt(totalMessageBytes)
        val msgUTF8 = messageBytes.drop(headerLen).utf8String
        (Some(msgUTF8), remainingBytes)
      }
    }
  }
}
