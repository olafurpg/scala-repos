// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.util.ByteString
import org.ensime.api._
import org.ensime.jerk._
import spray.json._

class JerkProtocol extends FramedStringProtocol {
  import JerkEnvelopeFormats._

  override def encode(resp: RpcResponseEnvelope): ByteString = writeString(resp.toJson.compactPrint)

  override def decode(bytes: ByteString): (Option[RpcRequestEnvelope], ByteString) = {
    tryReadString(bytes) match {
      case (Some(message), remainder) =>
        val parsedMessage = message.parseJson.convertTo[RpcRequestEnvelope]
        (Some(parsedMessage), remainder)
      case (None, remainder) =>
        (None, remainder)
    }
  }
}
