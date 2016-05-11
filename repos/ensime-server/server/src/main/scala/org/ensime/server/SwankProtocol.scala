// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.util.ByteString
import org.ensime.api._
import org.ensime.server.protocol.swank._
import org.ensime.sexp._

class SwankProtocol extends FramedStringProtocol {
  import SwankFormats._

  override def decode(bytes: ByteString): (Option[RpcRequestEnvelope], ByteString) = {
    tryReadString(bytes) match {
      case (Some(message), remainder) =>
        val parsedMessage = message.parseSexp.convertTo[RpcRequestEnvelope]
        (Some(parsedMessage), remainder)
      case (None, remainder) =>
        (None, remainder)
    }
  }

  override def encode(resp: RpcResponseEnvelope): ByteString = writeString(resp.toSexp.prettyPrint)

}
