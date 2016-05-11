// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.util.ByteString
import org.ensime.api._
import org.ensime.util.EnsimeSpec

class FramedStringProtocolSpec extends EnsimeSpec {

  // subclassed FramedStringProtocol so we can get access we want to test
  trait Proto extends FramedStringProtocol {
    override def decode(bytes: ByteString): (Option[RpcRequestEnvelope], ByteString) = ???
    override def encode(msg: RpcResponseEnvelope): ByteString = ???
  }

  "FramedStringProtocol" should "write framed strings" in new Proto {
    val buffer = writeString("foobar")
    val written = buffer.utf8String

    written shouldBe "000006foobar"
  }

  it should "write multi-byte UTF-8 strings" in new Proto {
    val buffer = writeString("€")
    val written = buffer.utf8String

    written shouldBe "000003€"
  }

  it should "read framed strings" in new Proto {
    val read = tryReadString(ByteString("000006foobar", "UTF-8"))

    read shouldBe ((Some("foobar"), ByteString()))
  }

  it should "read multi-byte UTF-8 strings" in new Proto {
    val read = tryReadString(ByteString("000003€000003€", "UTF-8"))

    read shouldBe ((Some("€"), ByteString("000003€", "UTF-8")))
  }
}
