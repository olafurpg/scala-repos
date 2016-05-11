// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import java.io.{ InputStreamReader, InputStream }

import akka.actor.ActorRef
import org.ensime.api.DebugOutputEvent
import org.slf4j.LoggerFactory

private class MonitorOutput(val inStream: InputStream, broadcaster: ActorRef) extends Thread {

  val log = LoggerFactory.getLogger("MonitorOutput")
  val in = new InputStreamReader(inStream)

  @volatile var finished = false

  // TODO This should have a stop method

  override def run(): Unit = {
    val buf = new Array[Char](512)

    try {
      var i = in.read(buf, 0, buf.length)
      while (!finished && i >= 0) {
        broadcaster ! DebugOutputEvent(new String(buf, 0, i))
        i = in.read(buf, 0, buf.length)
      }
    } catch {
      case t: Throwable =>
        log.info("Exception during execution", t)
    }
  }
}
