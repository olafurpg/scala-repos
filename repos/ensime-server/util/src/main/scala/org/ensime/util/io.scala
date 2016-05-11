// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io._
import com.google.common.io.ByteStreams

package object io {

  implicit class RichInputStream(val is: InputStream) extends AnyVal {
    def toByteArray(): Array[Byte] = ByteStreams.toByteArray(is)
  }

  implicit class RichOutputStream(val os: OutputStream) extends AnyVal {
    /**
     * Copy the input stream to the output stream, making best
     * endeavours to close everything afterward (even on failure).
     */
    def drain(in: InputStream): Unit =
      try ByteStreams.copy(in, os)
      finally {
        try in.close()
        finally os.close()
      }
  }

}
