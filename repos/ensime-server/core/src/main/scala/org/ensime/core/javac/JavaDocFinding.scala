// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import com.sun.source.util.TreePath
import org.ensime.core.{ DocSigPair }

trait JavaDocFinding extends Helpers {

  def docSignature(info: CompilationInfo, p: TreePath): Option[DocSigPair] = {
    fqn(info, p).map { fqn =>
      val sig = fqn.toDocSig
      DocSigPair(sig, sig)
    }
  }

}
