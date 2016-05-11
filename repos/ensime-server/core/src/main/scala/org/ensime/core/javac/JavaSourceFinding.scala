// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import akka.event.slf4j.SLF4JLogging
import com.sun.source.util.TreePath
import java.io.File
import javax.lang.model.element.Element
import org.ensime.api._
import org.ensime.model.LineSourcePositionHelper
import org.ensime.vfs._
import org.ensime.indexer.SearchService

trait JavaSourceFinding extends Helpers with SLF4JLogging {

  def askLinkPos(fqn: JavaFqn, file: SourceFileInfo): Option[SourcePosition]
  def search: SearchService
  def vfs: EnsimeVFS
  def config: EnsimeConfig

  protected def findInCompiledUnit(info: CompilationInfo, fqn: JavaFqn): Option[SourcePosition] = {
    Option(info.getElements().getTypeElement(fqn.toFqnString)).flatMap(elementPosition(info, _))
  }

  private def elementPosition(info: CompilationInfo, el: Element): Option[SourcePosition] = {
    // if we can get a tree for the element, determining start position
    // is easy
    Option(info.getTrees.getPath(el)).map { path =>
      OffsetSourcePosition(
        new File(path.getCompilationUnit.getSourceFile.getName),
        info.getTrees.getSourcePositions
          .getStartPosition(path.getCompilationUnit, path.getLeaf).toInt
      )
    }
  }

  protected def findDeclPos(info: CompilationInfo, path: TreePath): Option[SourcePosition] = {
    element(info, path).flatMap(elementPosition(info, _)).orElse(findInIndexer(info, path))
  }

  private def findInIndexer(info: CompilationInfo, path: TreePath): Option[SourcePosition] = {
    val javaFqn = fqn(info, path)
    val query = javaFqn.map(_.toFqnString).getOrElse("")
    val hit = search.findUnique(query)
    log.debug(s"search: '$query' = $hit")
    hit.flatMap(LineSourcePositionHelper.fromFqnSymbol(_)(config, vfs)).flatMap { sourcePos =>
      if (sourcePos.file.getName.endsWith(".java") && sourcePos.file.exists)
        javaFqn.flatMap(askLinkPos(_, SourceFileInfo(sourcePos.file, None, None))).orElse(Some(sourcePos))
      else
        Some(sourcePos)
    }
  }

}
