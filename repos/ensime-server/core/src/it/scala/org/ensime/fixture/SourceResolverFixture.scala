// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.fixture

import org.ensime.api._
import org.ensime.vfs._
import org.ensime.indexer._

trait SourceResolverFixture {
  def withSourceResolver(testCode: SourceResolver => Any): Any
  def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any
}

trait IsolatedSourceResolverFixture extends SourceResolverFixture
    with IsolatedEnsimeConfigFixture {
  override def withSourceResolver(testCode: SourceResolver => Any): Any = withEnsimeConfig { config =>
    implicit val vfs = EnsimeVFS()
    try {
      testCode(new SourceResolver(config))
    } finally {
      vfs.close()
    }
  }
  override def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any = withEnsimeConfig { config =>
    implicit val vfs = EnsimeVFS()
    try {
      testCode(config, new SourceResolver(config))
    } finally {
      vfs.close()
    }
  }
}

trait SharedSourceResolverFixture extends SourceResolverFixture
    with SharedEnsimeConfigFixture {
  this: SharedEnsimeVFSFixture =>

  private[fixture] var _resolver: SourceResolver = _
  override def beforeAll(): Unit = {
    super.beforeAll()
    _resolver = new SourceResolver(_config)
  }

  override def withSourceResolver(testCode: SourceResolver => Any): Any = testCode(_resolver)
  override def withSourceResolver(testCode: (EnsimeConfig, SourceResolver) => Any): Any = {
    testCode(_config, _resolver)
  }

}
