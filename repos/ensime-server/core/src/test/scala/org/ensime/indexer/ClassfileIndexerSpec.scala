// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.ensime.fixture.IsolatedEnsimeVFSFixture
import org.ensime.util.EnsimeSpec
import org.ensime.vfs._

class ClassfileIndexerSpec extends EnsimeSpec with IsolatedEnsimeVFSFixture {

  val indexer = new ClassfileIndexer with SLF4JLogging {}
  import indexer._

  // TODO: some assertions (currently we're just checking that no exceptions are raised!)

  "ClassfileIndexer" should "support Java 6 class files" in withVFS { implicit vfs =>
    indexClassfile(vfs.vres("jdk6/Test.class"))
  }

  it should "support Java 8 class files" in withVFS { implicit vfs =>
    indexClassfile(vfs.vres("jdk8/Test.class"))
    indexClassfile(vfs.vres("jdk8/MyAnnotation.class"))
    indexClassfile(vfs.vres("jdk8/Test$InnerClassWithCtorParam.class"))
  }

  it should "support typical J2SE classes" in withVFS { implicit vfs =>
    val (clazz, refs) = indexClassfile(vfs.vres("java/lang/String.class"))
    clazz.access shouldBe Public
  }

  it should "support typical Scala classes" in withVFS { implicit vfs =>
    indexClassfile(vfs.vres("scala/collection/immutable/List.class"))
    indexClassfile(vfs.vres("scala/collection/immutable/List$.class"))
  }
}
