// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.fixture.SharedEnsimeVFSFixture
import org.ensime.util.EnsimeSpec
import org.ensime.vfs._

class ClassfileDepicklerSpec extends EnsimeSpec with SharedEnsimeVFSFixture {

  "ClassfileDepickler" should "not depickle J2SE classes" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("java/lang/String.class")).scalasig should ===(None)
  }

  it should "support typical Scala classes" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("scala/collection/immutable/List.class")).scalasig shouldBe defined
  }

  it should "not expect anything in companions" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("scala/collection/immutable/List$.class")).scalasig should ===(None)
  }

  it should "not expect anything in closures" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("scala/io/Source$$anonfun$1.class")).scalasig should ===(None)
  }

  it should "find type aliases" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("scala/Predef.class")).getTypeAliases should contain(
      RawType(s"scala.Predef$$String", Public)
    )
  }
}
