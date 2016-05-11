// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.util.EnsimeSpec

class ClassNameSpec extends EnsimeSpec {
  "ClassName" should "remove \"package\" from FQNs" in {
    ClassName(PackageName(List("org", "example")), s"package$$Class").fqnString shouldBe "org.example.Class"
    ClassName(PackageName(List("org", "example")), "package$Class$Subclass").fqnString shouldBe "org.example.Class$Subclass"
    ClassName(PackageName(List("org", "example", "package")), "Class").fqnString shouldBe "org.example.Class"
    ClassName(PackageName(List("org", "example", "package$")), "Class").fqnString shouldBe "org.example.Class"
  }

  it should "preserve the FQN of package objects" in {
    ClassName(PackageName(List("org.example")), "package").fqnString shouldBe "org.example.package$"
    ClassName(PackageName(List("org.example")), "package$").fqnString shouldBe "org.example.package$"
  }
}
