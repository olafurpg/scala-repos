// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.File

class DiffUtilSpec extends EnsimeSpec {
  "DiffUtil" should "compare original and revised contents and produce a diff in the unified format" in {
    val originalContent =
      """|line1
         |line2
         |line3"""
        .stripMargin.lines.toSeq

    val revisedContent =
      """|line1
         |new-line2
         |line3"""
        .stripMargin.lines.toSeq
    val a = new File("a").getAbsolutePath()
    val b = new File("b").getAbsolutePath()
    val expectedDiff =
      s"""|--- $a	1970-01-01 12:00:00 +0000
          |+++ $b	1970-01-01 12:00:00 +0000
          |@@ -1,3 +1,3 @@
          | line1
          |-line2
          |+new-line2
          | line3
          |""".stripMargin

    val diff = DiffUtil.compareContents(originalContent, revisedContent)

    diff should ===(expectedDiff)
  }
}
