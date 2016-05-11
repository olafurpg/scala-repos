// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import Predef.{ any2stringadd => _ }
import com.google.common.base.Charsets
import com.google.common.io.Files
import java.io.{ File => JFile }

import org.scalatest._

class FileSpec extends FlatSpec with Matchers {
  import file._

  // all the later tests assume that these work, so do them first
  "file._" should "provide scoped temp directories" in {
    var scoped: File = null
    withTempDir { dir =>
      dir should be a 'directory
      assert(dir.exists())
      dir.getPath shouldBe dir.getCanonicalPath

      assert(JFile.createTempFile("foo", "bar", dir).exists())

      scoped = dir
    }
    assert(!scoped.exists())
  }

  it should "provide scoped temp files" in {
    var scoped: File = null
    withTempFile { file =>
      file should be a 'file
      assert(file.exists())
      scoped = file
    }
    assert(!scoped.exists())
  }

  it should "help define new files" in {
    File("foo"): File
  }

  it should "help create children files" in {
    val foo = File("foo")
    val bar = foo / "bar"
    bar.getPath shouldBe s"foo${JFile.separator}bar"
  }

  it should "break a File into its parts" in {
    File("./foo/bar/baz.wiz").parts shouldBe List("foo", "bar", "baz.wiz")
  }

  it should "provide an output stream" in withTempFile { file =>
    val out = file.outputStream
    out.write("hello".getBytes())
    out.close()

    Files.toString(file, Charsets.UTF_8) shouldBe "hello"
  }

  it should "create Files with parents if necessary" in withTempDir { dir =>
    val file = (dir / "baz/bar/foo.wiz")
    file.createWithParents()
    assert(file.exists())
  }

  val unix = Array[Byte](0x61, 0x0a, 0x62, 0x0a, 0x63)
  val windows = Array[Byte](0x61, 0x0d, 0x0a, 0x62, 0x0d, 0x0a, 0x63)
  val abc = List("a", "b", "c")

  it should "read lines from a File with UNIX line endings" in withTempFile { file =>
    val out = file.outputStream()
    out.write(unix)
    out.close()
    file.readLines shouldBe abc
  }

  it should "read lines from a File with Windows line endings" in withTempFile { file =>
    val out = file.outputStream()
    out.write(windows)
    out.close
    file.readLines shouldBe abc
  }

  it should "write lines" in withTempFile { file =>
    file.writeLines(abc)
    // should we perhaps assert that the host OS's newlines are used?
    file.readLines shouldBe abc
  }

  it should "write a string" in withTempFile { file =>
    file.writeString("abc\ndef")
    file.readLines shouldBe List("abc", "def")
  }

  it should "read a UNIX File as a String" in withTempFile { file =>
    val out = file.outputStream()
    out.write(unix)
    out.close()

    file.readString() shouldBe "a\nb\nc"
  }

  it should "read a File as a String preserving CR" in withTempFile { file =>
    val out = file.outputStream()
    out.write(windows)
    out.close()

    // carriage returns are preserved
    file.readString() shouldBe "a\r\nb\r\nc"
  }

  it should "traverse family trees" in {
    val here = File(".")
    here.tree should contain(here)

    here.tree.filter(_.getName == "FileSpec.scala").toList.size shouldBe 1
  }

  it should "calculate children" in withTempDir { dir =>
    dir.children should be an 'empty

    val foo = (dir / "foo")
    val bar = (foo / "bar")
    val baz = (dir / "baz")

    bar.createWithParents()
    baz.createWithParents()

    dir.children should contain only (foo, baz)
  }

  // don't know how to test .canon, no way to systematically create
  // this since the JVM doesn't support symbolic links until Java 7.
}
