package com.twitter.io

import java.io.{ByteArrayInputStream, DataInputStream, File}
import java.util.Arrays

import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TempFileTest extends WordSpec

  "TempFile" should

    "load resources" in
      val f1 = TempFile.fromResourcePath("/java/lang/String.class")
      val f2 = TempFile.fromResourcePath(getClass, "/java/lang/String.class")
      val f3 = TempFile.fromSystemResourcePath("java/lang/String.class")

      val c1 = Files.readBytes(f1)
      val c2 = Files.readBytes(f2)
      val c3 = Files.readBytes(f3)

      assert(Arrays.equals(c1, c2))
      assert(Arrays.equals(c2, c3))
      assert(
          new DataInputStream(new ByteArrayInputStream(c1)).readInt == 0xcafebabe)
