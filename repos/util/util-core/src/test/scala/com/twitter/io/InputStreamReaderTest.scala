package com.twitter.io

import java.io.ByteArrayInputStream

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import com.twitter.util.Await

@RunWith(classOf[JUnitRunner])
class InputStreamReaderTest extends FunSuite
  def arr(i: Int, j: Int) = Array.range(i, j).map(_.toByte)
  def buf(i: Int, j: Int) = Buf.ByteArray.Owned(arr(i, j))

  test("InputStreamReader - read empty")
    val a = Array.empty[Byte]
    val s = new ByteArrayInputStream(a)
    val r = new InputStreamReader(s, 4096)

    val f = r.read(10)
    assert(Await.result(f).isEmpty)

  test("InputStreamReader - read 0 bytes")
    val a = arr(0, 25)
    val s = new ByteArrayInputStream(a)
    val r = new InputStreamReader(s, 4096)

    val f1 = r.read(0)
    assert(Await.result(f1).get.isEmpty)

  test("InputStreamReader - read positive bytes")
    val a = arr(0, 25)
    val s = new ByteArrayInputStream(a)
    val r = new InputStreamReader(s, 4096)

    val f1 = r.read(10)
    assert(Await.result(f1) == Some(buf(0, 10)))

    val f2 = r.read(10)
    assert(Await.result(f2) == Some(buf(10, 20)))

    val f3 = r.read(10)
    assert(Await.result(f3) == Some(buf(20, 25)))

    val f4 = r.read(10)
    assert(Await.result(f4).isEmpty)

  test("InputStreamReader - read up to maxBufferSize")
    val a = arr(0, 250)
    val s = new ByteArrayInputStream(a)
    val r = new InputStreamReader(s, 100)

    val f1 = r.read(1000)
    assert(Await.result(f1) == Some(buf(0, 100)))

    val f2 = r.read(1000)
    assert(Await.result(f2) == Some(buf(100, 200)))

    val f3 = r.read(1000)
    assert(Await.result(f3) == Some(buf(200, 250)))

    val f4 = r.read(1000)
    assert(Await.result(f4).isEmpty)

  test("InputStreamReader - Reader.readAll")
    val a = arr(0, 250)
    val s1 = new ByteArrayInputStream(a)
    val s2 = new ByteArrayInputStream(a)
    val r1 = new InputStreamReader(s1, 100)
    val r2 = new InputStreamReader(s2, 500)

    val f1 = Reader.readAll(r1)
    assert(Await.result(f1) == buf(0, 250))

    val f2 = Reader.readAll(r1)
    assert(Await.result(f2).isEmpty)

    val f3 = Reader.readAll(r2)
    assert(Await.result(f3) == buf(0, 250))

    val f4 = Reader.readAll(r2)
    assert(Await.result(f4).isEmpty)

  test("InputStreamReader - discard")
    val a = arr(0, 25)
    val s = new ByteArrayInputStream(a)
    val r = new InputStreamReader(s, 4096)

    val f1 = r.read(10)
    assert(Await.result(f1) == Some(buf(0, 10)))

    r.discard()

    val f2 = r.read(10)
    intercept[Reader.ReaderDiscarded] { Await.result(f2) }
