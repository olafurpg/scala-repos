// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.fixture._
import org.ensime.util.EnsimeSpec

class DocFindingSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {
  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.DocsTestProject

  "PresentationCompiler" should "calculate doc signatures" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "import com.google.common.io.Files",
      "import com.google.common.base.Charsets",
      "import java.nio.channels.FileChannel._",
      "import java.io.File",
      "class Thing {",
      "  def main(){",
      "    val o = Some(1)",
      "    val nums = o.m@0@ap(_ + 2)",
      "    val b:Boo@0.5@lean = false",
      "    nums.isDe@1@fined",
      "    val nums2 = o.flat@2@Map {i:Int => Some(i + 1)}",
      "    val x = Some(Some(1)).fla@3@tten",
      "    val y = Some(1).fo@4@ld(0) { ea => ea + 2 }",
      "    val z = Some(1).mkS@5@tring(\".\", \".\", \".\")",
      "    val zz = Some(1).mkS@6@tring",
      "    val zzz = Some(1).mkS@7@tring(\".\")",
      "    val q = Some(1).getOr@8@Else(2)",
      "    val r = Some(1).gro@9@uped(2)",
      "    val xx = List.emp@10@ty",
      "    val f = new File(\".\")",
      "    Files.mo@11@ve(f, f)",
      "    Files.asByte@12@Source(f)",
      "    Files.m@13@ap(f, MapMode.PRIVATE)",
      "    Files.m@14@ap(f, MapMode.PRIVATE, 5)",
      "    val a = Array[Byte]()",
      "    Files.wri@15@te(a, f)",
      "    val aa = So@16@me(4)",
      "    val sss = \"abcd@17@efg\"",
      "    val ii = 123@18@456",
      "    val fo@20@x = new File(\".\")",
      "    val c = classOf[File].isInst@21@ance(fox)",
      "    scala.Predef.DummyIm@22@plicit",
      "    val ha@23@sh = new java.util.HashMap[Int, Int]()",
      "    val entries = hash.entry@24@Set()",
      "    val en@25@try = entries.iterator().next()",
      "    import java.ut@26@il.Vector",
      "    import scala.collec@27@tion._",
      "    val thing: Ex@28@ception = null",
      "    val ou = Some(1) +@29@+ List(1, 2)",
      "    List(1, 2).flat@30@Map(Some(_))",
      "    List(1, 2).coll@31@ect { case 1 => 5 }",
      "  }",
      "}"
    ) { (p, label, cc) =>
        val sig = cc.askDocSignatureAtPoint(p).get

        label match {
          case "0" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("map[B](f:A=>B):Option[B]"))
          case "0.5" => sig.scala shouldBe DocSig(DocFqn("scala", "Boolean"), None)
          case "1" => sig.scala shouldBe DocSig(DocFqn("scala", "Option"), Some("isDefined:Boolean"))
          case "2" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("flatMap[B](f:A=>Option[B]):Option[B]"))
          case "3" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("flatten[B](implicitev:<:<[A,Option[B]]):Option[B]"))
          case "4" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("fold[B](ifEmpty:=>B)(f:A=>B):B"))
          case "5" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("mkString(start:String,sep:String,end:String):String"))
          case "6" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("mkString:String"))
          case "7" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("mkString(sep:String):String"))
          case "8" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("getOrElse[B>:A](default:=>B):B"))
          case "9" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("grouped(size:Int):Iterator[Repr]"))
          case "10" => sig.scala shouldBe DocSig(DocFqn("scala.collection.immutable", "List$"), Some("empty[A]:List[A]"))
          case "11" => sig.java shouldBe DocSig(DocFqn("com.google.common.io", "Files"), Some("move(java.io.File, java.io.File)"))
          case "12" => sig.java shouldBe DocSig(DocFqn("com.google.common.io", "Files"), Some("asByteSource(java.io.File)"))
          case "13" => sig.java shouldBe DocSig(DocFqn("com.google.common.io", "Files"), Some("map(java.io.File, java.nio.channels.FileChannel.MapMode)"))
          case "14" => sig.java shouldBe DocSig(DocFqn("com.google.common.io", "Files"), Some("map(java.io.File, java.nio.channels.FileChannel.MapMode, long)"))
          case "15" => sig.java shouldBe DocSig(DocFqn("com.google.common.io", "Files"), Some("write(byte[], java.io.File)"))
          // TODO(fix this hack) - just goes to the class itself if companion
          // constructor is requested.
          case "16" => sig.java shouldBe DocSig(DocFqn("scala", "Some"), None)
          case "17" => sig.java shouldBe DocSig(DocFqn("java.lang", "String"), None)
          case "18" => sig.scala shouldBe DocSig(DocFqn("scala", "Int"), None)
          case "20" => sig.java shouldBe DocSig(DocFqn("java.io", "File"), None)
          case "21" => sig.java shouldBe DocSig(DocFqn("java.lang", "Class"), Some("isInstance(java.lang.Object)"))
          case "22" => sig.scala shouldBe DocSig(DocFqn("scala", "Predef$$DummyImplicit$"), None)
          case "23" => sig.java shouldBe DocSig(DocFqn("java.util", "HashMap"), None)
          case "24" => sig.java shouldBe DocSig(DocFqn("java.util", "HashMap"), Some("entrySet()"))
          case "25" => sig.java shouldBe DocSig(DocFqn("java.util", "Map.Entry"), None)
          case "26" => sig.java shouldBe DocSig(DocFqn("java.util", "package"), None)
          case "27" => sig.java shouldBe DocSig(DocFqn("scala.collection", "package"), None)
          // TODO: Would be nice to be able to inspect a particular constructor. The problem is that
          // symbolAt returns the type itself when point is in 'File', and it's not totally clear
          // that's wrong.
          //            case "28" => sig.java shouldBe DocSig("java.io.File", Some("File(java.lang.String, java.lang.String)")
          case "28" => sig.scala shouldBe DocSig(DocFqn("scala", "package"), Some("Exception=Exception"))

          // Check @usecase handling.
          case "29" => sig.scala shouldBe DocSig(DocFqn("scala", "Some"), Some("++[B>:A,That](that:scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That"))
          case "30" =>
            val expected = if (scala210)
              DocSig(DocFqn("scala.collection.immutable", "List"), Some("flatMap[B,That](f:A=>scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That"))
            else
              DocSig(DocFqn("scala.collection.immutable", "List"), Some("flatMap[B,That](f:A=>scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That"))

            sig.scala shouldBe expected
          case "31" =>
            val expected = if (scala210)
              DocSig(DocFqn("scala.collection.immutable", "List"), Some("collect[B,That](pf:PartialFunction[A,B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That"))
            else
              DocSig(DocFqn("scala.collection.immutable", "List"), Some("collect[B,That](pf:PartialFunction[A,B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That"))
            sig.scala shouldBe expected
        }

      }

  }
}
