// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.util.EnsimeSpec
import DescriptorParser.{ parse, parseType }
import ClassName._
import scala.util.Try

class DescriptorParserSpec extends EnsimeSpec {

  private val SZ = ClassName(PackageName(List("scalaz", "syntax")), "ToApplicativeOps$ApplicativeIdV$$anonfun$η$1")
  private val S = ClassName(PackageName(List("java", "lang")), "String")
  private val A = ArrayDescriptor
  private val D = Descriptor
  private val I = PrimitiveInt
  private val V = PrimitiveVoid
  private val Z = PrimitiveBoolean
  private val root = PackageName(Nil)

  "DescriptorParser" should "fail to parse the empty string" in {
    intercept[Exception](parse(""))
  }

  it should "fail to parse a bad string" in {
    intercept[Exception](parse("not valid"))
  }

  it should "parse descriptors without parameters" in {
    parse("()V") should ===(D(Nil, PrimitiveVoid))
    parse("()Ljava/lang/String;") should ===(D(Nil, S))
    parse("()[Ljava/lang/String;") should ===(D(Nil, A(S)))
    parse("()[[Ljava/lang/String;") should ===(D(Nil, A(A(S))))
    parse("()[[[Ljava/lang/String;") should ===(D(Nil, A(A(A(S)))))
  }

  it should "handle multiple object parameters" in {
    parse("(I[IILjava/lang/String;Z)V") should ===(D(List(I, A(I), I, S, Z), V))
  }

  it should "be invertible" in {
    def invert(desc: String) =
      parse(desc).descriptorString shouldBe desc

    invert("(I[IILjava/lang/String;Z)V")
  }

  "DescriptorParser's JVM internal mode" should "fail to parse the empty string" in {
    intercept[Exception](parseType(""))
  }

  it should "fail to parse a bad string" in {
    intercept[Exception](parseType("not valid"))
  }

  it should "handle $_- in package names" in {
    parseType("Lcom/-$random_/Foo;") should ===(ClassName(PackageName(List("com", "-$random_")), "Foo"))
  }

  it should "handle examples" in {
    parseType("Lscalaz/syntax/ToApplicativeOps$ApplicativeIdV$$anonfun$η$1;") should ===(SZ)
    parseType("Ljava/lang/String;") should ===(S)
    parseType("[Ljava/lang/String;") should ===(A(S))
    parseType("[[Ljava/lang/String;") should ===(A(A(S)))
    parseType("V") should ===(V)
    parseType("LMyAnnotation;") should ===(ClassName(root, "MyAnnotation"))

    // of course, SUN break their own rules for package names (capitals)
    Try(parseType("Lcom/sun/tools/corba/se/idl/toJavaPortable/NameModifierImpl;")).success

    // hmmm, apache, what???? dashes in package names????
    Try(parseType("Lorg/spark-project/guava/annotations/VisibleForTesting;")).success
  }

  it should "be invertible" in {
    def invert(desc: String) =
      parseType(desc).internalString shouldBe desc

    invert("Ljava/lang/String;")
    invert("[[Ljava/lang/String;")
  }
}
