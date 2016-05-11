// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._

import scala.concurrent._
import scala.concurrent.duration._

class SearchServiceSpec extends EnsimeSpec
    with SharedTestKitFixture
    with SharedSearchServiceFixture
    with SearchServiceTestUtils {

  def original = EnsimeConfigFixture.SimpleTestProject

  "search refreshing" should "parse all files on a pristine structure" in {
    withSearchService { implicit service =>
      val (deleted, indexed) = refresh()
      deleted shouldBe 0
      indexed should be > 0
    }
  }

  it should "not refresh files that have not changed" in {
    withSearchService { implicit service =>
      refresh() shouldBe ((0, 0))
    }
  }

  it should "refresh files that have 'changed'" in {
    withSearchService { (config, service) =>
      implicit val s = service
      val now = System.currentTimeMillis()
      for {
        m <- config.modules.values
        r <- m.targetDirs ++ m.testTargetDirs
        f <- r.tree
      } {
        // simulate a full recompile
        f.setLastModified(now)
      }

      val (deleted, indexed) = refresh()
      deleted should be > 0
      indexed should be > 0
    }
  }

  it should "remove classfiles that have been deleted" in {
    withSearchService { (config, service) =>
      implicit val s = service
      val classfile = config.subprojects.head.targetDirs.head / "org/example/Foo.class"

      classfile shouldBe 'exists

      classfile.delete()
      refresh() shouldBe ((1, 0))
    }
  }

  "class searching" should "return results from J2SE" in withSearchService { implicit service =>
    searchesClasses(
      "java.lang.String",
      "String", "string",
      "j.l.str", "j l str"
    )
  }

  it should "return results from dependencies" in withSearchService { implicit service =>
    searchesClasses(
      "org.scalatest.FunSuite",
      "FunSuite", "funsuite", "funsu",
      "o s Fun"
    )
  }

  it should "return results from the project" in withSearchService { implicit service =>
    searchesClasses(
      "org.example.Bloo",
      "o e bloo"
    )

    searchesClasses(
      "org.example.Blue$",
      "o e blue"
    )

    searchesClasses(
      "org.example.CaseClassWithCamelCaseName",
      "CaseClassWith", "caseclasswith",
      "o e Case", "o.e.caseclasswith",
      "CCWC" // <= CamelCaseAwesomeNess
    )
  }

  it should "return results from package objects" in withSearchService { implicit service =>
    searchClasses(
      "org.example.Blip$",
      "Blip"
    )

    searchClasses(
      "org.example.Blop",
      "Blop"
    )
  }

  "class and method searching" should "return results from classes" in {
    withSearchService { implicit service =>
      searchesClassesAndMethods(
        "java.lang.String",
        "String", "string",
        "j.l.str", "j l str"
      )
    }
  }

  it should "return results from static fields" in withSearchService { implicit service =>
    searchesEmpty(
      "CASE_INSENSITIVE", "case_insensitive",
      "case_"
    )
  }

  it should "not return results from instance fields" in withSearchService { implicit service =>
    searchesEmpty(
      "java.awt.Point.x"
    )
  }

  it should "return results from static methods" in withSearchService { implicit service =>
    searchesClassesAndMethods(
      "java.lang.Runtime.addShutdownHook",
      "addShutdownHook"
    )
  }

  it should "return results from instance methods" in withSearchService { implicit service =>
    searchesClassesAndMethods(
      "java.lang.Runtime.availableProcessors",
      "availableProcessors", "availableP"
    )
  }

  it should "not prioritise noisy inner classes" in withSearchService { implicit service =>
    val hits = service.searchClasses("Baz", 10).map(_.fqn)
    hits should contain theSameElementsAs (Seq(
      "org.example2.Baz",
      "org.example2.Baz$Wibble$baz",
      "org.example2.Baz$Wibble$baz$",
      "org.example2.Baz$Wibble$",
      "org.example2.Baz$",
      "org.example2.Baz$Wibble"
    ))
    hits.head shouldBe "org.example2.Baz"
  }

  "exact searches" should "find type aliases" in withSearchService { implicit service =>
    service.findUnique("org.scalatest.fixture.ConfigMapFixture$FixtureParam") shouldBe defined
  }
}

trait SearchServiceTestUtils {
  self: EnsimeSpec =>

  def refresh()(implicit service: SearchService): (Int, Int) =
    Await.result(service.refresh(), Duration.Inf)

  def searchClasses(expect: String, query: String)(implicit service: SearchService) = {
    val max = 10
    val info = s"'$query' expected '$expect')"
    val results = service.searchClasses(query, max)

    withClue(s"${results.size} $info")(results.size should be <= max)
    withClue(s"$info but was empty")(results should not be empty)
    // when we improve the search quality, we could
    // make this really look only at #1
    val got = results.map(_.fqn)
    withClue(s"$info got '$got'")(got should contain(expect))
    results
  }

  def searchesClasses(expect: String, queries: String*)(implicit service: SearchService) =
    (expect :: queries.toList).foreach(searchClasses(expect, _))

  def searchClassesAndMethods(expect: String, query: String)(implicit service: SearchService) = {
    val max = 10
    val info = s"'$query' expected '$expect')"
    val results = service.searchClassesMethods(List(query), max)
    withClue(s"${results.size} $info")(results.size should be <= max)
    withClue(s"$info but was empty")(results should not be empty)
    // when we improve the search quality, we could
    // make this really look only at #1
    val got = results.map(_.fqn)
    withClue(s"$info got '$got'")(got should contain(expect))
    results
  }

  def searchExpectEmpty(query: String)(implicit service: SearchService) = {
    val max = 1
    val results = service.searchClassesMethods(List(query), max)
    withClue("expected empty results from %s".format(query))(results shouldBe empty)
    results
  }

  def searchesEmpty(queries: String*)(implicit service: SearchService) =
    queries.toList.foreach(searchExpectEmpty)

  def searchesClassesAndMethods(expect: String, queries: String*)(implicit service: SearchService) =
    (expect :: queries.toList).foreach(searchClassesAndMethods(expect, _))

}
