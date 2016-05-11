/* NSC -- new Scala compiler
 * Copyright 2007-2016 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda, Felix Mulder
 */

package scala.tools.nsc.doc
package html
package page

import scala.tools.nsc.doc
import scala.tools.nsc.doc.model.{Package, DocTemplateEntity}
import scala.tools.nsc.doc.html.{Page, HtmlFactory}
import scala.util.parsing.json.{JSONObject, JSONArray, JSONType}

class IndexScript(universe: doc.Universe) extends Page {
  import model._
  import scala.tools.nsc.doc.base.comment.Text
  import scala.collection.immutable.Map

  def path = List("index.js")

  override def writeFor(site: HtmlFactory) {
    writeFile(site) {
      _.write("Index.PACKAGES = " + packages.toString() + ";")
    }
  }

  val packages = {
    val pairs = allPackagesWithTemplates.toIterable
      .map(_ match {
        case (pack, templates) => {
            val merged = mergeByQualifiedName(templates)

            val ary = merged.keys.toList
              .sortBy(_.toLowerCase)
              .map(key =>
                    {
                  val pairs = merged(key).flatMap {
                    t: DocTemplateEntity =>
                      Seq(kindToString(t) -> relativeLinkTo(t),
                          "kind" -> kindToString(t),
                          "members" -> membersToJSON(t.members.filter(
                                  !_.isShadowedOrAmbiguousImplicit)),
                          "shortDescription" -> shortDesc(t))
                  }

                  JSONObject(Map(pairs: _*) + ("name" -> key))
              })

            pack.qualifiedName -> JSONArray(ary)
          }
      })
      .toSeq

    JSONObject(Map(pairs: _*))
  }

  def mergeByQualifiedName(source: List[DocTemplateEntity]) = {
    var result = Map[String, List[DocTemplateEntity]]()

    for (t <- source) {
      val k = t.qualifiedName
      result += k -> (result.getOrElse(k, List()) :+ t)
    }

    result
  }

  def allPackages = {
    def f(parent: Package): List[Package] = {
      parent.packages.flatMap(
          p => f(p) :+ p
      )
    }
    f(universe.rootPackage).sortBy(_.toString)
  }

  def allPackagesWithTemplates = {
    Map(
        allPackages.map(
            (key) =>
              {
        key -> key.templates.collect {
          case t: DocTemplateEntity
              if !t.isPackage &&
              !universe.settings.hardcoded.isExcluded(t.qualifiedName) =>
            t
        }
    }): _*)
  }

  /** Gets the short description i.e. the first sentence of the docstring */
  def shortDesc(mbr: MemberEntity): String = mbr.comment.fold("") { c =>
    inlineToStr(c.short).replaceAll("\n", "")
  }

  /** Returns the json representation of the supplied members */
  def membersToJSON(entities: List[MemberEntity]): JSONType =
    JSONArray(entities map memberToJSON)

  private def memberToJSON(mbr: MemberEntity): JSONObject = {

    /** This function takes a member and gets eventual parameters and the
      *  return type. For example, the definition:
      *  {{{ def get(key: A): Option[B] }}}
      *  Gets turned into: "(key: A): Option[B]"
      */
    def memberTail: MemberEntity => String = {
      case d: Def =>
        d.valueParams //List[List[ValueParam]]
        .map { params =>
          params.map(p => p.name + ": " + p.resultType.name).mkString(", ")
        }.mkString("(", ")(", "): " + d.resultType.name)
      case v: Val => ": " + v.resultType.name
    }

    /** This function takes a member entity and return all modifiers in a
      *  string, example:
      *  {{{ lazy val scalaProps: java.util.Properties }}}
      *  Gets turned into: "lazy val"
      */
    def memberKindToString(mbr: MemberEntity): String = {
      val kind = mbr.flags.map(_.text.asInstanceOf[Text].text).mkString(" ")
      val space = if (kind == "") "" else " "

      kind + space + kindToString(mbr)
    }

    /** This function turns a member entity into a JSON object that the index.js
      *  script can use to render search results
      */
    def jsonObject(m: MemberEntity): JSONObject =
      JSONObject(
          Map("label" -> m.definitionName.replaceAll(".*#", ""), // member name
              "member" -> m.definitionName.replaceFirst("#", "."), // full member name
              "tail" -> memberTail(m),
              "kind" -> memberKindToString(m), // modifiers i.e. "abstract def"
              "link" -> memberToUrl(m))) // permalink to the member

    mbr match {
      case d: Def => jsonObject(d)
      case v: Val => jsonObject(v)
      case m: MemberEntity =>
        JSONObject(
            Map("member" -> m.definitionName, "error" -> "unsupported entity"))
    }
  }

  def memberToUrl(mbr: MemberEntity): String = {
    val path = templateToPath(mbr.inTemplate).reverse.mkString("/")
    s"$path#${mbr.signature}"
  }
}

object IndexScript {
  def apply(universe: doc.Universe) = new IndexScript(universe)
}
