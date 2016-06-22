/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package builtin
package snippet

import scala.language.existentials

import http.{S, DispatchSnippet, LiftRules}
import http.js._
import sitemap._
import util._
import common._
import scala.xml._
import JsCmds._
import JE._
import Helpers._

/**
  * <p>This built-in snippet can be used to render a menu representing your SiteMap.
  * There are three main snippet methods that you can use:</p>
  *
  * <ul>
  *   <li>builder - Renders the entire SiteMap, optionally expanding all child menus</li>
  *   <li>group - Renders the MenuItems corresponding to the specified group.</li>
  *   <li>item - Renders the specific named MenuItem</li>
  * </ul>
  *
  * <p>More detailed usage of each method is provided below</p>
  */
object Menu extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case "builder" => builder
    case "title" => title
    case "item" => item
    case "group" => group
    case "json" => jsonMenu
  }

  /**
    * <p>This snippet method renders a menu representing your SiteMap contents. The
    * menu is rendered as a set of nested unordered lists (&lt;ul /&gt;). By
    * default, it only renders nested child menus for menus that match the current request path.
    * You can add the "expandAll" attribute to the snippet tag to force full expansion of
    * all child menus. Additionally, you can use the following attribute prefixes to further customize
    * the generated list and list item elements:</p>
    *
    * <ul>
    *   <li>top - Adds the specified attribute to the top-level &lt;ul&gt; element that makes up the menu</li>
    *   <li>ul - Adds the specified attribute to each &lt;ul&gt; element (top-level and nested children) that makes up the menu</li>
    *   <li>li - Adds the specified attribute to each &lt;li&gt; element for the menu</li>
    *   <li>li_item - Adds the specified attribute to the current page’s menu item</li>
    *   <li>outer_tag - the tag for the outer XML element (ul by default)</li>
    *   <li>inner_tag - the tag for the inner XML element (li by default)</li>
    *   <li>li_path - Adds the specified attribute to the current page’s breadcrumb path. The
    *       breadcrumb path is the set of menu items leading to this one.</li>
    *   <li>linkToSelf - False by default, but available as 'true' to generate link to the current page</li>
    *   <li>level - Controls the level of menus that will be output. "0" is the top-level menu, "1" is children of
    *       the current menu item, and so on. Child menus will be expanded unless the "expand" attribute is set to <pre>false</pre>.</li>
    *   <li>expand - Controls whether or not to expand child menus. Defaults to <pre>true</pre>.</li>
    * </ul>
    *
    * <p>If you are using designer friendly invocation, you can access the namespaced attributes: <br/>
    * &lt;div class="lift:Menu?li_item:class=foo+bar"&gt;menu&lt;/div&gt;
    * </p>
    *
    * <p>For a simple, default menu, simply add</p>
    *
    * <pre>
    *   &lt;lift:Menu.builder /&gt;
    * </pre>
    *
    * <p>To your template. You can render the entire sitemap with</p>
    *
    * <pre>
    *    &lt;lift:Menu.builder expandAll="true" /&gt;
    * </pre>
    *
    * <p>Customizing the elements is handled through the prefixed attributes described above.
    *    For instance, you could make the current page menu item red:</p>
    *
    * <pre>
    *    &lt;lift:Menu.builder li_item:style="color: red;" /&gt;
    * </pre>
    */
  def builder(info: NodeSeq): NodeSeq = {
    val outerTag: String = S.attr("outer_tag") openOr "ul"
    val innerTag: String = S.attr("inner_tag") openOr "li"
    val expandAll = (S.attr("expandAll") or S.attr("expandall")).isDefined
    val linkToSelf: Boolean =
      (S.attr("linkToSelf") or S.attr("linktoself"))
        .map(Helpers.toBoolean) openOr false

    val expandAny: Boolean =
      S.attr("expand").map(Helpers.toBoolean) openOr true

    val level: Box[Int] = for (lvs <- S.attr("level"); i <- Helpers.asInt(lvs))
      yield i

    val toRender: Seq[MenuItem] = (S.attr("item"), S.attr("group")) match {
      case (Full(item), _) =>
        for {
          sm <- LiftRules.siteMap.toList
          req <- S.request.toList
          loc <- sm.findLoc(item).toList
          item <- buildItemMenu(loc, req.location, expandAll)
        } yield item

      case (_, Full(group)) =>
        for {
          sm <- LiftRules.siteMap.toList
          loc <- sm.locForGroup(group)
          req <- S.request.toList
          item <- buildItemMenu(loc, req.location, expandAll)
        } yield item
      case _ => renderWhat(expandAll)
    }

    def ifExpandCurrent(f: => NodeSeq): NodeSeq =
      if (expandAny || expandAll) f else NodeSeq.Empty
    def ifExpandAll(f: => NodeSeq): NodeSeq =
      if (expandAll) f else NodeSeq.Empty

    toRender.toList match {
      case Nil if S.attr("group").isDefined => NodeSeq.Empty
      case Nil => Text("No Navigation Defined.")
      case xs =>
        val liMap = S.prefixedAttrsToMap("li")
        val li = S.mapToAttrs(liMap)

        def buildANavItem(i: MenuItem) = {
          i match {
            // Per Loc.PlaceHolder, placeholder implies HideIfNoKids
            case m @ MenuItem(text, uri, kids, _, _, _)
                if m.placeholder_? && kids.isEmpty =>
              NodeSeq.Empty

            case m @ MenuItem(text, uri, kids, _, _, _) if m.placeholder_? =>
              Helpers.addCssClass(
                  i.cssClass,
                  Elem(
                      null,
                      innerTag,
                      Null,
                      TopScope,
                      true,
                      // Is a placeholder useful if we don't display the kids? I say no (DCB, 20101108)
                      <xml:group> <span>{text}</span>{buildUlLine(kids)}</xml:group>) %
                    (if (m.path)
                       S.prefixedAttrsToMetaData("li_path", liMap)
                     else Null) %
                    (if (m.current) S.prefixedAttrsToMetaData("li_item", liMap)
                     else Null))

            case MenuItem(text, uri, kids, true, _, _) if linkToSelf =>
              Helpers.addCssClass(
                  i.cssClass,
                  Elem(
                      null,
                      innerTag,
                      Null,
                      TopScope,
                      true,
                      <xml:group> <a href={uri}>{text}</a>{ifExpandCurrent(buildUlLine(kids))}</xml:group>) % S
                    .prefixedAttrsToMetaData("li_item", liMap))

            case MenuItem(text, uri, kids, true, _, _) =>
              Helpers.addCssClass(
                  i.cssClass,
                  Elem(
                      null,
                      innerTag,
                      Null,
                      TopScope,
                      true,
                      <xml:group> <span>{text}</span>{ifExpandCurrent(buildUlLine(kids))}</xml:group>) % S
                    .prefixedAttrsToMetaData("li_item", liMap))

            // Not current, but on the path, so we need to expand children to show the current one
            case MenuItem(text, uri, kids, _, true, _) =>
              Helpers.addCssClass(
                  i.cssClass,
                  Elem(
                      null,
                      innerTag,
                      Null,
                      TopScope,
                      true,
                      <xml:group> <a href={uri}>{text}</a>{buildUlLine(kids)}</xml:group>) % S
                    .prefixedAttrsToMetaData("li_path", liMap))

            case MenuItem(text, uri, kids, _, _, _) =>
              Helpers.addCssClass(
                  i.cssClass,
                  Elem(
                      null,
                      innerTag,
                      Null,
                      TopScope,
                      true,
                      <xml:group> <a href={uri}>{text}</a>{ifExpandAll(buildUlLine(kids))}</xml:group>) % li)
          }
        }

        def buildUlLine(in: Seq[MenuItem]): NodeSeq =
          if (in.isEmpty) {
            NodeSeq.Empty
          } else {
            if (outerTag.length > 0) {
              Elem(null,
                   outerTag,
                   Null,
                   TopScope,
                   true,
                   <xml:group>{in.flatMap(buildANavItem)}</xml:group>) % S
                .prefixedAttrsToMetaData("ul")
            } else {
              in.flatMap(buildANavItem)
            }
          }

        val realMenuItems = level match {
          case Full(lvl) if lvl > 0 =>
            def findKids(cur: Seq[MenuItem], depth: Int): Seq[MenuItem] =
              if (depth == 0) cur
              else findKids(cur.flatMap(mi => mi.kids), depth - 1)

            findKids(xs, lvl)

          case _ => xs
        }

        buildUlLine(realMenuItems) match {
          case top: Elem => top % S.prefixedAttrsToMetaData("top")
          case other => other
        }
    }
  }

  // This is used to build a MenuItem for a single Loc
  private def buildItemMenu[A](loc: Loc[A],
                               currLoc: Box[Loc[_]],
                               expandAll: Boolean): List[MenuItem] = {
    val kids: List[MenuItem] =
      if (expandAll) loc.buildKidMenuItems(loc.menu.kids) else Nil

    loc.buildItem(kids, currLoc == Full(loc), currLoc == Full(loc)).toList
  }

  private def renderWhat(expandAll: Boolean): Seq[MenuItem] =
    (if (expandAll)
       for { sm <- LiftRules.siteMap; req <- S.request } yield
         sm.buildMenu(req.location).lines
     else S.request.map(_.buildMenu.lines)) openOr Nil

  def jsonMenu(ignore: NodeSeq): NodeSeq = {
    val toRender = renderWhat(true)

    def buildItem(in: MenuItem): JsExp = in match {
      case MenuItem(text, uri, kids, current, path, _) =>
        JsObj("text" -> text.toString,
              "uri" -> uri.toString,
              "children" -> buildItems(kids),
              "current" -> current,
              "cssClass" -> Str(in.cssClass openOr ""),
              "placeholder" -> in.placeholder_?,
              "path" -> path)
    }

    def buildItems(in: Seq[MenuItem]): JsExp =
      JsArray(in.map(buildItem): _*)

    Script(
        JsCrVar(S.attr("var") openOr "lift_menu",
                JsObj("menu" -> buildItems(toRender))))
  }

  /**
    * <p>Renders the title for the current request path (location). You can use this to
    * automatically set the title for your page based on your SiteMap:</p>
    *
    * <pre>
    * ⋮
    * &lt;head&gt;
    *   &lt;title&gt;&lt;lift:Menu.title /&gt;&lt;/title&gt;
    * &lt;/head&gt;
    * ⋮
    * </pre>
    * <p>HTML5 does not support tags inside the &lt;title&gt; tag,
    * so you must do:
    * </p>
    *
    * <pre>
    * &lt;head&gt;
    *   &lt;title class=&quot;lift:Menu.title&quote;&gt;The page named %*% is being displayed&lt;/title&gt;
    * &lt;/head&gt;
    * </pre>
    * <p>
    * And Lift will substitute the title at the %*% marker if the marker exists, otherwise
    * append the title to the contents of the &lt;title&gt; tag.
    * </p>
    */
  def title(text: NodeSeq): NodeSeq = {
    val r = for (request <- S.request;
                 loc <- request.location) yield loc.title

    text match {
      case TitleText(attrs, str) => {
        r.map { rt =>
          {
            val rts = rt.text
            val idx = str.indexOf("%*%")
            val bodyStr = if (idx >= 0) {
              str.substring(0, idx) + rts + str.substring(idx + 3)
            } else {
              str + " " + rts
            }

            <title>{bodyStr}</title> % attrs
          }
        } openOr text
      }

      case _ => {
        r openOr Text("")
      }
    }
  }

  private object TitleText {
    def unapply(in: NodeSeq): Option[(MetaData, String)] =
      if (in.length == 1 && in(0).isInstanceOf[Elem]) {
        val e = in(0).asInstanceOf[Elem]
        if (e.prefix == null && e.label == "title") {
          if (e.child.length == 0) {
            Some(e.attributes -> "")
          } else if (e.child.length == 1 && e.child(0).isInstanceOf[Atom[_]]) {
            Some(e.attributes -> e.child.text)
          } else None
        } else None
      } else None
  }

  /**
    * Renders a group of menu items. You specify a group using the LocGroup LocItem
    * case class on your Menu Loc:
    *
    * {{{
    * val menus =
    *   Menu(Loc("a",...,...,LocGroup("test"))) ::
    *   Menu(Loc("b",...,...,LocGroup("test"))) ::
    *   Menu(Loc("c",...,...,LocGroup("test"))) :: Nil
    * }}}
    *
    * You can then render with the group snippet:
    *
    * {{{
    * <nav data-lift="Menu.group?group=test">
    *   <ul>
    *     <li>
    *       <a href="/sample/link">Bound menu item</a>
    *     </li>
    *   </ul>
    * </nav>
    * }}}
    *
    * By default, menu items bind the href and text of an `a` element in
    * the template, and iterates over `li` elements. You can customize
    * these settings using the `repeatedSelector`, `linkSelector`, and
    * `hrefSelector` parameters; for example:
    *.
    * {{{
    * <p data-lift="Menu.group?group=test&repeatedSelector=p&linkSelector=p&hrefSelector=[data-link]"
    *    data-link="/sample/link">
    *   Bound menu item
    * </p>
    * }}}
    *
    * These selectors are CSS selector transform selectors. `repeatedSelector`
    * and `linkSelector` are the left-hand-side, while `hrefSelector` is the
    * second part, which indicates what will be replaced by the href text.
    * For example, the above would roughly yield a transform that looks like:
    *
    * {{{
    * "p" #> {
    *   "p [data-link]" #> <menu href> &
    *   "p *" #> <menu text> &
    * }
    * }}}
    */
  def group: CssSel = {
    val repeatedSelector = S.attr("repeatedSelector") openOr "li"
    val linkSelector = S.attr("linkSelector") openOr "a"
    val hrefSelector = S.attr("hrefSelector") openOr "[href]"

    repeatedSelector #> {
      for {
        group <- S.attr("group").toList
        siteMap <- LiftRules.siteMap.toList
        loc <- siteMap.locForGroup(group)
        link <- loc.createDefaultLink
        linkText <- loc.linkText
      } yield {
        s"$linkSelector $hrefSelector" #> link & s"$linkSelector *" #> linkText
      }
    }
  }

  /**
    * <p>Renders a specific, named item, based on the name given in the Menu's Loc paramter:</p>
    *
    * <pre>
    * val menus =
    *   Menu(Loc("a",...,...,LocGroup("test"))) ::
    *   Menu(Loc("b",...,...,LocGroup("test"))) ::
    *   Menu(Loc("c",...,...,LocGroup("test"))) :: Nil
    * </pre>
    *
    * <p>You can then select the item using the name attribute:</p>
    *
    * <pre>
    * &lt;lift:Menu.item name="b" /&gt;
    * </pre>
    *
    * <p>The menu item is rendered as an anchor tag (&lta /&gt;). The text for the link
    * defaults to the named Menu's Loc.linkText, but you can specify your own link text
    * by providing contents to the tag:</p>
    *
    * <pre>
    * &lt;lift:Menu.item name="b"&gt;This is a link&lt;/lift:Menu.item&gt;
    * </pre>
    *
    * <p>Additionally you can customize
    * the tag using attributes prefixed with "a":</p>
    *
    * <pre>
    * &lt;lift:Menu.item name="b" a:style="color: red;" /&gt;
    * </pre>
    *
    * <p>The param attribute may be used with Menu Locs that are
    * CovertableLoc to parameterize the link</p>
    *
    * <p>Normally, the Menu item is not shown on pages that match its Menu's Loc. You can
    * set the "donthide" attribute on the tag to force it to show text only (same text as normal,
    * but not in an anchor tag)</p>
    *
    *
    * <p>Alternatively, you can set the "linkToSelf" attribute to "true" to force a link. You
    * can specify your own link text with the tag's contents. Note that <b>case is significant</b>, so
    * make sure you specify "linkToSelf" and not "linktoself".</p>
    *
    */
  def item(_text: NodeSeq): NodeSeq = {
    val donthide = S.attr("donthide").map(Helpers.toBoolean) openOr false
    val linkToSelf =
      (S.attr("linkToSelf") or S.attr("linktoself"))
        .map(Helpers.toBoolean) openOr false

    val text = ("a" #>
          ((n: NodeSeq) =>
                n match {
                  case e: Elem => e.child
                  case xs => xs
              })).apply(_text)

    for {
      name <- S.attr("name").toList
    } yield {
      type T = Q forSome { type Q }

      // Builds a link for the given loc
      def buildLink[T](loc: Loc[T]) = {
        Group(SiteMap.buildLink(name, text) match {
          case e: Elem =>
            Helpers.addCssClass(loc.cssClassForMenuItem,
                                e % S.prefixedAttrsToMetaData("a"))
          case x => x
        })
      }

      (S.originalRequest.flatMap(_.location),
       S.attr("param"),
       SiteMap.findAndTestLoc(name)) match {
        case (_, Full(param), Full(loc: Loc[_] with ConvertableLoc[_])) => {
          val typedLoc = loc.asInstanceOf[Loc[T] with ConvertableLoc[T]]

          (for {
            pv <- typedLoc.convert(param)
            link <- typedLoc.createLink(pv)
          } yield {
            Helpers.addCssClass(
                typedLoc.cssClassForMenuItem,
                <a href={link}></a> % S.prefixedAttrsToMetaData("a"))
          }) openOr {
            Text("")
          }
        }

        case (Full(loc), _, _) if loc.name == name => {
          (linkToSelf, donthide) match {
            case (true, _) => buildLink(loc)
            case (_, true) => {
              if (!text.isEmpty) {
                Group(text)
              } else {
                Group(loc.linkText openOr Text(loc.name))
              }
            }
            case _ => Text("")
          }
        }

        case (Full(loc), _, _) => buildLink(loc)

        case _ => Text("")
      }
    }
  }
}
