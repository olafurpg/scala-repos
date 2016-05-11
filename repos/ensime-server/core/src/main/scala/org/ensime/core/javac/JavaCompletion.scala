// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import akka.actor.ActorRef
import akka.event.slf4j.SLF4JLogging
import com.sun.source.tree.{ MemberSelectTree, Tree, IdentifierTree }
import com.sun.source.util.TreePath
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.{ Element, ExecutableElement, PackageElement, TypeElement, VariableElement }
import javax.lang.model.util.{ ElementFilter, Elements }
import org.ensime.core.CompletionUtil
import org.ensime.util.file._
import org.ensime.api._
import scala.collection.JavaConversions._
import java.nio.charset.Charset
import com.sun.source.tree.Scope
import scala.collection.mutable.ArrayBuffer;
import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait JavaCompletion extends Helpers with SLF4JLogging {

  import CompletionUtil._

  protected def scopeForPoint(file: SourceFileInfo, offset: Int): Option[(CompilationInfo, Scope)]
  protected def pathToPoint(file: SourceFileInfo, offset: Int): Option[(CompilationInfo, TreePath)]
  protected def indexer: ActorRef

  def completionsAt(info: SourceFileInfo, offset: Int, maxResultsArg: Int, caseSens: Boolean): CompletionInfoList = {
    val maxResults = if (maxResultsArg == 0) Int.MaxValue else maxResultsArg
    val s = contentsAsString(info, DefaultCharset)

    val preceding = s.slice(Math.max(0, offset - 100), offset)

    log.info("PRECEDING: " + preceding)

    val defaultPrefix = JavaIdentRegexp.findFirstMatchIn(preceding) match {
      case Some(m) => m.group(1)
      case _ => ""
    }

    log.info("PREFIX: " + defaultPrefix)

    val constructing = ConstructingRegexp.findFirstMatchIn(preceding).isDefined

    log.info("CONSTRUCTING: " + constructing)

    val indexAfterTarget = Math.max(0, offset - defaultPrefix.length - 1)

    val precedingChar = s(indexAfterTarget)

    val isMemberAccess = precedingChar == '.'

    val candidates: List[CompletionInfo] = (if (ImportSubtypeRegexp.findFirstMatchIn(preceding).isDefined) {
      // Erase the trailing partial subtype (it breaks type resolution).
      val patched = s.substring(0, indexAfterTarget) + " " + s.substring(indexAfterTarget + defaultPrefix.length + 1);
      (pathToPoint(SourceFileInfo(info.file, Some(patched), None), indexAfterTarget - 1) map {
        case (info: CompilationInfo, path: TreePath) => {
          memberCandidates(info, path.getLeaf, defaultPrefix, true, caseSens)
        }
      })
    } else if (ImportRegexp.findFirstMatchIn(preceding).isDefined) {
      (pathToPoint(info, indexAfterTarget) flatMap {
        case (info: CompilationInfo, path: TreePath) => {
          getEnclosingMemberSelectTree(path).map { m =>
            packageMemberCandidates(info, m, defaultPrefix, caseSens)
          }
        }
      })
    } else if (isMemberAccess) {
      // TODO how to avoid allocating a new string? buffer of immutable string slices?
      // Erase the trailing partial member (it breaks type resolution).
      val patched = s.substring(0, indexAfterTarget) + ".wait()" + s.substring(indexAfterTarget + defaultPrefix.length + 1);
      (pathToPoint(SourceFileInfo(info.file, Some(patched), None), indexAfterTarget + 1) flatMap {
        case (info: CompilationInfo, path: TreePath) => {
          getEnclosingMemberSelectTree(path).map { m =>
            memberCandidates(info, m.getExpression(), defaultPrefix, false, caseSens)
          }
        }
      })
    } else {

      // Kick off an index search if the name looks like a type.
      val typeSearch = if (TypeNameRegex.findFirstMatchIn(defaultPrefix).isDefined) {
        Some(fetchTypeSearchCompletions(defaultPrefix, maxResults, indexer))
      } else None

      (scopeForPoint(info, indexAfterTarget) map {
        case (info: CompilationInfo, s: Scope) => {
          scopeMemberCandidates(info, s, defaultPrefix, caseSens, constructing)
        }
      }) map { scopeCandidates =>
        val typeSearchResult = typeSearch.flatMap(Await.result(_, Duration.Inf)).getOrElse(List())
        scopeCandidates ++ typeSearchResult
      }

    }).getOrElse(List())
    CompletionInfoList(defaultPrefix, candidates.sortWith({ (c1, c2) =>
      c1.relevance > c2.relevance ||
        (c1.relevance == c2.relevance &&
          c1.name.length < c2.name.length)
    }).take(maxResults))
  }

  private def getEnclosingMemberSelectTree(path: TreePath): Option[MemberSelectTree] = {
    var p = path
    while (p != null) {
      p.getLeaf match {
        case m: MemberSelectTree => return Some(m)
        case _ => {}
      }
      p = p.getParentPath
    }
    None
  }

  private def selectedPackageName(m: MemberSelectTree): String = {
    val name = m.getIdentifier.toString
    m.getExpression match {
      case m: MemberSelectTree => selectedPackageName(m) + "." + name
      case i: IdentifierTree => i.getName.toString() + "." + name
      case _ => name
    }
  }

  private def packageMemberCandidates(
    info: CompilationInfo,
    select: MemberSelectTree,
    prefix: String,
    caseSense: Boolean
  ): List[CompletionInfo] = {
    val pkg = selectedPackageName(select)
    val candidates = (Option(info.getElements.getPackageElement(pkg)) map { p: PackageElement =>
      p.getEnclosedElements().flatMap { e => filterElement(info, e, prefix, caseSense, true, false) }
    }).getOrElse(List())
    candidates.toList
  }

  private def filterElement(info: CompilationInfo, e: Element, prefix: String, caseSense: Boolean,
    typesOnly: Boolean, constructors: Boolean, baseRelevance: Int = 0): List[CompletionInfo] = {
    val s = e.getSimpleName.toString

    // reward case case-sensitive matches
    val relevance = if (s.startsWith(prefix)) baseRelevance + 50 else baseRelevance

    if (matchesPrefix(s, prefix, matchEntire = false, caseSens = caseSense) && !s.contains("$")) {
      e match {
        case e: ExecutableElement if !typesOnly => List(methodInfo(e, relevance + 5))
        case e: VariableElement if !typesOnly => List(fieldInfo(e, relevance + 10))
        case e: TypeElement => if (constructors) constructorInfos(info, e, relevance + 5) else List(typeInfo(e, relevance))
        case _ => List()
      }
    } else List()
  }

  private def scopeMemberCandidates(
    info: CompilationInfo,
    scope: Scope,
    prefix: String,
    caseSense: Boolean,
    constructing: Boolean
  ): List[CompletionInfo] = {
    var candidates = ArrayBuffer[CompletionInfo]()

    // Note Scope#getLocalElements does not include fields / members of
    // enclosing classes. Need to add those manually.
    //
    def addTypeMembers(tel: TypeElement, relevance: Int): Unit = {
      for (el <- info.getElements().getAllMembers(tel)) {
        for (info <- filterElement(info, el, prefix, caseSense, false, constructing, relevance)) {
          candidates += info
        }
      }
    }

    var relavence = 0
    for (tel <- Option(scope.getEnclosingClass())) {
      addTypeMembers(tel, relavence)
      var t = tel.getEnclosingElement()
      while (t != null) {
        t match {
          case tel: TypeElement => addTypeMembers(tel, relavence)
          case _ =>
        }
        t = t.getEnclosingElement()
        relavence -= 10
      }
    }

    relavence = 0
    var s = scope
    while (s != null) {
      for (el <- s.getLocalElements()) {
        for (info <- filterElement(info, el, prefix, caseSense, false, constructing, relavence)) {
          candidates += info
        }
      }
      s = s.getEnclosingScope()
      relavence -= 10
    }
    candidates.toList
  }

  private def memberCandidates(
    info: CompilationInfo,
    target: Tree,
    prefix: String,
    importing: Boolean,
    caseSense: Boolean
  ): List[CompletionInfo] = {
    val candidates = typeElement(info, target).map { el =>
      el match {
        case tel: TypeElement => {
          val elements: Elements = info.getElements()
          elements.getAllMembers(tel).flatMap { e =>
            filterElement(info, e, prefix, caseSense, importing, false)
          }
        }
        case e => {
          log.warn("Unrecognized type element " + e)
          List()
        }
      }
    }.getOrElse(List())
    candidates.toList
  }

  private def methodInfo(e: ExecutableElement, relavence: Int): CompletionInfo = {
    val s = e.getSimpleName.toString
    CompletionInfo(
      s,
      CompletionSignature(
        List(e.getParameters().map { p => (p.getSimpleName.toString, p.asType.toString) }.toList),
        e.getReturnType.toString,
        false
      ),
      true, relavence, None
    )
  }

  private def fieldInfo(e: VariableElement, relavence: Int): CompletionInfo = {
    val s = e.getSimpleName.toString
    CompletionInfo(
      s, CompletionSignature(List(), e.asType.toString, false), false, relavence, None
    )
  }

  private def typeInfo(e: TypeElement, relavence: Int): CompletionInfo = {
    val s = e.getSimpleName.toString
    CompletionInfo(
      s, CompletionSignature(List(), e.asType.toString, false), false, relavence, None
    )
  }

  private def constructorInfos(info: CompilationInfo, e: TypeElement, relavence: Int): List[CompletionInfo] = {
    val s = e.getSimpleName.toString
    ElementFilter.constructorsIn(info.getElements().getAllMembers(e)).map(methodInfo(_, relavence)).map { m =>
      m.copy(name = s)
    }.toList
  }

  private def localTypeName(tm: TypeMirror) = {
    val s = tm.toString
    val (front, back) = s.split("\\.").partition { s => s.forall(Character.isLowerCase) }
    if (back.isEmpty) s else back.mkString(".")
  }

  private def contentsAsString(sf: SourceFileInfo, charset: Charset) = sf match {
    case SourceFileInfo(f, None, None) => f.readString()
    case SourceFileInfo(f, Some(contents), None) => contents
    case SourceFileInfo(f, None, Some(contentsIn)) => contentsIn.readString()
  }

}
