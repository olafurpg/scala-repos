// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
/*
 * This file contains derivative works that require the following
 * header to be displayed:
 *
 * Copyright 2002-2014 EPFL.
 * Copyright 2011-2014 Typesafe, Inc.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software in
 * source or binary form for any purpose with or without fee is hereby
 * granted, provided that the following conditions are met:
 *
 *    1. Redistributions of source code must retain the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer.
 *    2. Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *    3. Neither the name of the EPFL nor the names of its
 *       contributors may be used to endorse or promote products
 *       derived from this software without specific prior written
 *       permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
package org.ensime.core

import akka.actor.ActorRef
import akka.pattern.Patterns
import akka.util.Timeout
import org.ensime.api._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.reflect.internal.util.{ BatchSourceFile, SourceFile }

trait CompletionControl {
  self: RichPresentationCompiler =>

  sealed trait CompletionContext {
    val source: SourceFile
    val offset: Int
    val prefix: String
    val constructing: Boolean
  }

  case class ScopeContext(
    source: SourceFile,
    offset: Int,
    prefix: String,
    constructing: Boolean
  ) extends CompletionContext

  case class MemberContext(
    source: SourceFile,
    offset: Int,
    prefix: String,
    constructing: Boolean
  ) extends CompletionContext

  import CompletionUtil._

  def completionsAt(inputP: Position, maxResultsArg: Int, caseSens: Boolean): CompletionInfoList = {
    val origContents = inputP.source.content
    val point = inputP.endOrCursor

    if (point > origContents.length) {
      // invalid request - completion point is outside of file
      // this causes an ArrayOutOfBounds in the array copy below
      logger.warn("completionsAt request has point outside of file")
      CompletionInfoList("", List.empty)
    } else {
      val maxResults = if (maxResultsArg == 0) Int.MaxValue else maxResultsArg

      val preceding = inputP.source.content.slice(
        Math.max(0, inputP.point - 100), inputP.point
      )

      val defaultPrefix = IdentRegexp.findFirstMatchIn(preceding) match {
        case Some(m) => m.group(1)
        case _ => ""
      }

      val constructing = ConstructingRegexp.findFirstMatchIn(preceding).isDefined

      val (src, p, patched) = if (defaultPrefix.isEmpty) {

        // Add a fake prefix if none was provided by the user. Otherwise the
        // compiler will give us a weird tree.
        val orig = origContents

        // Uses array logic to minimise memory spikes of large Strings
        val contents = Array.ofDim[Char](orig.length + 1)
        System.arraycopy(orig, 0, contents, 0, point)
        contents(point) = 'a'
        System.arraycopy(orig, point, contents, point + 1, orig.length - point)

        // uses the same VirtualFile as the original
        val src = new BatchSourceFile(inputP.source.file, contents)

        (src, inputP.withSource(src).withShift(1), true)
      } else {
        (inputP.source, inputP, false)
      }
      askReloadFile(src)
      val x = new Response[Tree]
      askTypeAt(p, x)

      val contextOpt = x.get match {
        case Left(tree) =>
          logger.debug("Completing at tree:" + tree.summaryString)
          tree match {
            case Apply(fun, _) =>
              fun match {
                case Select(qualifier: New, name) =>
                  Some(ScopeContext(src, qualifier.pos.endOrCursor, defaultPrefix, constructing = true))
                case Select(qual, name) if qual.pos.isDefined && qual.pos.isRange =>
                  val prefix = if (patched) "" else name.decoded
                  Some(MemberContext(src, qual.pos.endOrCursor, prefix, constructing))
                case _ =>
                  val prefix = if (patched) "" else src.content.slice(fun.pos.startOrCursor, fun.pos.endOrCursor).mkString
                  Some(ScopeContext(src, fun.pos.endOrCursor, prefix, constructing))
              }
            case Literal(Constant(_)) => None
            case New(name) =>
              Some(ScopeContext(src, name.pos.endOrCursor, defaultPrefix, constructing = true))
            case Select(qualifier, name) if qualifier.pos.isDefined && qualifier.pos.isRange =>
              Some(MemberContext(src, qualifier.pos.endOrCursor, defaultPrefix, constructing))
            case Import(expr, _) =>
              val topLevel = ImportTopLevelRegexp.findFirstMatchIn(preceding).isDefined
              if (topLevel) {
                Some(ScopeContext(src, expr.pos.endOrCursor, defaultPrefix, constructing = false))
              } else {
                Some(MemberContext(src, expr.pos.endOrCursor, defaultPrefix, constructing = false))
              }
            case other =>
              Some(ScopeContext(src, p.point, defaultPrefix, constructing))
          }
        case _ =>
          logger.error("Unrecognized completion context.")
          None
      }
      contextOpt match {
        case Some(context) =>
          CompletionInfoList(
            context.prefix,
            makeAll(context, maxResults, caseSens).sortWith({ (c1, c2) =>
              c1.relevance > c2.relevance ||
                (c1.relevance == c2.relevance &&
                  c1.name.length < c2.name.length)
            }).take(maxResults)
          )
        case _ => CompletionInfoList("", Nil)
      }
    }
  }

  def makeAll(context: CompletionContext, maxResults: Int, caseSens: Boolean): List[CompletionInfo] = {

    def toCompletionInfo(
      context: CompletionContext,
      sym: Symbol,
      tpe: Type,
      inherited: Boolean,
      viaView: Symbol
    ): List[CompletionInfo] = {

      var score = 0
      if (sym.nameString.startsWith(context.prefix)) score += 10
      if (!inherited) score += 10
      if (!sym.hasPackageFlag) score += 10
      if (!sym.isType) score += 10
      if (sym.isLocalToBlock) score += 10
      if (sym.isPublic) score += 10
      if (viaView == NoSymbol) score += 10
      if (sym.owner != definitions.AnyClass &&
        sym.owner != definitions.AnyRefClass &&
        sym.owner != definitions.ObjectClass) score += 30

      val infos = List(CompletionInfo.fromSymbolAndType(sym, tpe, score))

      if (context.constructing) {
        val constructorSyns = constructorSynonyms(sym).map {
          c => CompletionInfo.fromSymbolAndType(sym, c.tpe, score + 50)
        }
        infos ++ constructorSyns
      } else {
        val applySyns = applySynonyms(sym).map {
          c => CompletionInfo.fromSymbolAndType(sym, c.tpe, score)
        }
        infos ++ applySyns
      }
    }

    val buff = new mutable.LinkedHashSet[CompletionInfo]()

    // Kick off an index search if the name looks like a type.
    // Do this before the lookups below, so the two can
    // proceed concurrently.
    val typeSearch = context match {
      case ScopeContext(_, _, prefix, _) =>
        if (TypeNameRegex.findFirstMatchIn(prefix).isDefined) {
          Some(fetchTypeSearchCompletions(prefix, maxResults, indexer))
        } else None
      case _ => None
    }

    var members = List[Member]()
    val x = new Response[List[Member]]
    context match {
      case ScopeContext(src, offset, _, _) =>
        askScopeCompletion(rangePos(src, offset, offset, offset), x)
      case MemberContext(src, offset, _, _) =>
        askTypeCompletion(rangePos(src, offset, offset, offset), x)
    }
    do {
      x.get match {
        case Left(mems) => members ++= mems
        case _ =>
      }
    } while (!x.isComplete)

    logger.info("Found " + members.size + " members.")

    // Any interaction with the members (their types and symbols) must be done
    // on the compiler thread.
    askOption[Unit] {
      val filtered = members.filter { m =>
        val s = m.sym.nameString
        matchesPrefix(s, context.prefix, matchEntire = false, caseSens = caseSens) && !s.contains("$")
      }
      logger.info("Filtered down to " + filtered.size + ".")
      for (m <- filtered) {
        m match {
          case m @ ScopeMember(sym, tpe, accessible, viaView) =>
            val p = sym.pos
            val inSymbol = p.isRange && (context.offset >= p.startOrCursor && context.offset <= p.endOrCursor)
            if (!sym.isConstructor && !inSymbol) {
              buff ++= toCompletionInfo(context, sym, tpe, inherited = false, NoSymbol)
            }
          case m @ TypeMember(sym, tpe, accessible, inherited, viaView) =>
            if (!sym.isConstructor) {
              buff ++= toCompletionInfo(context, sym, tpe, inherited, viaView)
            }
          case _ =>
        }
      }
    }

    val typeSearchResults = typeSearch.flatMap(Await.result(_, Duration.Inf))

    def keywordCompletions(prefix: String): Seq[CompletionInfo] = {
      if (prefix.length > 0) {
        Keywords.keywordCompletions.filter(_.name.startsWith(prefix))
      } else
        Seq()
    }

    buff.toList ++ typeSearchResults.getOrElse(Nil) ++ keywordCompletions(context.prefix)

  }

}

object Keywords {
  val keywords = Seq(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    //"do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    //"if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "requires",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  val keywordCompletions = keywords map {
    CompletionInfo(_, CompletionSignature(List(), "", hasImplicit = false), false, 100, None)
  }
}

trait Completion { self: RichPresentationCompiler =>

  def completePackageMember(path: String, prefix: String): List[CompletionInfo] = {
    packageSymFromPath(path) match {
      case Some(sym) =>
        val memberSyms = packageMembers(sym).filterNot { s =>
          s == NoSymbol || s.nameString.contains("$")
        }
        memberSyms.flatMap { s =>
          val name = if (s.hasPackageFlag) { s.nameString } else { typeShortName(s) }
          if (name.startsWith(prefix)) Some(CompletionInfo(name, CompletionSignature(List.empty, "", false), isCallable = false, 50, None)) else None
        }.toList.sortBy(ci => (ci.relevance, ci.name))
      case _ => List.empty
    }
  }

}

object CompletionUtil {

  val IdentRegexp = """([a-zA-Z0-9_#:<=>@!%&*+/?\\^|~-]*)\z""".r
  val JavaIdentRegexp = """([a-zA-Z0-9_]*)\z""".r
  val ImportTopLevelRegexp = """import [^\.]*\z""".r
  val ImportRegexp = """import [a-zA-Z0-9_\.]*\z""".r
  val ImportSubtypeRegexp = """import [a-z0-9_\.]*[A-Z][a-zA-Z0-9_]*\.[a-zA-Z0-9_\.]*\z""".r
  val ConstructingRegexp = """new [\.a-zA-Z0-9_]*\z""".r
  val TypeNameRegex = """^[A-Z][a-zA-Z0-9]*\z""".r

  def matchesPrefix(m: String, prefix: String,
    matchEntire: Boolean, caseSens: Boolean): Boolean = {
    val prefixUpper = prefix.toUpperCase

    (matchEntire && m == prefix) ||
      (!matchEntire && caseSens && m.startsWith(prefix)) ||
      (!matchEntire && !caseSens && m.toUpperCase.startsWith(prefixUpper))
  }

  def fetchTypeSearchCompletions(prefix: String, maxResults: Int, indexer: ActorRef): Future[Option[List[CompletionInfo]]] = {
    val req = TypeCompletionsReq(prefix, maxResults)
    import scala.concurrent.ExecutionContext.Implicits.{ global => exe }
    val askRes = Patterns.ask(indexer, req, Timeout(1000.milliseconds))
    askRes.map {
      case s: SymbolSearchResults =>
        s.syms.map { s =>
          CompletionInfo(
            s.localName, CompletionSignature(List.empty, s.name, false),
            isCallable = false, 40, None
          )
        }
      case unknown =>
        throw new IllegalStateException("Unexpected response type from request:" + unknown)
    }.map(Some(_)).recover { case _ => None }
  }

}
