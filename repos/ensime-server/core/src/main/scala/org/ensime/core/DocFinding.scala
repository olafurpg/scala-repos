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

// Transform symbols to scaladoc components, with anchors to select specific
// members of a container type.
//
// See scala/src/scaladoc/scala/tools/nsc/doc/base/MemberLookupBase.scala for
// details related to link construction.
trait DocFinding { self: RichPresentationCompiler =>

  private def isRoot(s: Symbol) = (s eq NoSymbol) || s.isRootSymbol || s.isEmptyPackage || s.isEmptyPackageClass

  private def fullPackage(sym: Symbol): String =
    sym.ownerChain.reverse.filterNot(isRoot)
      .takeWhile(_.hasPackageFlag).map(_.nameString).mkString(".")

  private def fullTypeName(sym: Symbol, nestedTypeSep: String, nameString: (Symbol => String)): String =
    sym.ownerChain.takeWhile(!_.hasPackageFlag).reverse.map(nameString).mkString(nestedTypeSep)

  private val ScalaPrim = """^(Boolean|Byte|Char|Double|Float|Int|Long|Short)$""".r
  private val ScalaAny = """^(Any|AnyVal|AnyRef)$""".r
  private def javaFqn(tpe: Type): DocFqn = {
    def nameString(sym: Symbol) = sym.nameString.replace("$", "")
    val sym = tpe.typeSymbol
    val s = if (sym.hasPackageFlag) {
      DocFqn(fullPackage(sym), "package")
    } else {
      DocFqn(fullPackage(sym), fullTypeName(sym, ".", nameString))
    }
    s match {
      case DocFqn("scala", ScalaPrim(datatype)) => DocFqn("", datatype.toLowerCase)
      case DocFqn("scala", ScalaAny(datatype)) => DocFqn("java.lang", "Object")
      case DocFqn("scala", "Array") =>
        tpe.typeArgs.headOption.map { tpe =>
          val fqn = javaFqn(tpe)
          fqn.copy(typeName = fqn.typeName + "[]")
        }.getOrElse(s)
      case _ => s
    }
  }

  protected def scalaFqn(sym: Symbol): DocFqn = {
    def nameString(s: Symbol) = s.nameString + (if ((s.isModule || s.isModuleClass) && !s.hasPackageFlag) "$" else "")
    if (sym.isPackageObjectOrClass) {
      DocFqn(fullPackage(sym.owner), "package")
    } else if (sym.hasPackageFlag) {
      DocFqn(fullPackage(sym), ".package")
    } else {
      DocFqn(fullPackage(sym), fullTypeName(sym, "$", nameString))
    }
  }

  private def linkName(sym: Symbol, java: Boolean): DocFqn = {
    if (java) javaFqn(sym.tpe) else scalaFqn(sym)
  }

  private def signatureString(sym: Symbol, java: Boolean): String = {
    sym.nameString + (if (java) {
      if (sym.paramLists.isEmpty) ""
      else sym.paramLists.flatMap(_.map { sym => javaFqn(sym.tpe).mkString }).mkString("(", ", ", ")")
    } else sym.signatureString.replaceAll("[\\s]", ""))
  }

  def docSignature(sym: Symbol, pos: Option[Position]): Option[DocSigPair] = {
    def docSig(java: Boolean) = {
      val owner = sym.owner
      if (sym.isCaseApplyOrUnapply) {
        DocSig(linkName(owner.companionClass, java), None)
      } else if (sym.isClass || sym.isModule || sym.isTrait || sym.hasPackageFlag)
        DocSig(linkName(sym, java), None)
      else if (owner.isClass || owner.isModule || owner.isTrait || owner.hasPackageFlag) {
        val ownerAtSite = pos.flatMap(specificOwnerOfSymbolAt).getOrElse(owner)
        DocSig(linkName(ownerAtSite, java), Some(signatureString(sym, java)))
      } else
        DocSig(linkName(sym.tpe.typeSymbol, java), None)
    }
    Some(DocSigPair(docSig(java = false), docSig(java = true)))
  }

}
