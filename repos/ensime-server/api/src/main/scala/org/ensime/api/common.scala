// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File

sealed abstract class DeclaredAs(val symbol: scala.Symbol)

object DeclaredAs {
  case object Method extends DeclaredAs('method)
  case object Trait extends DeclaredAs('trait)
  case object Interface extends DeclaredAs('interface)
  case object Object extends DeclaredAs('object)
  case object Class extends DeclaredAs('class)
  case object Field extends DeclaredAs('field)
  case object Nil extends DeclaredAs('nil)

  def allDeclarations = Seq(Method, Trait, Interface, Object, Class, Field, Nil)
}

sealed trait FileEdit extends Ordered[FileEdit] {
  def file: File
  def text: String
  def from: Int
  def to: Int

  // Required as of Scala 2.11 for reasons unknown - the companion to Ordered
  // should already be in implicit scope
  import scala.math.Ordered.orderingToOrdered

  def compare(that: FileEdit): Int =
    (this.file, this.from, this.to, this.text).compare((that.file, that.from, that.to, that.text))
}

case class TextEdit(file: File, from: Int, to: Int, text: String) extends FileEdit

// the next case classes have weird fields because we need the values in the protocol
case class NewFile(file: File, from: Int, to: Int, text: String) extends FileEdit
object NewFile {
  def apply(file: File, text: String): NewFile = new NewFile(file, 0, text.length - 1, text)
}

case class DeleteFile(file: File, from: Int, to: Int, text: String) extends FileEdit
object DeleteFile {
  def apply(file: File, text: String): DeleteFile = new DeleteFile(file, 0, text.length - 1, text)
}

sealed trait NoteSeverity
case object NoteError extends NoteSeverity
case object NoteWarn extends NoteSeverity
case object NoteInfo extends NoteSeverity
object NoteSeverity {
  def apply(severity: Int) = severity match {
    case 2 => NoteError
    case 1 => NoteWarn
    case 0 => NoteInfo
  }
}

sealed abstract class RefactorLocation(val symbol: Symbol)

object RefactorLocation {
  case object QualifiedName extends RefactorLocation('qualifiedName)
  case object File extends RefactorLocation('file)
  case object NewName extends RefactorLocation('newName)
  case object Name extends RefactorLocation('name)
  case object Start extends RefactorLocation('start)
  case object End extends RefactorLocation('end)
  case object MethodName extends RefactorLocation('methodName)
}

sealed abstract class RefactorType(val symbol: Symbol)

object RefactorType {
  case object Rename extends RefactorType('rename)
  case object ExtractMethod extends RefactorType('extractMethod)
  case object ExtractLocal extends RefactorType('extractLocal)
  case object InlineLocal extends RefactorType('inlineLocal)
  case object OrganizeImports extends RefactorType('organizeImports)
  case object AddImport extends RefactorType('addImport)

  def allTypes = Seq(Rename, ExtractMethod, ExtractLocal, InlineLocal, OrganizeImports, AddImport)
}

case class SourceFileInfo(
    file: File,
    contents: Option[String] = None,
    contentsIn: Option[File] = None
) {
  // keep the log file sane for unsaved files
  override def toString = s"SourceFileInfo($file,${contents.map(_ => "...")},$contentsIn)"
}
