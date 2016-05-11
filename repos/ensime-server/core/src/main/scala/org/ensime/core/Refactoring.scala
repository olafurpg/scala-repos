// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File
import org.ensime.api._
import org.ensime.util._
import org.ensime.util.file.RichFile
import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring._
import scala.tools.refactoring.analysis.GlobalIndexes
import scala.tools.refactoring.common.CompilerAccess
import scala.tools.refactoring.implementations._
import scala.util.{ Success, Try }
import scalariform.astselect.AstSelector
import scalariform.formatter.ScalaFormatter
import scalariform.utils.Range

abstract class RefactoringEnvironment(file: String, start: Int, end: Int) {

  val refactoring: MultiStageRefactoring with CompilerAccess

  def performRefactoring(
    procId: Int,
    tpe: RefactorType,
    parameters: refactoring.RefactoringParameters
  ): Either[RefactorFailure, RefactorEffect] = {

    val af = AbstractFile.getFile(file)

    refactoring.compilationUnitOfFile(af) match {
      case Some(cu) =>
        val selection = new refactoring.FileSelection(af, cu.body, start, end)
        refactoring.prepare(selection) match {
          case Right(prepare) =>
            refactoring.perform(selection, prepare, parameters) match {
              case Right(modifications) =>
                val edits = modifications.map(FileEditHelper.fromChange).sorted
                Right(new RefactorEffect(procId, tpe, edits))
              case Left(error) => Left(RefactorFailure(procId, error.cause))
            }
          case Left(error) => Left(RefactorFailure(procId, error.cause))
        }
      case None =>
        Left(RefactorFailure(procId, "Compilation unit not found: " + af))
    }

  }
}

trait RefactoringHandler { self: Analyzer =>

  private var effects = Map.empty[Int, RefactorEffect]

  def handleRefactorPrepareRequest(req: PrepareRefactorReq): RpcResponse = {
    val procedureId = req.procId
    val refactor = req.params
    val result = scalaCompiler.askPrepareRefactor(procedureId, refactor)

    result match {
      case Right(effect: RefactorEffect) =>
        effects += procedureId -> effect
        effect

      case Left(failure) =>
        failure
    }
  }

  def handleRefactorRequest(req: RefactorReq): RpcResponse = {
    val cs = charset
    val procedureId = req.procId
    val refactor = req.params
    val result = scalaCompiler.askPrepareRefactor(procedureId, refactor)

    result match {
      case Right(effect: RefactorEffect) => {
        FileUtils.writeDiffChanges(effect.changes, cs) match {
          case Right(f) =>
            new RefactorDiffEffect(
              effect.procedureId,
              effect.refactorType,
              f
            )
          case Left(err) => RefactorFailure(effect.procedureId, err.toString)
        }
      }
      case Left(failure) =>
        failure
    }
  }

  def handleRefactorExec(req: ExecRefactorReq): RpcResponse = {
    val procedureId = req.procId
    effects.get(procedureId) match {
      case Some(effect: RefactorEffect) =>
        scalaCompiler.askExecRefactor(procedureId, req.tpe, effect) match {
          case Right(success) => success
          case Left(failure) => failure
        }
      case None =>
        RefactorFailure(procedureId, "No effect found for procId " + procedureId)
    }
  }

  def handleRefactorCancel(req: CancelRefactorReq): RpcResponse = {
    effects -= req.procId
    VoidResponse
  }

  def handleExpandselection(file: File, start: Int, stop: Int): FileRange = {
    FileUtils.readFile(file, charset) match {
      case Right(contents) =>
        val selectionRange = Range(start, stop - start)
        AstSelector.expandSelection(contents, selectionRange) match {
          case Some(range) => FileRange(file.getPath, range.offset, range.offset + range.length)
          case _ =>
            FileRange(file.getPath, start, stop)
        }
      case Left(e) => throw e
    }
  }

  def handleFormatFiles(files: List[File]): Unit = {
    val cs = charset
    val changeList = files.map { f =>
      FileUtils.readFile(f, cs) match {
        case Right(contents) =>
          Try(ScalaFormatter.format(contents, config.formattingPrefs)).map((f, contents, _))
        case Left(e) => throw e
      }
    }.collect {
      case Success((f, contents, formatted)) =>
        TextEdit(f, 0, contents.length, formatted)
    }
    FileUtils.writeChanges(changeList, cs)
  }

  def handleFormatFile(fileInfo: SourceFileInfo): String = {
    val sourceFile = createSourceFile(fileInfo)
    val contents = sourceFile.content.mkString
    ScalaFormatter.format(contents, config.formattingPrefs)
  }

}

trait RefactoringControl { self: RichCompilerControl with RefactoringImpl =>

  def askPrepareRefactor(
    procId: Int,
    refactor: RefactorDesc
  ): Either[RefactorFailure, RefactorEffect] = {
    askOption(prepareRefactor(procId, refactor)).getOrElse(Left(RefactorFailure(procId, "Refactor call failed")))
  }

  def askExecRefactor(
    procId: Int,
    tpe: RefactorType,
    effect: RefactorEffect
  ): Either[RefactorFailure, RefactorResult] = {
    askOption(execRefactor(procId, tpe, effect)).getOrElse(
      Left(RefactorFailure(procId, "Refactor exec call failed."))
    ) match {
        case Right(result) =>
          // Reload all files touched by refactoring, so subsequent refactorings
          // will see consistent state.
          askReloadFiles(result.touchedFiles.map(f => createSourceFile(f.getPath)))
          Right(result)
        case Left(failure) => Left(failure)
      }
  }

}

trait RefactoringImpl { self: RichPresentationCompiler =>

  import org.ensime.util.FileUtils._

  protected def doRename(procId: Int, tpe: RefactorType, name: String, file: File, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new Rename with GlobalIndexes {
        val global = RefactoringImpl.this
        val invalidSet = toBeRemoved.synchronized { toBeRemoved.toSet }
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractMethod(procId: Int, tpe: RefactorType,
    name: String, file: File, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractMethod with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doExtractLocal(procId: Int, tpe: RefactorType,
    name: String, file: File, start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new ExtractLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, name)
    }.result

  protected def doInlineLocal(procId: Int, tpe: RefactorType, file: File,
    start: Int, end: Int) =
    new RefactoringEnvironment(file.getPath, start, end) {
      val refactoring = new InlineLocal with GlobalIndexes {
        val global = RefactoringImpl.this
        val cuIndexes = this.global.activeUnits().map { u => CompilationUnitIndex(u.body) }
        val index = GlobalIndex(cuIndexes.toList)
      }
      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters())
    }.result

  protected def doOrganizeImports(procId: Int, tpe: RefactorType, file: File) =
    new RefactoringEnvironment(file.getPath, 0, 0) {
      val refactoring = new OrganizeImports {
        val global = RefactoringImpl.this
      }

      val result = performRefactoring(procId, tpe, new refactoring.RefactoringParameters(
        options = List(
          refactoring.SortImports,
          refactoring.SortImportSelectors,
          refactoring.CollapseImports,
          refactoring.SimplifyWildcards,
          refactoring.RemoveDuplicates,
          refactoring.GroupImports(List("java", "scala"))
        )
      ))
    }.result

  private def using[A, R <: { def close(): Unit }](r: R)(f: R => A): A = {
    import scala.language.reflectiveCalls
    try {
      f(r)
    } finally {
      r.close()
    }
  }

  protected def doAddImport(procId: Int, tpe: RefactorType, qualName: String, file: File) = {
    val refactoring = new AddImportStatement {
      val global = RefactoringImpl.this
    }

    val af = AbstractFile.getFile(file.getPath)
    val modifications = refactoring.addImport(af, qualName)
    Right(new RefactorEffect(procId, tpe, modifications.map(FileEditHelper.fromChange)))
  }

  protected def reloadAndType(f: File) = reloadAndTypeFiles(List(this.createSourceFile(f.getPath)))

  protected def prepareRefactor(procId: Int, refactor: RefactorDesc): Either[RefactorFailure, RefactorEffect] = {

    val tpe = refactor.refactorType

    try {
      refactor match {
        case InlineLocalRefactorDesc(file, start, end) =>
          reloadAndType(file)
          doInlineLocal(procId, tpe, file, start, end)
        case RenameRefactorDesc(newName, file, start, end) =>
          reloadAndType(file)
          doRename(procId, tpe, newName, file, start, end)
        case ExtractMethodRefactorDesc(methodName, file, start, end) =>
          reloadAndType(file)
          doExtractMethod(procId, tpe, methodName, file, start, end)
        case ExtractLocalRefactorDesc(name, file, start, end) =>
          reloadAndType(file)
          doExtractLocal(procId, tpe, name, file, start, end)
        case OrganiseImportsRefactorDesc(file) =>
          reloadAndType(file)
          doOrganizeImports(procId, tpe, file)
        case AddImportRefactorDesc(qualifiedName, file) =>
          reloadAndType(file)
          doAddImport(procId, tpe, qualifiedName, file)
      }
    } catch {
      case e: Throwable =>
        logger.error("Error during refactor request: " + refactor, e)
        Left(RefactorFailure(procId, e.toString))
    }
  }

  // TODO: don't change files on the server side
  // https://github.com/ensime/ensime-server/issues/996
  protected def execRefactor(
    procId: Int,
    refactorType: RefactorType,
    effect: RefactorEffect
  ): Either[RefactorFailure, RefactorResult] = {
    logger.info("Applying changes: " + effect.changes)
    writeChanges(effect.changes, charset) match {
      case Right(touchedFiles) =>
        val sortedTouchedFiles = touchedFiles.toList.sortBy(_.canon.getPath)
        Right(new RefactorResult(procId, refactorType, sortedTouchedFiles))
      case Left(err) => Left(RefactorFailure(procId, err.toString))
    }
  }

}
