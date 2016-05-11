// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.{ File => JFile }
import java.nio.charset.Charset

import akka.actor._
import akka.event.LoggingReceive.withLabel
import org.ensime.api._
import org.ensime.vfs._
import org.ensime.indexer.SearchService
import org.ensime.model._
import org.ensime.util.{ PresentationReporter, ReportHandler, FileUtils }
import org.slf4j.LoggerFactory
import org.ensime.util.file._

import scala.reflect.internal.util.{ OffsetPosition, RangePosition, SourceFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.util.Try

case class CompilerFatalError(e: Throwable)

/**
 * Information necessary to create a javadoc or scaladoc URI for a
 * particular type or type member.
 */
case class DocFqn(pack: String, typeName: String) {
  def mkString: String = if (pack.isEmpty) typeName else pack + "." + typeName
  def inPackage(prefix: String): Boolean = pack == prefix || pack.startsWith(prefix + ".")
  def javaStdLib: Boolean = inPackage("java") || inPackage("javax")
  def androidStdLib: Boolean = inPackage("android")
  def scalaStdLib: Boolean = inPackage("scala")
}
case class DocSig(fqn: DocFqn, member: Option[String])

/**
 * We generate DocSigs for java and scala at the same time, since we
 * don't know a priori whether the docs will be in scaladoc or javadoc
 * format.
 */
case class DocSigPair(scala: DocSig, java: DocSig)

class Analyzer(
    broadcaster: ActorRef,
    indexer: ActorRef,
    search: SearchService,
    implicit val config: EnsimeConfig,
    implicit val vfs: EnsimeVFS
) extends Actor with Stash with ActorLogging with RefactoringHandler {

  import FileUtils._

  private var allFilesMode = false

  private var settings: Settings = _
  private var reporter: PresentationReporter = _

  protected var scalaCompiler: RichCompilerControl = _

  override def preStart(): Unit = {
    val presCompLog = LoggerFactory.getLogger(classOf[Global])

    settings = new Settings(presCompLog.error)
    settings.YpresentationDebug.value = presCompLog.isTraceEnabled
    settings.YpresentationVerbose.value = presCompLog.isDebugEnabled
    settings.verbose.value = presCompLog.isDebugEnabled
    settings.usejavacp.value = false
    config.scalaLibrary match {
      case Some(scalaLib) => settings.bootclasspath.value = scalaLib.getAbsolutePath
      case None => log.warning("scala-library.jar not present, enabling Odersky mode")
    }
    settings.classpath.value = config.compileClasspath.mkString(JFile.pathSeparator)
    settings.processArguments(config.compilerArgs, processAll = false)
    presCompLog.debug("Presentation Compiler settings:\n" + settings)

    reporter = new PresentationReporter(new ReportHandler {
      override def messageUser(str: String): Unit = {
        broadcaster ! SendBackgroundMessageEvent(str, 101)
      }
      override def clearAllScalaNotes(): Unit = {
        broadcaster ! ClearAllScalaNotesEvent
      }
      override def reportScalaNotes(notes: List[Note]): Unit = {
        broadcaster ! NewScalaNotesEvent(isFull = false, notes)
      }
    })
    reporter.disable() // until we start up

    scalaCompiler = makeScalaCompiler()

    broadcaster ! SendBackgroundMessageEvent("Initializing Analyzer. Please wait...")

    scalaCompiler.askNotifyWhenReady()
    if (config.sourceMode) scalaCompiler.askReloadAllFiles()
  }

  protected def makeScalaCompiler() = new RichPresentationCompiler(
    config, settings, reporter, self, indexer, search
  )

  protected def restartCompiler(keepLoaded: Boolean): Unit = {
    log.warning("Restarting the Presentation Compiler")
    val files = scalaCompiler.loadedFiles
    scalaCompiler.askShutdown()
    scalaCompiler = makeScalaCompiler()
    if (keepLoaded) {
      scalaCompiler.askReloadFiles(files)
    }
    scalaCompiler.askNotifyWhenReady()
    broadcaster ! CompilerRestartedEvent
  }

  override def postStop(): Unit = {
    Try(scalaCompiler.askShutdown())
  }

  def charset: Charset = scalaCompiler.charset

  def receive: Receive = startup

  def startup: Receive = withLabel("startup") {
    case FullTypeCheckCompleteEvent =>
      reporter.enable()
      // legacy clients expect to see AnalyzerReady and a
      // FullTypeCheckCompleteEvent on connection.
      broadcaster ! Broadcaster.Persist(AnalyzerReadyEvent)
      broadcaster ! Broadcaster.Persist(FullTypeCheckCompleteEvent)
      context.become(ready)
      unstashAll()

    case other =>
      stash()
  }

  def ready: Receive = withLabel("ready") {
    case ReloadExistingFilesEvent if allFilesMode =>
      log.info("Skipping reload, in all-files mode")
    case ReloadExistingFilesEvent =>
      restartCompiler(keepLoaded = true)

    case FullTypeCheckCompleteEvent =>
      broadcaster ! FullTypeCheckCompleteEvent

    case req: RpcAnalyserRequest =>
      // fommil: I'm not entirely sure about the logic of
      // enabling/disabling the reporter so I am reluctant to refactor
      // this, but it would perhaps be simpler if we enable the
      // reporter when the presentation compiler is loaded, and only
      // disable it when we explicitly want it to be quiet, instead of
      // enabling on every incoming message.
      reporter.enable()
      allTheThings(req)
  }

  def allTheThings: PartialFunction[RpcAnalyserRequest, Unit] = {
    case RemoveFileReq(file: File) =>
      scalaCompiler.askRemoveDeleted(file)
      sender ! VoidResponse
    case TypecheckAllReq =>
      allFilesMode = true
      scalaCompiler.askRemoveAllDeleted()
      scalaCompiler.askReloadAllFiles()
      scalaCompiler.askNotifyWhenReady()
      sender ! VoidResponse
    case UnloadAllReq =>
      if (config.sourceMode) {
        log.info("in source mode, will reload all files")
        scalaCompiler.askRemoveAllDeleted()
        restartCompiler(keepLoaded = true)
      } else {
        allFilesMode = false
        restartCompiler(keepLoaded = false)
      }
      sender ! VoidResponse
    case TypecheckFileReq(fileInfo) =>
      sender ! handleReloadFiles(List(fileInfo))
    case TypecheckFilesReq(files) =>
      sender ! handleReloadFiles(files.map(toSourceFileInfo))
    case req: PrepareRefactorReq =>
      sender ! handleRefactorPrepareRequest(req)
    case req: ExecRefactorReq =>
      sender ! handleRefactorExec(req)
    case req: CancelRefactorReq =>
      sender ! handleRefactorCancel(req)
    case req: RefactorReq =>
      sender ! handleRefactorRequest(req)
    case CompletionsReq(fileInfo, point, maxResults, caseSens, _reload) =>
      sender ! withExisting(fileInfo) {
        reporter.disable()
        scalaCompiler.askCompletionsAt(pos(fileInfo, point), maxResults, caseSens)
      }
    case UsesOfSymbolAtPointReq(file, point) =>
      sender ! withExisting(file) {
        val p = pos(file, point)
        scalaCompiler.askLoadedTyped(p.source)
        val uses = scalaCompiler.askUsesOfSymAtPoint(p)
        ERangePositions(uses.map(ERangePositionHelper.fromRangePosition))
      }
    case PackageMemberCompletionReq(path: String, prefix: String) =>
      val members = scalaCompiler.askCompletePackageMember(path, prefix)
      sender ! members
    case InspectTypeAtPointReq(file, range: OffsetRange) =>
      sender ! withExisting(file) {
        val p = pos(file, range)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askInspectTypeAt(p).getOrElse(FalseResponse)
      }
    case InspectTypeByNameReq(name: String) =>
      sender ! scalaCompiler.askInspectTypeByName(name).getOrElse(FalseResponse)
    case SymbolAtPointReq(file, point: Int) =>
      sender ! withExisting(file) {
        val p = pos(file, point)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askSymbolInfoAt(p).getOrElse(FalseResponse)
      }
    case SymbolByNameReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) =>
      sender ! scalaCompiler.askSymbolByName(typeFullName, memberName, signatureString).getOrElse(FalseResponse)
    case DocUriAtPointReq(file, range: OffsetRange) =>
      val p = pos(file, range)
      scalaCompiler.askLoadedTyped(p.source)
      sender() ! scalaCompiler.askDocSignatureAtPoint(p)
    case DocUriForSymbolReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) =>
      sender() ! scalaCompiler.askDocSignatureForSymbol(typeFullName, memberName, signatureString)
    case InspectPackageByPathReq(path: String) =>
      sender ! scalaCompiler.askPackageByPath(path).getOrElse(FalseResponse)
    case TypeAtPointReq(file, range: OffsetRange) =>
      sender ! withExisting(file) {
        val p = pos(file, range)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askTypeInfoAt(p).getOrElse(FalseResponse)
      }
    case TypeByNameReq(name: String) =>
      sender ! scalaCompiler.askTypeInfoByName(name).getOrElse(FalseResponse)
    case TypeByNameAtPointReq(name: String, file, range: OffsetRange) =>
      sender ! withExisting(file) {
        val p = pos(file, range)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askTypeInfoByNameAt(name, p).getOrElse(FalseResponse)
      }
    case SymbolDesignationsReq(f, start, end, Nil) =>
      sender ! SymbolDesignations(f.file, List.empty)
    case SymbolDesignationsReq(f, start, end, tpes) =>
      sender ! withExisting(f) {
        val sf = createSourceFile(f)
        val clampedEnd = math.max(end, start)
        val pos = new RangePosition(sf, start, start, clampedEnd)
        scalaCompiler.askLoadedTyped(pos.source)
        scalaCompiler.askSymbolDesignationsInRegion(pos, tpes)
      }
    case ImplicitInfoReq(file, range: OffsetRange) =>
      sender ! withExisting(file) {
        val p = pos(file, range)
        scalaCompiler.askLoadedTyped(p.source)
        scalaCompiler.askImplicitInfoInRegion(p)
      }
    case ExpandSelectionReq(file, start: Int, stop: Int) =>
      sender ! handleExpandselection(file, start, stop)
    case FormatSourceReq(files: List[File]) =>
      handleFormatFiles(files)
      sender ! VoidResponse
    case FormatOneSourceReq(fileInfo: SourceFileInfo) =>
      sender ! StringResponse(handleFormatFile(fileInfo))
    case StructureViewReq(fileInfo: SourceFileInfo) =>
      sender ! withExisting(fileInfo) {
        val sourceFile = createSourceFile(fileInfo)
        StructureView(scalaCompiler.askStructure(sourceFile))
      }
  }

  def handleReloadFiles(files: List[SourceFileInfo]): RpcResponse = {
    val (existing, missingFiles) = files.partition(FileUtils.exists)
    if (missingFiles.nonEmpty) {
      val missingFilePaths = missingFiles.map { f => "\"" + f.file + "\"" }.mkString(",")
      EnsimeServerError(s"file(s): $missingFilePaths do not exist")
    } else {
      val (javas, scalas) = existing.partition(_.file.getName.endsWith(".java"))
      if (scalas.nonEmpty) {
        val sourceFiles = scalas.map(createSourceFile)
        scalaCompiler.askReloadFiles(sourceFiles)
        scalaCompiler.askNotifyWhenReady()
      }
      VoidResponse
    }
  }

  def withExisting(x: SourceFileInfo)(f: => RpcResponse): RpcResponse =
    if (FileUtils.exists(x)) f else EnsimeServerError(s"File does not exist: ${x.file}")

  def pos(file: File, range: OffsetRange): OffsetPosition =
    pos(createSourceFile(file), range)
  def pos(file: File, offset: Int): OffsetPosition =
    pos(createSourceFile(file), offset)
  def pos(file: SourceFileInfo, range: OffsetRange): OffsetPosition =
    pos(createSourceFile(file), range)
  def pos(file: SourceFileInfo, offset: Int): OffsetPosition =
    pos(createSourceFile(file), offset)
  def pos(f: SourceFile, range: OffsetRange): OffsetPosition = {
    if (range.from == range.to) new OffsetPosition(f, range.from)
    else new RangePosition(f, range.from, range.from, range.to)
  }
  def pos(f: SourceFile, offset: Int): OffsetPosition = new OffsetPosition(f, offset)

  def createSourceFile(file: File): SourceFile =
    scalaCompiler.createSourceFile(file.getPath)

  def createSourceFile(file: SourceFileInfo): SourceFile =
    scalaCompiler.createSourceFile(file)
}

object Analyzer {
  def apply(
    broadcaster: ActorRef,
    indexer: ActorRef,
    search: SearchService
  )(
    implicit
    config: EnsimeConfig,
    vfs: EnsimeVFS
  ) = Props(new Analyzer(broadcaster, indexer, search, config, vfs))
}
