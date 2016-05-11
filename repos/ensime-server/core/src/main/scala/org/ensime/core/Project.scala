// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import akka.actor._
import akka.event.LoggingReceive.withLabel
import org.apache.commons.vfs2.FileObject
import org.ensime.api._
import org.ensime.core.debug.DebugManager
import org.ensime.vfs._
import org.ensime.indexer._

import scala.collection.immutable.ListSet
import scala.concurrent.duration._
import scala.util._

import org.ensime.util.file._
import org.ensime.util.FileUtils

/**
 * The Project actor simply forwards messages coming from the user to
 * the respective subcomponent.
 */
class Project(
    broadcaster: ActorRef,
    implicit val config: EnsimeConfig
) extends Actor with ActorLogging with Stash {
  import context.{ dispatcher, system }

  import FileUtils._

  /* The main components of the ENSIME server */
  private var scalac: ActorRef = _
  private var javac: ActorRef = _
  private var debugger: ActorRef = _

  // TODO consolidate search/indexer
  private var indexer: ActorRef = _
  private var docs: ActorRef = _

  // TODO: use state transitions to manage this state
  // vfs, resolver, search and watchers are considered "reliable" (hah!)
  // TODO: Actor-ise as many of these vals as possible
  private implicit val vfs: EnsimeVFS = EnsimeVFS()
  private val resolver = new SourceResolver(config)
  private val searchService = new SearchService(config, resolver)
  private val sourceWatcher = new SourceWatcher(config, resolver :: Nil)
  private val reTypecheck = new FileChangeListener {
    def reTypeCheck(): Unit = self ! AskReTypecheck
    def fileAdded(f: FileObject): Unit = reTypeCheck()
    def fileChanged(f: FileObject): Unit = reTypeCheck()
    def fileRemoved(f: FileObject): Unit = reTypeCheck()
    override def baseReCreated(f: FileObject): Unit = reTypeCheck()
  }
  private val classfileWatcher = context.actorOf(Props(new ClassfileWatcher(config, searchService :: reTypecheck :: Nil)), "classFileWatcher")

  def receive: Receive = awaitingConnectionInfoReq

  def awaitingConnectionInfoReq: Receive = withLabel("awaitingConnectionInfoReq") {
    case ConnectionInfoReq =>
      sender() ! ConnectionInfo()
      context.become(handleRequests)
      init()
      unstashAll()
    case other =>
      stash()
  }

  private def init(): Unit = {
    searchService.refresh().onComplete {
      case Success((deletes, inserts)) =>
        // legacy clients expect to see IndexerReady on connection.
        // we could also just blindly send this on each connection.
        broadcaster ! Broadcaster.Persist(IndexerReadyEvent)
        log.debug(s"created $inserts and removed $deletes searchable rows")
      case Failure(problem) =>
        log.warning(problem.toString)
        throw problem
    }(context.dispatcher)
    indexer = context.actorOf(Indexer(searchService), "indexer")
    if (config.scalaLibrary.isDefined || Set("scala", "dotty")(config.name)) {

      // we merge scala and java AnalyzerReady messages into a single
      // AnalyzerReady message, fired only after java *and* scala are ready
      val merger = context.actorOf(Props(new Actor {
        var senders = ListSet.empty[ActorRef]
        def receive: Receive = {
          case Broadcaster.Persist(AnalyzerReadyEvent) if senders.size == 1 =>
            broadcaster ! Broadcaster.Persist(AnalyzerReadyEvent)
          case Broadcaster.Persist(AnalyzerReadyEvent) => senders += sender()
          case msg => broadcaster forward msg
        }
      }))

      scalac = context.actorOf(Analyzer(merger, indexer, searchService), "scalac")
      javac = context.actorOf(JavaAnalyzer(merger, indexer, searchService), "javac")
    } else {
      log.warning("Detected a pure Java project. Scala queries are not available.")
      scalac = system.deadLetters
      javac = context.actorOf(JavaAnalyzer(broadcaster, indexer, searchService), "javac")
    }
    debugger = context.actorOf(DebugManager(broadcaster), "debugging")
    docs = context.actorOf(DocResolver(), "docs")
  }

  override def postStop(): Unit = {
    // make sure the "reliable" dependencies are cleaned up
    Try(sourceWatcher.shutdown())
    searchService.shutdown() // async
    Try(vfs.close())
  }

  // debounces ReloadExistingFilesEvent
  private var rechecking: Cancellable = _

  def handleRequests: Receive = withLabel("handleRequests") {
    case AskReTypecheck =>
      Option(rechecking).foreach(_.cancel())
      rechecking = system.scheduler.scheduleOnce(
        5 seconds, scalac, ReloadExistingFilesEvent
      )
    // HACK: to expedite initial dev, Java requests use the Scala API
    case m @ TypecheckFileReq(sfi) if sfi.file.isJava => javac forward m
    case m @ CompletionsReq(sfi, _, _, _, _) if sfi.file.isJava => javac forward m
    case m @ DocUriAtPointReq(sfi, _) if sfi.file.isJava => javac forward m
    case m @ TypeAtPointReq(sfi, _) if sfi.file.isJava => javac forward m
    case m @ SymbolDesignationsReq(sfi, _, _, _) if sfi.file.isJava => javac forward m
    case m @ SymbolAtPointReq(sfi, _) if sfi.file.isJava => javac forward m

    // mixed mode query
    case TypecheckFilesReq(files) =>
      val (javas, scalas) = files.partition(_.file.isJava)
      if (javas.nonEmpty) javac forward TypecheckFilesReq(javas)
      if (scalas.nonEmpty) scalac forward TypecheckFilesReq(scalas)

    case m: RpcAnalyserRequest => scalac forward m
    case m: RpcDebuggerRequest => debugger forward m
    case m: RpcSearchRequest => indexer forward m
    case m: DocSigPair => docs forward m

    // added here to prevent errors when client sends this repeatedly (e.g. as a keepalive
    case ConnectionInfoReq =>
      sender() ! ConnectionInfo()
  }

}
object Project {
  def apply(target: ActorRef)(implicit config: EnsimeConfig): Props =
    Props(classOf[Project], target, config)
}
