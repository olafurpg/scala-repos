// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import java.io.File

import akka.actor._
import akka.event.LoggingReceive
import com.sun.jdi._
import com.sun.jdi.event._
import com.sun.jdi.request.StepRequest
import org.ensime.api._

import scala.collection.mutable

case class DMClassPrepareEvent(prepareEvent: ClassPrepareEvent, eventSet: EventSet)

object DebugManager {
  def apply(
    broadcaster: ActorRef
  )(
    implicit
    config: EnsimeConfig
  ): Props = Props(new DebugManager(broadcaster, config))
}

class DebugManager(
    broadcaster: ActorRef,
    config: EnsimeConfig
) extends Actor with ActorLogging {

  // TODO this is built once on startup - probably makes sense for it to be done each time a debug vm is created
  private var sourceMap = new SourceMap(config)

  private var activeBreakpoints = Set[Breakpoint]()

  // These vals should be vars to make it more obvious that we are managing state
  // Map unqualified file names to sets of fully qualified paths.
  private val pendingBreaksBySourceName =
    new mutable.HashMap[String, mutable.HashSet[Breakpoint]].withDefault { _ => new mutable.HashSet }
  private var maybeVM: Option[VM] = None

  def tryPendingBreaksForSourcename(sourceName: String): Unit = {
    for (breaks <- pendingBreaksBySourceName.get(sourceName)) {
      val toTry = mutable.HashSet() ++ breaks
      for (bp <- toTry) {
        setBreakpoint(bp.file, bp.line)
      }
    }
  }

  def setBreakpoint(file: File, line: Int): Boolean = {
    val applied = maybeVM.exists { vm => vm.setBreakpoint(file, line) }
    if (applied) {
      activeBreakpoints += Breakpoint(file, line)
      true
    } else {
      addPendingBreakpoint(Breakpoint(file, line))
      false
    }
  }

  def clearBreakpoint(file: File, line: Int): Unit = {
    val clearBp = Breakpoint(file, line)
    for (bps <- pendingBreaksBySourceName.get(file.getName)) {
      bps.retain { _ != clearBp }
    }
    val toRemove = activeBreakpoints.filter { _ == clearBp }
    for (vm <- maybeVM) {
      vm.clearBreakpoints(toRemove)
    }
    activeBreakpoints --= toRemove
  }

  def clearAllBreakpoints(): Unit = {
    pendingBreaksBySourceName.clear()
    activeBreakpoints = Set.empty
    for (vm <- maybeVM) {
      vm.clearAllBreakpoints()
    }
  }

  def moveActiveBreaksToPending(): Unit = {
    activeBreakpoints.foreach(addPendingBreakpoint)
    activeBreakpoints = Set.empty
  }

  def addPendingBreakpoint(bp: Breakpoint): Unit = {
    val file = bp.file
    val breaks = pendingBreaksBySourceName.getOrElse(file.getName, mutable.HashSet())
    breaks.add(bp)
    pendingBreaksBySourceName(file.getName) = breaks
  }

  def pendingBreakpoints: List[Breakpoint] = {
    pendingBreaksBySourceName.values.flatten.toList
  }

  def disconnectDebugVM(): Unit = {
    withVM { vm =>
      vm.dispose()
    }
    moveActiveBreaksToPending()
    maybeVM = None
    broadcaster ! DebugVMDisconnectEvent
  }

  def vmOptions(): List[String] = List(
    "-classpath",
    config.runtimeClasspath.mkString("\"", File.pathSeparator, "\"")
  ) ++ config.debugVMArgs

  def withVM[T](action: (VM => T)): Option[T] = {
    maybeVM.synchronized {
      try {
        for (vm <- maybeVM) yield {
          action(vm)
        }
      } catch {
        case e: VMDisconnectedException =>
          log.error(e, "Attempted interaction with disconnected VM:")
          disconnectDebugVM()
          None
        case e: Throwable =>
          log.error(e, "Exception thrown whilst handling vm action")
          None
      }
    }
  }

  private def handleRPCWithVM()(action: (VM => RpcResponse)): RpcResponse = {
    withVM { vm =>
      action(vm)
    }.getOrElse {
      log.warning("Could not access debug VM.")
      FalseResponse
    }
  }

  private def handleRPCWithVMAndThread(threadId: DebugThreadId)(action: ((VM, ThreadReference) => RpcResponse)): RpcResponse = {
    withVM { vm =>
      (for (thread <- vm.threadById(threadId)) yield {
        action(vm, thread)
      }).getOrElse {
        log.warning(s"Could not find thread: $threadId")
        FalseResponse
      }
    }.getOrElse {
      log.warning("Could not access debug VM")
      FalseResponse
    }
  }

  def bgMessage(msg: String): Unit = {
    broadcaster ! SendBackgroundMessageEvent(msg)
  }

  // the JVM should have its own actor
  def receive: Receive = LoggingReceive { fromJvm orElse fromUser }

  def fromJvm: Receive = {
    case DebuggerShutdownEvent =>
      withVM { vm =>
        vm.dispose()
      }
      context.stop(self)

    case DMClassPrepareEvent(prepareEvent, eventSet) =>
      withVM { vm =>
        val refType = prepareEvent.referenceType()
        vm.typeAdded(refType)
        tryPendingBreaksForSourcename(refType.sourceName())
      }
      eventSet.resume()

    case e: VMStartEvent =>
      withVM { vm =>
        vm.initLocationMap()
        // by default we will attempt to start in suspended mode so we can attach breakpoints etc
        vm.resume()
      }
      broadcaster ! DebugVMStartEvent
    case e: VMDisconnectedException =>
      disconnectDebugVM()
    case e: VMDeathEvent =>
      disconnectDebugVM()
    case e: VMDisconnectEvent =>
      disconnectDebugVM()
    case e: StepEvent =>
      (for (pos <- sourceMap.locToPos(e.location())) yield {
        broadcaster ! DebugStepEvent(DebugThreadId(e.thread().uniqueID()), e.thread().name, pos.file, pos.line)
      }) getOrElse {
        val loc = e.location()
        log.warning(s"Step position not found: ${loc.sourceName()} : ${loc.lineNumber()}")
      }
    case e: BreakpointEvent =>
      (for (pos <- sourceMap.locToPos(e.location())) yield {
        broadcaster ! DebugBreakEvent(DebugThreadId(e.thread().uniqueID()), e.thread().name, pos.file, pos.line)
      }) getOrElse {
        val loc = e.location()
        log.warning(s"Break position not found: ${loc.sourceName()} : ${loc.lineNumber()}")
      }
    case e: ExceptionEvent =>
      withVM { vm => vm.remember(e.exception) }

      val pos = if (e.catchLocation() != null) sourceMap.locToPos(e.catchLocation()) else None
      broadcaster ! DebugExceptionEvent(
        e.exception.uniqueID(),
        DebugThreadId(e.thread().uniqueID()),
        e.thread().name,
        pos.map(_.file),
        pos.map(_.line)
      )
    case e: ThreadDeathEvent =>
      broadcaster ! DebugThreadDeathEvent(DebugThreadId(e.thread().uniqueID()))
    case e: ThreadStartEvent =>
      broadcaster ! DebugThreadStartEvent(DebugThreadId(e.thread().uniqueID()))
    case e: AccessWatchpointEvent =>
    case e: ClassPrepareEvent =>
      withVM { vm =>
        log.info(s"ClassPrepareEvent: ${e.referenceType().name()}")
      }
    case e: ClassUnloadEvent =>
    case e: MethodEntryEvent =>
    case e: MethodExitEvent =>
  }

  def disposeCurrentVM(): Unit = {
    withVM { vm =>
      vm.dispose()
    }
  }
  def handleStartupFailure(e: Exception): RpcResponse = {
    maybeVM = None
    log.error(e, "Failure during VM startup")
    val message = e.toString
    DebugVmError(1, message)
  }

  def handleDebugStartReq(commandLine: String): RpcResponse = {
    disposeCurrentVM()
    try {
      val vm = new VM(VmStart(commandLine), vmOptions(), self, broadcaster, sourceMap)
      maybeVM = Some(vm)
      vm.start()
      DebugVmSuccess()
    } catch {
      case e: Exception =>
        log.error(e, "Could not start VM")
        handleStartupFailure(e)
    }
  }

  def handleDebugAttachReq(hostname: String, port: String): RpcResponse = {
    disposeCurrentVM()
    try {
      val vm = new VM(VmAttach(hostname, port), vmOptions(), self, broadcaster, sourceMap)
      maybeVM = Some(vm)
      vm.start()
      DebugVmSuccess()
    } catch {
      case e: Exception =>
        log.error(e, "Could not attach VM")
        handleStartupFailure(e)
    }
  }

  def fromUser: Receive = {
    case DebugStartReq(commandLine: String) =>
      sender ! handleDebugStartReq(commandLine)
    case DebugAttachReq(hostname, port) ⇒
      sender ! handleDebugAttachReq(hostname, port)
    case DebugActiveVmReq =>
      sender ! handleRPCWithVM() { vm =>
        TrueResponse
      }
    case DebugStopReq =>
      sender ! handleRPCWithVM() { vm =>
        if (vm.mode.shouldExit) {
          vm.exit(0)
        }
        vm.dispose()
        TrueResponse
      }
    case DebugRunReq =>
      sender ! handleRPCWithVM() { vm =>
        vm.resume()
        TrueResponse
      }
    case DebugContinueReq(threadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.resume()
          TrueResponse
      }
    case DebugSetBreakReq(file, line: Int) =>
      if (!setBreakpoint(file, line)) {
        bgMessage("Location not loaded. Set pending breakpoint.")
      }
      sender ! TrueResponse
    case DebugClearBreakReq(file, line: Int) =>
      clearBreakpoint(file, line)
      sender ! TrueResponse

    case DebugClearAllBreaksReq =>
      clearAllBreakpoints()
      sender ! TrueResponse

    case DebugListBreakpointsReq =>
      val breaks = BreakpointList(activeBreakpoints.toList, pendingBreakpoints)
      sender ! breaks

    case DebugNextReq(threadId: DebugThreadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.newStepRequest(
            thread,
            StepRequest.STEP_LINE,
            StepRequest.STEP_OVER
          )
          TrueResponse
      }

    case DebugStepReq(threadId: DebugThreadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.newStepRequest(
            thread,
            StepRequest.STEP_LINE,
            StepRequest.STEP_INTO
          )
          TrueResponse
      }

    case DebugStepOutReq(threadId: DebugThreadId) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.newStepRequest(
            thread,
            StepRequest.STEP_LINE,
            StepRequest.STEP_OUT
          )
          TrueResponse
      }

    case DebugLocateNameReq(threadId: DebugThreadId, name: String) =>
      sender ! handleRPCWithVMAndThread(threadId) {
        (vm, thread) =>
          vm.locationForName(thread, name).getOrElse(FalseResponse)
      }
    case DebugBacktraceReq(threadId: DebugThreadId, index: Int, count: Int) =>
      sender ! handleRPCWithVMAndThread(threadId) { (vm, thread) =>
        vm.backtrace(thread, index, count)
      }
    case DebugValueReq(location) =>
      sender ! handleRPCWithVM() { vm =>
        vm.debugValueAtLocation(location).getOrElse(FalseResponse)
      }
    case DebugToStringReq(threadId, location) =>
      sender ! handleRPCWithVM() { vm =>
        vm.debugValueAtLocationToString(threadId, location) match {
          case Some(strValue) => StringResponse(strValue)
          case None => FalseResponse
        }
      }

    case DebugSetValueReq(location, newValue) =>
      sender ! handleRPCWithVM() { vm =>
        location match {
          case DebugStackSlot(threadId, frame, offset) => vm.threadById(threadId) match {
            case Some(thread) =>
              val status = vm.setStackVar(thread, frame, offset, newValue)
              status match {
                case true =>
                  TrueResponse
                case false =>
                  FalseResponse
              }
            case _ =>
              log.error(s"Unknown thread $threadId for debug-set-value")
              FalseResponse
          }
          case unknown =>
            log.error(s"Unsupported location type for debug-set-value.: $unknown")
            FalseResponse
        }
      }
  }
}
