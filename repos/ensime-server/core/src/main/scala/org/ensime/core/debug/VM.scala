// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import akka.actor.ActorRef
import com.sun.jdi.request.{ EventRequest, EventRequestManager }
import com.sun.jdi._
import org.ensime.api._
import org.slf4j.LoggerFactory
import org.ensime.util.file._

import scala.collection.mutable.ListBuffer
import scala.collection.{ Iterable, mutable }

class VM(val mode: VmMode, vmOptions: List[String], debugManager: ActorRef, broadcaster: ActorRef, sourceMap: SourceMap) {
  val log = LoggerFactory.getLogger("DebugVM")

  import scala.collection.JavaConversions._

  private val vm: VirtualMachine = {
    mode match {
      case VmStart(commandLine) ⇒
        val connector = Bootstrap.virtualMachineManager().defaultConnector()
        val arguments = connector.defaultArguments()

        val opts = arguments.get("options").value
        val allVMOpts = (List(opts) ++ vmOptions).mkString(" ")
        arguments.get("options").setValue(allVMOpts)
        arguments.get("main").setValue(commandLine)
        // set the debugged process into suspend mode so we can catch it and add
        // breakpoints (see vm start  event), otherwise we have a race condition.
        arguments.get("suspend").setValue("true")

        log.info("Using Connector: " + connector.name + " : " + connector.description())
        log.info("Connector class: " + connector.getClass.getName)
        log.info("Debugger VM args: " + allVMOpts)
        log.info("Debugger program args: " + commandLine)
        connector.launch(arguments)
      case VmAttach(hostname, port) ⇒
        log.info("Attach to running vm")

        val vmm = Bootstrap.virtualMachineManager()
        val connector = vmm.attachingConnectors().get(0)

        val env = connector.defaultArguments()
        env.get("port").setValue(port)
        env.get("hostname").setValue(hostname)

        log.info("Using Connector: " + connector.name + " : " + connector.description())
        log.info("Debugger arguments: " + env)
        log.info("Attach to VM")
        val vm = connector.attach(env)
        log.info("VM: " + vm.description + ", " + vm)
        // if the remote VM has been started in suspended state, we need to nudge it
        // if the remote VM has been started in running state, this call seems to be a no-op
        vm.resume()
        vm
    }
  }

  //This flag is useful for debugging but not needed during general use
  // vm.setDebugTraceMode(VirtualMachine.TRACE_EVENTS)
  val evtQ = new VMEventManager(vm.eventQueue(), debugManager)
  val erm: EventRequestManager = vm.eventRequestManager();
  {
    val req = erm.createClassPrepareRequest()
    req.setSuspendPolicy(EventRequest.SUSPEND_ALL)
    req.enable()
  }
  {
    val req = erm.createThreadStartRequest()
    req.setSuspendPolicy(EventRequest.SUSPEND_NONE)
    req.enable()
  }
  {
    val req = erm.createThreadDeathRequest()
    req.setSuspendPolicy(EventRequest.SUSPEND_NONE)
    req.enable()
  }
  {
    val req = erm.createExceptionRequest(null, false, true)
    req.setSuspendPolicy(EventRequest.SUSPEND_ALL)
    req.enable()
  }

  private val fileToUnits = mutable.HashMap[String, mutable.HashSet[ReferenceType]]()
  private val process = vm.process()
  private val monitor = mode match {
    case VmAttach(_, _) => Nil
    case VmStart(_) => List(
      new MonitorOutput(process.getErrorStream, broadcaster),
      new MonitorOutput(process.getInputStream, broadcaster)
    )
  }
  private val savedObjects = new mutable.HashMap[DebugObjectId, ObjectReference]()

  def start(): Unit = {
    evtQ.start()
    monitor.foreach { _.start() }
  }

  def exit(exitCode: Int): Unit = {
    vm.exit(exitCode)
  }

  def dispose() = try {
    evtQ.finished = true
    vm.dispose()
    monitor.foreach { _.finished = true }
  } catch {
    case e: VMDisconnectedException =>
  }

  def remember(value: Value): Value = {
    value match {
      case v: ObjectReference => remember(v)
      case _ => value
    }
  }

  def remember(v: ObjectReference): ObjectReference = {
    savedObjects(DebugObjectId(v.uniqueID)) = v
    v
  }

  def resume(): Unit = {
    vm.resume()
  }

  def newStepRequest(thread: ThreadReference, stride: Int, depth: Int): Unit = {
    erm.deleteEventRequests(erm.stepRequests)
    val request = erm.createStepRequest(
      thread,
      stride,
      depth
    )
    request.addCountFilter(1)
    request.enable()
    vm.resume()
  }

  def bgMessage(msg: String): Unit = {
    broadcaster ! SendBackgroundMessageEvent(msg)
  }

  def setBreakpoint(file: File, line: Int): Boolean = {
    val locs = locations(file, line)
    if (locs.nonEmpty) {
      bgMessage(s"Resolved breakpoint at: $file : $line")
      bgMessage(s"Installing breakpoint at locations: $locs")
      for (loc <- locs) {
        val request = erm.createBreakpointRequest(loc)
        request.setSuspendPolicy(EventRequest.SUSPEND_ALL)
        request.enable()
      }
      true
    } else {
      false
    }
  }

  def clearAllBreakpoints(): Unit = {
    erm.deleteAllBreakpoints()
  }

  def clearBreakpoints(bps: Iterable[Breakpoint]): Unit = {
    for (bp <- bps) {
      for (
        req <- erm.breakpointRequests();
        pos <- sourceMap.locToPos(req.location())
      ) {
        if (pos.file == bp.file && pos.line == bp.line) {
          req.disable()
        }
      }
    }
  }

  def typeAdded(t: ReferenceType): Unit = {
    try {
      val key = t.sourceName
      val types = fileToUnits.getOrElse(key, mutable.HashSet[ReferenceType]())
      types += t
      fileToUnits(key) = types
    } catch {
      case e: AbsentInformationException =>
        log.info(s"No location information available for: ${t.name()}")
    }
  }

  def initLocationMap() = {
    for (t <- vm.allClasses) {
      typeAdded(t)
    }
  }

  def locations(file: File, line: Int): Set[Location] = {

    // Group locations by file and line
    case class LocationClass(loc: Location) {
      override def equals(that: Any): Boolean = that match {
        case that: Location =>
          loc.sourcePath == that.sourcePath &&
            loc.sourceName == that.sourceName &&
            loc.lineNumber == that.lineNumber
        case _ => false
      }
      override def hashCode: Int = loc.lineNumber.hashCode ^ loc.sourceName.hashCode
    }

    val buf = mutable.HashSet[LocationClass]()
    val key = file.getName
    for (types <- fileToUnits.get(key)) {
      for (t <- types) {
        for (m <- t.methods()) {
          try { buf ++= m.locationsOfLine(line).map(LocationClass.apply) } catch {
            case e: AbsentInformationException =>
          }
        }
        try { buf ++= t.locationsOfLine(line).map(LocationClass.apply) } catch {
          case e: AbsentInformationException =>
        }
      }
    }
    buf.map(_.loc).toSet
  }

  def threadById(id: DebugThreadId): Option[ThreadReference] = {
    vm.allThreads().find(t => t.uniqueID == id.id)
  }

  // Helper as Value.toString doesn't give
  // us what we want...
  def valueSummary(value: Value): String = {
    value match {
      case v: BooleanValue => v.value().toString
      case v: ByteValue => v.value().toString
      case v: CharValue => "'" + v.value().toString + "'"
      case v: DoubleValue => v.value().toString
      case v: FloatValue => v.value().toString
      case v: IntegerValue => v.value().toString
      case v: LongValue => v.value().toString
      case v: ShortValue => v.value().toString
      case v: VoidValue => "void"
      case v: StringReference => "\"" + v.value() + "\""
      case v: ArrayReference =>
        val length = v.length()
        if (length > 3)
          "Array[" + v.getValues(0, 3).map(valueSummary).mkString(", ") + ",...]"
        else
          "Array[" + v.getValues.map(valueSummary).mkString(", ") + "]"
      case v: ObjectReference =>
        val tpe = v.referenceType()
        if (tpe.name().matches("^scala\\.runtime\\.[A-Z][a-z]+Ref$")) {
          val elemField = tpe.fieldByName("elem")
          valueSummary(v.getValue(elemField))
        } else "Instance of " + lastNameComponent(v.referenceType().name())
      case _ => "NA"
    }
  }

  private def lastNameComponent(s: String): String = {
    "^.*?\\.([^\\.]+)$".r.findFirstMatchIn(s) match {
      case Some(m) => m.group(1)
      case None => s
    }
  }

  private def makeFields(
    tpeIn: ReferenceType,
    obj: ObjectReference
  ): List[DebugClassField] = {
    tpeIn match {
      case tpeIn: ClassType =>
        var fields = List[DebugClassField]()
        var tpe = tpeIn
        while (tpe != null) {
          var i = -1
          fields = tpe.fields().map { f =>
            i += 1
            val value = obj.getValue(f)
            DebugClassField(
              i, f.name(),
              f.typeName(),
              valueSummary(value)
            )
          }.toList ++ fields
          tpe = tpe.superclass
        }
        fields
      case _ => List.empty
    }
  }

  private def fieldByName(obj: ObjectReference, name: String): Option[Field] = {
    val tpeIn = obj.referenceType
    tpeIn match {
      case tpeIn: ClassType =>
        var result: Option[Field] = None
        var tpe = tpeIn
        while (tpe != null && result.isEmpty) {
          for (f <- tpe.fields()) {
            if (f.name() == name) result = Some(f)
          }
          tpe = tpe.superclass
        }
        result
      case _ => None
    }
  }

  private def makeDebugObj(value: ObjectReference): DebugObjectInstance = {
    DebugObjectInstance(
      valueSummary(value),
      makeFields(value.referenceType(), value),
      value.referenceType().name(),
      DebugObjectId(value.uniqueID())
    )
  }

  private def makeDebugStr(value: StringReference): DebugStringInstance = {
    DebugStringInstance(
      valueSummary(value),
      makeFields(value.referenceType(), value),
      value.referenceType().name(),
      DebugObjectId(value.uniqueID())
    )
  }

  private def makeDebugArr(value: ArrayReference): DebugArrayInstance = {
    DebugArrayInstance(
      value.length,
      value.referenceType().name,
      value.referenceType().asInstanceOf[ArrayType].componentTypeName(),
      DebugObjectId(value.uniqueID)
    )
  }

  private def makeDebugPrim(value: PrimitiveValue): DebugPrimitiveValue = DebugPrimitiveValue(
    valueSummary(value),
    value.`type`().name()
  )

  private def makeDebugNull(): DebugNullValue = DebugNullValue("Null")

  private def makeDebugValue(value: Value): DebugValue = {
    if (value == null) makeDebugNull()
    else {
      value match {
        case v: ArrayReference => makeDebugArr(v)
        case v: StringReference => makeDebugStr(v)
        case v: ObjectReference => makeDebugObj(v)
        case v: PrimitiveValue => makeDebugPrim(v)
      }
    }
  }

  def locationForName(thread: ThreadReference, name: String): Option[DebugLocation] = {
    val stackFrame = thread.frame(0)
    val objRef = stackFrame.thisObject()
    if (name == "this") {
      Some(DebugObjectReference(remember(objRef).uniqueID))
    } else {
      stackSlotForName(thread, name).map({ slot =>
        DebugStackSlot(DebugThreadId(thread.uniqueID), slot.frame, slot.offset)
      }).orElse(
        fieldByName(objRef, name).flatMap { f =>
          Some(DebugObjectField(DebugObjectId(objRef.uniqueID), f.name))
        }
      )
    }
  }

  private def valueAtLocation(location: DebugLocation): Option[Value] = {
    location match {
      case DebugObjectReference(objId) =>
        valueForId(objId)
      case DebugObjectField(objectId, name) =>
        valueForField(objectId, name)
      case DebugArrayElement(objectId, index) =>
        valueForIndex(objectId, index)
      case DebugStackSlot(threadId, frame, offset) =>
        threadById(threadId) match {
          case Some(thread) =>
            valueForStackVar(thread, frame, offset)
          case None => None
        }
    }
  }

  def debugValueAtLocation(location: DebugLocation): Option[DebugValue] = {
    valueAtLocation(location).map(makeDebugValue)
  }

  private def callMethod(thread: ThreadReference, obj: ObjectReference, name: String, signature: String, args: java.util.List[Value]): Option[Value] = {
    if (!vm.canBeModified) {
      log.info("Sorry, this debug VM is read-only.")
      None
    } else {
      log.info("DebugManager.callMethod(obj = " + obj + " of type " + obj.referenceType + ", name = " +
        name + ", signature = " + signature + ", args = " + args)
      obj.referenceType.methodsByName("toString", "()Ljava/lang/String;").headOption match {
        case Some(m) =>
          log.info("Invoking: " + m)
          Some(obj.invokeMethod(thread, m, args, ObjectReference.INVOKE_SINGLE_THREADED))
        case other =>
          log.error("toString method not found: " + other)
          None
      }
    }
  }

  def debugValueAtLocationToString(threadId: DebugThreadId, location: DebugLocation): Option[String] = {
    valueAtLocation(location) match {
      case Some(arr: ArrayReference) =>
        val quantifier = if (arr.length == 1) "element" else "elements"
        Some("<array of " + arr.length + " " + quantifier + ">")
      case Some(str: StringReference) =>
        Some(str.value)
      case Some(obj: ObjectReference) =>
        threadById(threadId) flatMap { thread =>
          callMethod(thread, obj, "toString", "()Ljava/lang/String;", new java.util.Vector()) match {
            case Some(v: StringReference) =>
              Some(v.value)
            case Some(null) => Some("null")
            case _ => None
          }
        }
      case Some(value) => Some(valueSummary(value))
      case _ =>
        log.info("No value found at location.")
        None
    }
  }

  private def valueForId(objectId: DebugObjectId): Option[ObjectReference] = {
    savedObjects.get(objectId)
  }

  private def valueForField(objectId: DebugObjectId, name: String): Option[Value] = {
    for (
      obj <- savedObjects.get(objectId);
      f <- fieldByName(obj, name)
    ) yield {
      remember(obj.getValue(f))
    }
  }

  private def valueForIndex(objectId: DebugObjectId, index: Int): Option[Value] = {
    savedObjects.get(objectId) match {
      case Some(arr: ArrayReference) => Some(remember(arr.getValue(index)))
      case _ => None
    }
  }

  private def valueForStackVar(thread: ThreadReference, frame: Int, offset: Int): Option[Value] = {
    if (thread.frameCount > frame &&
      thread.frame(frame).visibleVariables.length > offset) {
      val stackFrame = thread.frame(frame)
      val value = stackFrame.getValue(stackFrame.visibleVariables.get(offset))
      Some(remember(value))
    } else None
  }

  case class StackSlot(frame: Int, offset: Int)

  private def stackSlotForName(thread: ThreadReference, name: String): Option[StackSlot] = {
    var result: Option[StackSlot] = None
    var frame = 0
    while (result.isEmpty && frame < thread.frameCount) {
      val stackFrame = thread.frame(frame)
      val visVars = stackFrame.visibleVariables()
      var offset = 0
      while (offset < visVars.length) {
        if (visVars(offset).name == name) {
          result = Some(StackSlot(frame, offset))
        }
        offset += 1
      }
      frame += 1
    }
    result
  }

  def ignoreErr[T](action: => T, orElse: => T): T = {
    try { action } catch { case e: Exception => orElse }
  }

  private def makeStackFrame(index: Int, frame: StackFrame): DebugStackFrame = {
    val locals = ignoreErr({
      frame.visibleVariables.zipWithIndex.map {
        case (v, i) =>
          DebugStackLocal(i, v.name,
            valueSummary(frame.getValue(v)),
            v.typeName())
      }.toList
    }, List.empty)

    val numArgs = ignoreErr(frame.getArgumentValues.length, 0)
    val methodName = ignoreErr(frame.location.method().name(), "Method")
    val className = ignoreErr(frame.location.declaringType().name(), "Class")
    val pcLocation = sourceMap.locToPos(frame.location).getOrElse(
      LineSourcePosition(
        File(frame.location.sourcePath()).canon,
        frame.location.lineNumber
      )
    )
    val thisObjId = ignoreErr(remember(frame.thisObject()).uniqueID, -1L)
    DebugStackFrame(index, locals, numArgs, className, methodName, pcLocation, DebugObjectId(thisObjId))
  }

  def backtrace(thread: ThreadReference, index: Int, count: Int): DebugBacktrace = {
    val frames = ListBuffer[DebugStackFrame]()
    var i = index
    while (i < thread.frameCount && (count == -1 || i < count)) {
      val stackFrame = thread.frame(i)
      frames += makeStackFrame(i, stackFrame)
      i += 1
    }
    DebugBacktrace(frames.toList, DebugThreadId(thread.uniqueID()), thread.name())
  }

  private def mirrorFromString(tpe: Type, toMirror: String): Option[Value] = {
    val s = toMirror.trim
    if (s.length > 0) {
      tpe match {
        case tpe: BooleanType => Some(vm.mirrorOf(s.toBoolean))
        case tpe: ByteType => Some(vm.mirrorOf(s.toByte))
        case tpe: CharType => Some(vm.mirrorOf(s(0)))
        case tpe: DoubleType => Some(vm.mirrorOf(s.toDouble))
        case tpe: FloatType => Some(vm.mirrorOf(s.toFloat))
        case tpe: IntegerType => Some(vm.mirrorOf(s.toInt))
        case tpe: LongType => Some(vm.mirrorOf(s.toLong))
        case tpe: ShortType => Some(vm.mirrorOf(s.toShort))
        case tpe: ReferenceType if tpe.name == "java.lang.String" =>
          if (s.startsWith("\"") && s.endsWith("\"")) {
            Some(vm.mirrorOf(s.substring(1, s.length - 1)))
          } else Some(vm.mirrorOf(s))
        case _ => None
      }
    } else None
  }

  def setStackVar(thread: ThreadReference, frame: Int, offset: Int,
    newValue: String): Boolean = {
    if (thread.frameCount > frame &&
      thread.frame(frame).visibleVariables.length > offset) {
      val stackFrame = thread.frame(frame)
      val localVar: LocalVariable = stackFrame.visibleVariables.get(offset)
      mirrorFromString(localVar.`type`(), newValue) match {
        case Some(v) =>
          stackFrame.setValue(localVar, v); true
        case None => false
      }
    } else false
  }

}
