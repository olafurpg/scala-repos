// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.jerk

import org.ensime.api._
import org.ensime.util.{ EnsimeSpec, EscapingStringInterpolation }

class JerkFormatsSpec extends EnsimeSpec with SprayJsonTestSupport with EnsimeTestData {

  import JerkFormats._
  import JerkEnvelopeFormats._

  import EscapingStringInterpolation._

  "Jerk Formats" should "roundtrip request envelopes" in {
    roundtrip(
      RpcRequestEnvelope(ConnectionInfoReq, 13),
      """{"callId":13,"req":{"typehint":"ConnectionInfoReq"}}"""
    )
  }

  it should "roundtrip RPC response envelopes" in {
    roundtrip(
      RpcResponseEnvelope(Some(13), sourcePos3),
      """{"callId":13,"payload":{"typehint":"EmptySourcePosition"}}"""
    )
  }

  it should "roundtrip async response envelopes" in {
    roundtrip(
      RpcResponseEnvelope(None, SendBackgroundMessageEvent("ABCDEF", 1)),
      """{"payload":{"typehint":"SendBackgroundMessageEvent","detail":"ABCDEF","code":1}}"""
    )
  }

  it should "roundtrip startup messages" in {
    roundtrip(
      ConnectionInfoReq: RpcRequest,
      """{"typehint":"ConnectionInfoReq"}"""
    )
  }

  it should "unmarshal RpcSearchRequests" in {
    roundtrip(
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest,
      """{"typehint":"PublicSymbolSearchReq","keywords":["foo","bar"],"maxResults":10}"""
    )

    roundtrip(
      ImportSuggestionsReq(Left(file1), 1, List("foo", "bar"), 10): RpcRequest,
      s"""{"point":1,"maxResults":10,"names":["foo","bar"],"typehint":"ImportSuggestionsReq","file":"$file1"}"""
    )
  }

  it should "unmarshal RpcAnalyserRequests" in {
    roundtrip(
      RemoveFileReq(file1): RpcRequest,
      s"""{"typehint":"RemoveFileReq","file":"$file1"}"""
    )

    roundtrip(
      TypecheckFileReq(sourceFileInfo): RpcRequest,
      s"""{"typehint":"TypecheckFileReq","fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"}}"""
    )

    roundtrip(
      TypecheckFilesReq(List(Left(file1), Left(file2))): RpcRequest,
      s"""{"typehint":"TypecheckFilesReq","files":["$file1","$file2"]}"""
    )

    roundtrip(
      UnloadAllReq: RpcRequest,
      """{"typehint":"UnloadAllReq"}"""
    )

    roundtrip(
      TypecheckAllReq: RpcRequest,
      """{"typehint":"TypecheckAllReq"}"""
    )

    roundtrip(
      FormatSourceReq(List(file1, file2)): RpcRequest,
      s"""{"typehint":"FormatSourceReq","files":["$file1","$file2"]}"""
    )

    roundtrip(
      FormatOneSourceReq(sourceFileInfo): RpcRequest,
      s"""{"typehint":"FormatOneSourceReq","file":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"}}"""
    )

    roundtrip(
      DocUriAtPointReq(Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""{"typehint":"DocUriAtPointReq","file":"$file1","point":{"from":1,"to":10}}"""
    )

    roundtrip(
      DocUriForSymbolReq("foo.bar", Some("Baz"), None): RpcRequest,
      """{"typehint":"DocUriForSymbolReq","typeFullName":"foo.bar","memberName":"Baz"}"""
    )

    roundtrip(
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest,
      s"""{"point":10,"maxResults":100,"typehint":"CompletionsReq","caseSens":true,"fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"},"reload":false}"""
    )

    roundtrip(
      PackageMemberCompletionReq("foo", "bar"): RpcRequest,
      """{"typehint":"PackageMemberCompletionReq","path":"foo","prefix":"bar"}"""
    )

    roundtrip(
      UsesOfSymbolAtPointReq(Left(file1), 100): RpcRequest,
      s"""{"typehint":"UsesOfSymbolAtPointReq","file":"$file1","point":100}"""
    )

    roundtrip(
      TypeByNameReq("foo.bar"): RpcRequest,
      """{"typehint":"TypeByNameReq","name":"foo.bar"}"""
    )

    roundtrip(
      TypeByNameAtPointReq("foo.bar", Left(file1), OffsetRange(1, 10)): RpcRequest,
      s"""{"typehint":"TypeByNameAtPointReq","name":"foo.bar","file":"$file1","range":{"from":1,"to":10}}"""
    )

    roundtrip(
      TypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""{"typehint":"TypeAtPointReq","file":"$file1","range":{"from":1,"to":100}}"""
    )

    roundtrip(
      InspectTypeAtPointReq(Left(file1), OffsetRange(1, 100)): RpcRequest,
      s"""{"typehint":"InspectTypeAtPointReq","file":"$file1","range":{"from":1,"to":100}}"""
    )

    roundtrip(
      InspectTypeByNameReq("foo.Bar"): RpcRequest,
      """{"typehint":"InspectTypeByNameReq","name":"foo.Bar"}"""
    )

    roundtrip(
      SymbolAtPointReq(Left(file1), 101): RpcRequest,
      s"""{"typehint":"SymbolAtPointReq","file":"$file1","point":101}"""
    )

    roundtrip(
      SymbolByNameReq("foo.Bar", Some("baz"), None): RpcRequest,
      """{"typehint":"SymbolByNameReq","typeFullName":"foo.Bar","memberName":"baz"}"""
    )

    roundtrip(
      InspectPackageByPathReq("foo.bar"): RpcRequest,
      """{"typehint":"InspectPackageByPathReq","path":"foo.bar"}"""
    )

    roundtrip(
      PrepareRefactorReq(1, 'ignored, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""{"tpe":"ignored","procId":1,"params":{"newName":"bar","typehint":"RenameRefactorDesc","end":100,"file":"$file1","start":1},"typehint":"PrepareRefactorReq","interactive":false}"""
    )

    roundtrip(
      ExecRefactorReq(1, RefactorType.Rename): RpcRequest,
      """{"typehint":"ExecRefactorReq","procId":1,"tpe":{"typehint":"Rename"}}"""
    )

    roundtrip(
      CancelRefactorReq(1): RpcRequest,
      """{"typehint":"CancelRefactorReq","procId":1}"""
    )

    roundtrip(
      RefactorReq(1, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest,
      s"""{"procId":1,"params":{"newName":"bar","typehint":"RenameRefactorDesc","end":100,"file":"$file1","start":1},"typehint":"RefactorReq","interactive":false}"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Left(file1), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""{"requestedTypes":[{"typehint":"ObjectSymbol"},{"typehint":"ValSymbol"}],"typehint":"SymbolDesignationsReq","end":100,"file":"$file1","start":1}"""
    )

    roundtrip(
      SymbolDesignationsReq(
        Right(SourceFileInfo(file1, None, None)), 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest,
      s"""{"requestedTypes":[{"typehint":"ObjectSymbol"},{"typehint":"ValSymbol"}],"typehint":"SymbolDesignationsReq","file":{"file":"$file1"}, "end":100,"start":1}"""
    )

    roundtrip(
      ExpandSelectionReq(file1, 100, 200): RpcRequest,
      s"""{"typehint":"ExpandSelectionReq","file":"$file1","start":100,"end":200}"""
    )

    roundtrip(
      ImplicitInfoReq(Left(file1), OffsetRange(0, 123)): RpcRequest,
      s"""{"typehint":"ImplicitInfoReq","file":"$file1","range":{"from":0,"to":123}}"""
    )

    roundtrip(
      StructureViewReq(sourceFileInfo): RpcRequest,
      s"""{"typehint":"StructureViewReq","fileInfo":{"file":"$file1","contents":"{/* code here */}","contentsIn":"$file2"}}"""
    )

  }

  it should "roundtrip RpcDebugRequests" in {
    roundtrip(
      DebugActiveVmReq: RpcRequest,
      """{"typehint":"DebugActiveVmReq"}"""
    )

    roundtrip(
      DebugStartReq("blah blah blah"): RpcRequest,
      """{"typehint":"DebugStartReq","commandLine":"blah blah blah"}"""
    )

    roundtrip(
      DebugAttachReq("mylovelyhorse", "13"): RpcRequest,
      """{"typehint":"DebugAttachReq","hostname":"mylovelyhorse","port":"13"}"""
    )

    roundtrip(
      DebugStopReq: RpcRequest,
      """{"typehint":"DebugStopReq"}"""
    )

    roundtrip(
      DebugSetBreakReq(file1, 13): RpcRequest,
      s"""{"typehint":"DebugSetBreakReq","file":"$file1","line":13}"""
    )

    roundtrip(
      DebugClearBreakReq(file1, 13): RpcRequest,
      s"""{"typehint":"DebugClearBreakReq","file":"$file1","line":13}"""
    )

    roundtrip(
      DebugClearAllBreaksReq: RpcRequest,
      """{"typehint":"DebugClearAllBreaksReq"}"""
    )

    roundtrip(
      DebugListBreakpointsReq: RpcRequest,
      """{"typehint":"DebugListBreakpointsReq"}"""
    )

    roundtrip(
      DebugRunReq: RpcRequest,
      """{"typehint":"DebugRunReq"}"""
    )

    roundtrip(
      DebugContinueReq(dtid): RpcRequest,
      """{"typehint":"DebugContinueReq","threadId":13}"""
    )

    roundtrip(
      DebugStepReq(dtid): RpcRequest,
      """{"typehint":"DebugStepReq","threadId":13}"""
    )

    roundtrip(
      DebugNextReq(dtid): RpcRequest,
      """{"typehint":"DebugNextReq","threadId":13}"""
    )

    roundtrip(
      DebugStepOutReq(dtid): RpcRequest,
      """{"typehint":"DebugStepOutReq","threadId":13}"""
    )

    roundtrip(
      DebugLocateNameReq(dtid, "foo"): RpcRequest,
      """{"typehint":"DebugLocateNameReq","threadId":13,"name":"foo"}"""
    )

    roundtrip(
      DebugValueReq(debugLocationArray): RpcRequest,
      """{"typehint":"DebugValueReq","loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14}}"""
    )

    roundtrip(
      DebugToStringReq(dtid, debugLocationArray): RpcRequest,
      """{"typehint":"DebugToStringReq","threadId":13,"loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14}}"""
    )

    roundtrip(
      DebugSetValueReq(debugLocationArray, "bar"): RpcRequest,
      """{"typehint":"DebugSetValueReq","loc":{"typehint":"DebugArrayElement","objectId":{"id":13},"index":14},"newValue":"bar"}"""
    )

    roundtrip(
      DebugBacktraceReq(dtid, 100, 200): RpcRequest,
      """{"typehint":"DebugBacktraceReq","threadId":13,"index":100,"count":200}"""
    )

  }

  it should "roundtrip EnsimeGeneralEvent as EnsimeEvent" in {
    roundtrip(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeServerMessage,
      """{"typehint":"SendBackgroundMessageEvent","detail":"ABCDEF","code":1}"""
    )

    roundtrip(
      AnalyzerReadyEvent: EnsimeServerMessage,
      """{"typehint":"AnalyzerReadyEvent"}"""
    )

    roundtrip(
      FullTypeCheckCompleteEvent: EnsimeServerMessage,
      """{"typehint":"FullTypeCheckCompleteEvent"}"""
    )

    roundtrip(
      IndexerReadyEvent: EnsimeServerMessage,
      """{"typehint":"IndexerReadyEvent"}"""
    )

    roundtrip(
      NewScalaNotesEvent(
        isFull = false,
        List(new Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeServerMessage,
      """{"typehint":"NewScalaNotesEvent","isFull":false,"notes":[{"beg":50,"line":77,"col":5,"end":55,"file":"foo.scala","msg":"testMsg","severity":{"typehint":"NoteWarn"}}]}"""
    )

    roundtrip(
      ClearAllScalaNotesEvent: EnsimeServerMessage,
      """{"typehint":"ClearAllScalaNotesEvent"}"""
    )
  }

  it should "roundtrip DebugEvent as EnsimeEvent" in {
    roundtrip(
      DebugOutputEvent("XXX"): EnsimeServerMessage,
      """{"typehint":"DebugOutputEvent","body":"XXX"}"""
    )

    roundtrip(
      DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeServerMessage,
      // why is the typehint not the first entry?
      s"""{"line":57,"typehint":"DebugStepEvent","file":"$file1","threadName":"threadNameStr","threadId":207}"""
    )

    roundtrip(
      DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeServerMessage,
      s"""{"line":57,"typehint":"DebugBreakEvent","file":"$file1","threadName":"threadNameStr","threadId":209}"""
    )

    roundtrip(
      DebugVMStartEvent: EnsimeServerMessage,
      """{"typehint":"DebugVMStartEvent"}"""
    )
    roundtrip(
      DebugVMDisconnectEvent: EnsimeServerMessage,
      """{"typehint":"DebugVMDisconnectEvent"}"""
    )
    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)): EnsimeServerMessage,
      s"""{"line":57,"exception":33,"typehint":"DebugExceptionEvent","file":"$file1","threadName":"threadNameStr","threadId":13}"""
    )
    roundtrip(
      DebugExceptionEvent(33L, dtid, "threadNameStr", None, None): EnsimeServerMessage,
      """{"typehint":"DebugExceptionEvent","exception":33,"threadId":13,"threadName":"threadNameStr"}"""
    )

    roundtrip(
      DebugThreadStartEvent(dtid): EnsimeServerMessage,
      """{"typehint":"DebugThreadStartEvent","threadId":13}"""
    )
    roundtrip(
      DebugThreadDeathEvent(dtid): EnsimeServerMessage,
      """{"typehint":"DebugThreadDeathEvent","threadId":13}"""
    )
  }

  it should "roundtrip DebugLocation" in {
    roundtrip(
      DebugObjectReference(57L): EnsimeServerMessage,
      """{"typehint":"DebugObjectReference","objectId":{"id":57}}"""
    )

    roundtrip(
      DebugArrayElement(DebugObjectId(58L), 2): EnsimeServerMessage,
      """{"typehint":"DebugArrayElement","objectId":{"id":58},"index":2}"""
    )

    roundtrip(
      DebugObjectField(DebugObjectId(58L), "fieldName"): EnsimeServerMessage,
      """{"typehint":"DebugObjectField","objectId":{"id":58},"field":"fieldName"}"""
    )

    roundtrip(
      DebugStackSlot(DebugThreadId(27), 12, 23): EnsimeServerMessage,
      """{"typehint":"DebugStackSlot","threadId":27,"frame":12,"offset":23}"""
    )
  }

  it should "roundtrip DebugValue" in {
    roundtrip(
      DebugPrimitiveValue("summaryStr", "typeNameStr"): EnsimeServerMessage,
      """{"typehint":"DebugPrimitiveValue","summary":"summaryStr","typeName":"typeNameStr"}"""
    )

    roundtrip(
      DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): EnsimeServerMessage,
      """{"typehint":"DebugStringInstance","typeName":"typeNameStr","fields":[{"index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}],"objectId":{"id":5},"summary":"summaryStr"}"""
    )

    roundtrip(
      DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): EnsimeServerMessage,
      """{"typehint":"DebugObjectInstance","typeName":"typeNameStr","fields":[{"index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}],"objectId":{"id":5},"summary":"summaryStr"}"""
    )

    roundtrip(
      DebugNullValue("typeNameStr"): EnsimeServerMessage,
      """{"typehint":"DebugNullValue","typeName":"typeNameStr"}"""
    )

    roundtrip(
      DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)): EnsimeServerMessage,
      """{"elementTypeName":"elementType","typehint":"DebugArrayInstance","typeName":"typeName","length":3,"objectId":{"id":5}}"""
    )

    roundtrip(
      debugClassField: EnsimeServerMessage,
      """{"typehint":"DebugClassField","index":19,"name":"nameStr","typeName":"typeNameStr","summary":"summaryStr"}"""
    )

    roundtrip(
      debugStackLocal1: EnsimeServerMessage,
      """{"typehint":"DebugStackLocal","index":3,"name":"name1","summary":"summary1","typeName":"type1"}"""
    )

    roundtrip(
      debugStackFrame: EnsimeServerMessage,
      s"""{"typehint":"DebugStackFrame","thisObjectId":{"id":7},"methodName":"method1","locals":[{"index":3,"name":"name1","summary":"summary1","typeName":"type1"},{"index":4,"name":"name2","summary":"summary2","typeName":"type2"}],"pcLocation":{"file":"$file1","line":57},"className":"class1","numArgs":4,"index":7}"""
    )

    roundtrip(
      DebugBacktrace(List(debugStackFrame), dtid, "thread1"): EnsimeServerMessage,
      s"""{"typehint":"DebugBacktrace","frames":[{"thisObjectId":{"id":7},"methodName":"method1","locals":[{"index":3,"name":"name1","summary":"summary1","typeName":"type1"},{"index":4,"name":"name2","summary":"summary2","typeName":"type2"}],"pcLocation":{"file":"$file1","line":57},"className":"class1","numArgs":4,"index":7}],"threadId":13,"threadName":"thread1"}"""
    )

    roundtrip(
      sourcePos1: EnsimeServerMessage,
      s"""{"typehint":"LineSourcePosition","file":"$file1","line":57}"""
    )
    roundtrip(
      sourcePos2: EnsimeServerMessage,
      s"""{"typehint":"LineSourcePosition","file":"$file1","line":59}"""
    )
    roundtrip(
      sourcePos3: EnsimeServerMessage,
      """{"typehint":"EmptySourcePosition"}"""
    )
    roundtrip(
      sourcePos4: EnsimeServerMessage,
      s"""{"typehint":"OffsetSourcePosition","file":"$file1","offset":456}"""
    )

    roundtrip(
      breakPoint1: EnsimeServerMessage,
      s"""{"typehint":"Breakpoint","file":"$file1","line":57}"""
    )

    roundtrip(
      BreakpointList(List(breakPoint1), List(breakPoint2)): EnsimeServerMessage,
      s"""{"typehint":"BreakpointList","active":[{"file":"$file1","line":57}],"pending":[{"file":"$file1","line":59}]}"""
    )

    roundtrip(
      DebugVmSuccess(): EnsimeServerMessage,
      """{"typehint":"DebugVmSuccess","status":"success"}"""
    )

    roundtrip(
      DebugVmError(303, "xxxx"): EnsimeServerMessage,
      """{"typehint":"DebugVmError","errorCode":303,"details":"xxxx","status":"error"}"""
    )
  }

  it should "roundtrip various informational types" in {
    roundtrip(
      note1: EnsimeServerMessage,
      """{"typehint":"Note","beg":23,"line":19,"col":8,"end":33,"file":"file1","msg":"note1","severity":{"typehint":"NoteError"}}"""
    )

    roundtrip(
      completionInfo: EnsimeServerMessage,
      """{"typehint":"CompletionInfo","name":"name","typeSig":{"sections":[[["abc","def"],["hij","lmn"]]],"result":"ABC", "hasImplicit": false},"relevance":90,"isCallable":false,"toInsert":"BAZ"}"""
    )

    roundtrip(
      completionInfo2: EnsimeServerMessage,
      """{"typehint":"CompletionInfo","name":"name2","typeSig":{"sections":[[["abc","def"]]],"result":"ABC", "hasImplicit": false},"relevance":91,"isCallable":true}"""
    )

    roundtrip(
      CompletionInfoList("fooBar", List(completionInfo)): EnsimeServerMessage,
      """{"typehint":"CompletionInfoList","prefix":"fooBar","completions":[{"name":"name","typeSig":{"sections":[[["abc","def"],["hij","lmn"]]],"result":"ABC", "hasImplicit": false},"relevance":90,"isCallable":false,"toInsert":"BAZ"}]}"""
    )

    roundtrip(
      new SymbolInfo("name", "localName", None, typeInfo, false): EnsimeServerMessage,
      """{"typehint":"SymbolInfo","name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false}"""
    )

    roundtrip(
      new NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EnsimeServerMessage,
      """{"typehint":"NamedTypeMemberInfo","name":"typeX","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      entityInfo: EnsimeServerMessage,
      """{"resultType":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"name":"Arrow1","paramSections":[{"params":[["ABC",{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}]],"isImplicit":false}],"typehint":"ArrowTypeInfo"}"""
    )

    roundtrip(
      typeInfo: EnsimeServerMessage,
      """{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      packageInfo: EnsimeServerMessage,
      """{"typehint":"PackageInfo","name":"name","fullName":"fullName","members":[]}"""
    )

    roundtrip(
      interfaceInfo: EnsimeServerMessage,
      """{"typehint":"InterfaceInfo","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"viaView":"DEF"}"""
    )

    roundtrip(
      new TypeInspectInfo(typeInfo, List(interfaceInfo)): EnsimeServerMessage,
      """{"typehint":"TypeInspectInfo","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"interfaces":[{"type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"viaView":"DEF"}],"infoType":"typeInspect"}"""
    )
  }

  it should "support search related responses" in {
    roundtrip(
      SymbolSearchResults(List(methodSearchRes, typeSearchRes)): EnsimeServerMessage,
      s"""{"typehint":"SymbolSearchResults","syms":[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]}"""
    )

    roundtrip(
      ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): EnsimeServerMessage,
      s"""{"typehint":"ImportSuggestions","symLists":[[{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}},{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}]]}"""
    )

    roundtrip(
      methodSearchRes: EnsimeServerMessage,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"MethodSearchResult","ownerName":"ownerStr","declAs":{"typehint":"Method"}}"""
    )

    roundtrip(
      typeSearchRes: EnsimeServerMessage,
      s"""{"name":"abc","localName":"a","pos":{"typehint":"LineSourcePosition","file":"$abd","line":10},"typehint":"TypeSearchResult","declAs":{"typehint":"Trait"}}"""
    )
  }

  it should "support ranges and semantic highlighting" in {
    roundtrip(
      ERangePositions(ERangePosition(batchSourceFile, 75, 70, 90) :: Nil): EnsimeServerMessage,
      s"""{"typehint":"ERangePositions","positions":[{"file":"/abc","offset":75,"start":70,"end":90}]}"""
    )

    roundtrip(
      FileRange("/abc", 7, 9): EnsimeServerMessage,
      s"""{"typehint":"FileRange","file":"/abc","start":7,"end":9}"""
    )

    roundtrip(
      SymbolDesignations(
        symFile, List(
        SymbolDesignation(7, 9, VarFieldSymbol),
        SymbolDesignation(11, 22, ClassSymbol)
      )
      ): EnsimeServerMessage,
      s"""{"typehint":"SymbolDesignations","file":"$symFile","syms":[{"start":7,"end":9,"symType":{"typehint":"VarFieldSymbol"}},{"start":11,"end":22,"symType":{"typehint":"ClassSymbol"}}]}"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitConversionInfo(5, 6, symbolInfo))): EnsimeServerMessage,
      """{"typehint":"ImplicitInfos","infos":[{"typehint":"ImplicitConversionInfo","start":5,"end":6,"fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false}}]}"""
    )

    roundtrip(
      ImplicitInfos(List(ImplicitParamInfo(5, 6, symbolInfo, List(symbolInfo, symbolInfo), true))): EnsimeServerMessage,
      """{"typehint":"ImplicitInfos","infos":[{"params":[{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false},{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false}],"typehint":"ImplicitParamInfo","fun":{"name":"name","localName":"localName","type":{"name":"type1","fullName":"FOO.type1","typehint":"BasicTypeInfo","typeArgs":[],"members":[],"declAs":{"typehint":"Method"}},"isCallable":false},"funIsImplicit":true,"end":6,"start":5}]}"""
    )

  }

  it should "support refactoring messages" in {
    roundtrip(
      RefactorFailure(7, "message"): EnsimeServerMessage,
      """{"typehint":"RefactorFailure","procedureId":7,"reason":"message","status":"failure"}"""
    )

    roundtrip(
      refactorEffect: EnsimeServerMessage,
      s"""{"typehint":"RefactorEffect","procedureId":9,"refactorType":{"typehint":"AddImport"},"changes":[{"text":"aaa","typehint":"TextEdit","to":7,"from":5,"file":"$file3"}],"status":"success"}"""
    )

    roundtrip(
      refactorResult: EnsimeServerMessage,
      s"""{"typehint":"RefactorResult","procedureId":7,"refactorType":{"typehint":"AddImport"},"touchedFiles":["$file3","$file1"],"status":"success"}"""
    )

    roundtrip(
      refactorDiffEffect: EnsimeServerMessage,
      s"""{"typehint":"RefactorDiffEffect","procedureId":9,"refactorType":{"typehint":"AddImport"},"diff":"$file2"}"""
    )

  }

  it should "support legacy raw response types" in {
    roundtrip(
      FalseResponse: EnsimeServerMessage,
      """{"typehint":"FalseResponse"}"""
    )

    roundtrip(
      TrueResponse: EnsimeServerMessage,
      """{"typehint":"TrueResponse"}"""
    )

    roundtrip(
      StringResponse("wibble"): EnsimeServerMessage,
      """{"typehint":"StringResponse","text":"wibble"}"""
    )

    roundtrip(
      VoidResponse: EnsimeServerMessage,
      """{"typehint":"VoidResponse"}"""
    )
  }
}
