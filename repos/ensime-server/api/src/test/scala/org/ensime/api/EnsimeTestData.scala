// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.apache.org/licenses/LICENSE-2.0
package org.ensime.api

import java.io.File

trait EnsimeTestData {
  // duplicating utils to minimise dependencies
  private def canon(s: String) = {
    val file = new File(s)
    try file.getCanonicalFile
    catch {
      case t: Throwable => file.getAbsoluteFile
    }
  }

  val typeInfo = new BasicTypeInfo("type1", DeclaredAs.Method, "FOO.type1", List(), List(), None)

  val interfaceInfo = new InterfaceInfo(typeInfo, Some("DEF"))
  val typeInspectInfo = new TypeInspectInfo(typeInfo, List(interfaceInfo))

  val paramSectionInfo = new ParamSectionInfo(List(("ABC", typeInfo)), false)

  val symFile = canon("/abc")
  val symbolDesignations = SymbolDesignations(
    symFile,
    List(
      SymbolDesignation(7, 9, ObjectSymbol),
      SymbolDesignation(11, 22, TraitSymbol)
    )
  )

  val symbolInfo = new SymbolInfo("name", "localName", None, typeInfo, false)

  val implicitInfos = List(
    ImplicitConversionInfo(5, 6, symbolInfo),
    ImplicitParamInfo(7, 8, symbolInfo, List(symbolInfo, symbolInfo), true)
  )

  val batchSourceFile = "/abc"

  val rangePos1 = new ERangePosition(batchSourceFile, 75, 70, 90)

  val rangePos2 = new ERangePosition(batchSourceFile, 85, 80, 100)

  val packageInfo = new PackageInfo("name", "fullName", List())

  val completionInfo = new CompletionInfo("name", new CompletionSignature(List(List(("abc", "def"), ("hij", "lmn"))), "ABC", false), false, 90, Some("BAZ"))

  val completionInfo2 = new CompletionInfo("name2", new CompletionSignature(List(List(("abc", "def"))), "ABC", false), true, 91, None)

  val completionInfoList = List(completionInfo, completionInfo2)

  val refactorFailure = RefactorFailure(7, "message")

  val file1 = canon("/abc/def")
  val file2 = canon("/test/test/")
  val file3 = canon("/foo/abc")
  val file4 = canon("/foo/def")
  val file5 = canon("/foo/hij")

  val refactorEffect = new RefactorEffect(9, RefactorType.AddImport, List(TextEdit(file3, 5, 7, "aaa")))
  val refactorResult = new RefactorResult(7, RefactorType.AddImport, List(file3, file1))
  val refactorDiffEffect = new RefactorDiffEffect(9, RefactorType.AddImport, file2)

  val sourcePos1 = new LineSourcePosition(file1, 57)
  val sourcePos2 = new LineSourcePosition(file1, 59)
  val sourcePos3 = new EmptySourcePosition()
  val sourcePos4 = new OffsetSourcePosition(file1, 456)

  val breakPoint1 = new Breakpoint(sourcePos1.file, sourcePos1.line)
  val breakPoint2 = new Breakpoint(sourcePos2.file, sourcePos2.line)

  val breakpointList = BreakpointList(List(breakPoint1), List(breakPoint2))

  val debugStackLocal1 = DebugStackLocal(3, "name1", "summary1", "type1")
  val debugStackLocal2 = DebugStackLocal(4, "name2", "summary2", "type2")

  val debugStackFrame = DebugStackFrame(7, List(debugStackLocal1, debugStackLocal2), 4, "class1", "method1", sourcePos1, DebugObjectId(7))

  val debugBacktrace = DebugBacktrace(List(debugStackFrame), DebugThreadId(17), "thread1")

  val analyzerFile = canon("Analyzer.scala")
  val fooFile = canon("Foo.scala")

  val abd = canon("/abd")

  val methodSearchRes = MethodSearchResult("abc", "a", DeclaredAs.Method, Some(LineSourcePosition(abd, 10)), "ownerStr")
  val typeSearchRes = TypeSearchResult("abc", "a", DeclaredAs.Trait, Some(LineSourcePosition(abd, 10)))

  val importSuggestions = new ImportSuggestions(List(List(methodSearchRes, typeSearchRes)))

  val symbolSearchResults = new SymbolSearchResults(List(methodSearchRes, typeSearchRes))

  val completionInfoCList = CompletionInfoList("fooBar", List(completionInfo))

  val refactorRenameEffect = new RefactorEffect(7, RefactorType.Rename, List(TextEdit(file3, 5, 7, "aaa")))

  val fileRange = FileRange("/abc", 7, 9)

  val debugLocObjectRef: DebugLocation = DebugObjectReference(57L)

  val debugNullValue = DebugNullValue("typeNameStr")

  val debugArrayInstValue = DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L))

  val debugPrimitiveValue = DebugPrimitiveValue("summaryStr", "typeNameStr")

  val debugClassField = DebugClassField(19, "nameStr", "typeNameStr", "summaryStr")

  val debugStringValue = DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(6L))

  val note1 = new Note("file1", "note1", NoteError, 23, 33, 19, 8)
  val note2 = new Note("file1", "note2", NoteWarn, 23, 33, 19, 8)

  val noteList = NewScalaNotesEvent(isFull = true, List(note1, note2))

  val entityInfo: TypeInfo = new ArrowTypeInfo("Arrow1", typeInfo, List(paramSectionInfo))

  val sourceFileInfo = SourceFileInfo(file1, Some("{/* code here */}"), Some(file2))
  val dtid = DebugThreadId(13)
  val debugLocationArray = DebugArrayElement(DebugObjectId(13), 14)

  val structureView = StructureView(List(
    StructureViewMember(
      keyword = "class",
      name = "StructureView",
      position = sourcePos1,
      members = Nil
    ),
    StructureViewMember(
      keyword = "object",
      name = "StructureView",
      position = sourcePos2,
      members = List(
        StructureViewMember(
          keyword = "type",
          name = "BasicType",
          position = sourcePos4,
          members = Nil
        )
      )
    )
  ))
}
