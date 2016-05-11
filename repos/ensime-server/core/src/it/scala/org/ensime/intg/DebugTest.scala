// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import java.io.File

import akka.testkit._
import org.ensime.api._
import org.ensime.core._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._
import org.scalatest.Matchers

// must be refreshing as the tests don't clean up after themselves properly
class DebugTest extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with DebugTestUtils {

  val original = EnsimeConfigFixture.DebugTestProject.copy(
    javaLibs = Nil // no need to index the JRE
  )

  // TODO This is broken because step in our case is stepping into
  // the classloader for inner class rather than doing a user
  // visible step.
  "Debug - stepping" should "handle basic stepping" ignore {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit testkit =>
        withProject { (project, asyncHelper) =>
          implicit val p = (project, asyncHelper)
          withDebugSession(
            "stepping.ForComprehensionListString",
            "stepping/ForComprehensionListString.scala",
            9
          ) { breakpointsFile =>
              import testkit._

              checkTopStackFrame("stepping.ForComprehensionListString$", "main", 9)
              project ! DebugNextReq(DebugThreadId(1))
              expectMsg(VoidResponse)

              checkTopStackFrame("stepping.ForComprehensionListString$$anonfun$main$1", "apply", 10)
            }
        }
      }
    }
  }

  "Breakpoints" should "trigger/continue" in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "breakpoints.Breakpoints",
          "breakpoints/Breakpoints.scala",
          32
        ) { breakpointsFile =>
            import testkit._
            val breakpointsPath = breakpointsFile.getAbsolutePath

            project ! DebugBacktraceReq(DebugThreadId(1), 0, 3)
            expectMsgType[DebugBacktrace] should matchPattern {
              case DebugBacktrace(List(
                DebugStackFrame(0, List(), 0, "breakpoints.Breakpoints", "mainTest",
                  LineSourcePosition(`breakpointsFile`, 32), _),
                DebugStackFrame(1, List(
                  DebugStackLocal(0, "args", "Array[]", "java.lang.String[]")
                  ), 1, "breakpoints.Breakpoints$", "main",
                  LineSourcePosition(`breakpointsFile`, 41), _),
                DebugStackFrame(2, List(), 1, "breakpoints.Breakpoints", "main",
                  LineSourcePosition(`breakpointsFile`, _), _)
                ), DebugThreadId(1), "main") =>
            }

            //            val bp11 = session.addLineBreakpoint(BP_TYPENAME, 11)
            project ! DebugSetBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)
            //            val bp13 = session.addLineBreakpoint(BP_TYPENAME, 13)
            project ! DebugSetBreakReq(breakpointsFile, 13)
            expectMsg(TrueResponse)

            //              session.waitForBreakpointsToBeEnabled(bp11, bp13)

            //              session.resumetoSuspension()
            //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)

            asyncHelper.expectMsg(DebugBreakEvent(DebugThreadId(1), "main", breakpointsFile, 32))

            project ! DebugContinueReq(DebugThreadId(1))
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(DebugThreadId(1), "main", breakpointsFile, 11))

            //              session.resumetoSuspension()
            //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 13)

            project ! DebugContinueReq(DebugThreadId(1))
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(DebugThreadId(1), "main", breakpointsFile, 13))

            //              bp11.setEnabled(false)
            project ! DebugClearBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)

            //              session.waitForBreakpointsToBeDisabled(bp11)
            //
            //              session.resumetoSuspension()
            project ! DebugContinueReq(DebugThreadId(1))
            expectMsg(TrueResponse)
            //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 13)
            asyncHelper.expectMsg(DebugBreakEvent(DebugThreadId(1), "main", breakpointsFile, 13))
            //
            //              bp11.setEnabled(true); bp13.setEnabled(false)
            project ! DebugSetBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)
            project ! DebugClearBreakReq(breakpointsFile, 13)
            expectMsg(TrueResponse)

            //
            //              session.waitForBreakpointsToBeEnabled(bp11)
            //              session.waitForBreakpointsToBeDisabled(bp13)
            //
            //              session.resumetoSuspension()
            //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)
            project ! DebugContinueReq(DebugThreadId(1))
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(DebugThreadId(1), "main", breakpointsFile, 11))
            //
            //              session.resumetoSuspension()
            //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)
            project ! DebugContinueReq(DebugThreadId(1))
            expectMsg(TrueResponse)
            asyncHelper.expectMsg(DebugBreakEvent(DebugThreadId(1), "main", breakpointsFile, 11))
            //
            project ! DebugContinueReq(DebugThreadId(1))
            expectMsg(TrueResponse)

            //asyncHelper.expectAsync(60 seconds, DebugVMDisconnectEvent)
            //              session.resumeToCompletion()
            //              bp11.delete()
            //              bp13.delete()
          }
      }
    }
  }

  it should "list/clear" in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "breakpoints.Breakpoints",
          "breakpoints/Breakpoints.scala",
          32
        ) { breakpointsFile =>
            import testkit._
            val breakpointsPath = breakpointsFile.getAbsolutePath

            // TODO: test listing/clearing pending breakpoints (i.e. before we connect)

            project ! DebugListBreakpointsReq
            expectMsgType[BreakpointList] should matchPattern {
              case BreakpointList(Nil, Nil) =>
            }

            // break in main
            project ! DebugSetBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)
            project ! DebugSetBreakReq(breakpointsFile, 13)
            expectMsg(TrueResponse)

            // breakpoints should now be active
            project ! DebugListBreakpointsReq
            inside(expectMsgType[BreakpointList]) {
              case BreakpointList(activeBreakpoints, pendingBreakpoints) =>
                activeBreakpoints should contain theSameElementsAs Set(
                  Breakpoint(breakpointsFile, 11), Breakpoint(breakpointsFile, 13)
                )
                pendingBreakpoints shouldBe empty
            }

            // check clear works again
            project ! DebugClearAllBreaksReq
            expectMsg(TrueResponse)
            project ! DebugListBreakpointsReq
            expectMsgType[BreakpointList] should matchPattern {
              case BreakpointList(Nil, Nil) =>
            }
          }
      }
    }
  }

  // starting up a debug session for each variable is unneeded and wasteful of test time.
  // this approach means that there is one test method, but it still explores all of the paths.
  "Debug Inspect variables" should "inspect variables" in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "debug.Variables",
          "debug/Variables.scala",
          21
        ) { variablesFile =>
            // boolean local
            getVariableValue(DebugThreadId(1), "a") should matchPattern {
              case DebugPrimitiveValue("true", "boolean") =>
            }

            // char local
            getVariableValue(DebugThreadId(1), "b") should matchPattern {
              case DebugPrimitiveValue("'c'", "char") =>
            }

            // short local
            getVariableValue(DebugThreadId(1), "c") should matchPattern {
              case DebugPrimitiveValue("3", "short") =>
            }

            // int local
            getVariableValue(DebugThreadId(1), "d") should matchPattern {
              case DebugPrimitiveValue("4", "int") =>
            }

            // long local
            getVariableValue(DebugThreadId(1), "e") should matchPattern {
              case DebugPrimitiveValue("5", "long") =>
            }

            // float local
            getVariableValue(DebugThreadId(1), "f") should matchPattern {
              case DebugPrimitiveValue("1.0", "float") =>
            }

            // double local
            getVariableValue(DebugThreadId(1), "g") should matchPattern {
              case DebugPrimitiveValue("2.0", "double") =>
            }

            // String local
            inside(getVariableValue(DebugThreadId(1), "h")) {
              case DebugStringInstance("\"test\"", debugFields, "java.lang.String", _) =>
                exactly(1, debugFields) should matchPattern {
                  case DebugClassField(_, "value", "char[]", "Array['t', 'e', 's',...]") =>
                }
            }

            // primitive array local
            getVariableValue(DebugThreadId(1), "i") should matchPattern {
              case DebugArrayInstance(3, "int[]", "int", _) =>
            }

            // type local
            inside(getVariableValue(DebugThreadId(1), "j")) {
              case DebugObjectInstance("Instance of $colon$colon", debugFields, "scala.collection.immutable.$colon$colon", _) =>
                exactly(1, debugFields) should matchPattern {
                  case DebugClassField(_, head, "java.lang.Object", "Instance of Integer") if head == "head" | head == "scala$collection$immutable$$colon$colon$$hd" =>
                }
            }

            // object array local
            getVariableValue(DebugThreadId(1), "k") should matchPattern {
              case DebugArrayInstance(3, "java.lang.Object[]", "java.lang.Object", _) =>
            }
          }
      }
    }
  }
}

trait DebugTestUtils {
  this: ProjectFixture with Matchers with EnsimeConfigFixture =>

  /**
   * @param fileName to place the breakpoint
   * @param className containing the main method
   * @param breakLine where to start the session in the fileName
   */
  def withDebugSession(
    className: String,
    fileName: String,
    breakLine: Int
  )(
    f: File => Any
  )(
    implicit
    config: EnsimeConfig,
    testkit: TestKitFix,
    // don't take an implicit TestActorRef or it steals the implicit sender
    p: (TestActorRef[Project], TestProbe)
  ): Any = {
    import testkit._
    val resolvedFile = scalaMain(config) / fileName
    val project = p._1
    val asyncHelper = p._2

    project ! DebugSetBreakReq(resolvedFile, breakLine)
    expectMsg(TrueResponse)

    project ! DebugStartReq(className)
    expectMsg(DebugVmSuccess())

    asyncHelper.expectMsg(DebugVMStartEvent)
    asyncHelper.expectMsgType[DebugThreadStartEvent]

    val expect = DebugBreakEvent(DebugThreadId(1), "main", resolvedFile, breakLine)
    asyncHelper.expectMsg(expect)
    project ! DebugClearBreakReq(resolvedFile, breakLine)
    expectMsg(TrueResponse)

    try {
      f(resolvedFile)
    } finally {
      project ! DebugClearAllBreaksReq
      expectMsg(TrueResponse)

      // no way to await the stopped condition so we let the app run
      // its course on the main thread
      project ! DebugContinueReq(DebugThreadId(1))
      expectMsg(TrueResponse)
      project ! DebugStopReq
      expectMsgPF() {
        case TrueResponse =>
        case FalseResponse => // windows does this sometimes
      }
    }
  }

  def getVariableValue(threadId: DebugThreadId, variableName: String)(implicit testkit: TestKitFix, p: (TestActorRef[Project], TestProbe)): DebugValue = {
    import testkit._
    val project = p._1
    project ! DebugLocateNameReq(threadId, variableName)
    val vLoc = expectMsgType[DebugLocation]

    project ! DebugValueReq(vLoc)
    expectMsgType[DebugValue]
  }

  def checkTopStackFrame(className: String, method: String, line: Int)(implicit testkit: TestKitFix, p: (TestActorRef[Project], TestProbe)): Unit = {
    import testkit._
    val project = p._1

    project ! DebugBacktraceReq(DebugThreadId(1), 0, 1)
    expectMsgType[DebugBacktrace] should matchPattern {
      case DebugBacktrace(List(DebugStackFrame(0, _, 1, `className`, `method`,
        LineSourcePosition(_, `line`), _)),
        DebugThreadId(1), "main") =>
    }
  }
}
