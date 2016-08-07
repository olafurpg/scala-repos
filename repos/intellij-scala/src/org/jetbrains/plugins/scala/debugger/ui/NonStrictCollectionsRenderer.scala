package org.jetbrains.plugins.scala.debugger.ui

import java.util

import com.intellij.debugger.DebuggerContext
import com.intellij.debugger.engine.DebuggerUtils
import com.intellij.debugger.engine.evaluation.{EvaluateException, EvaluationContext, EvaluationContextImpl}
import com.intellij.debugger.impl.PositionUtil
import com.intellij.debugger.ui.impl.watch.{ValueDescriptorImpl, WatchItemDescriptor}
import com.intellij.debugger.ui.tree.render._
import com.intellij.debugger.ui.tree.{DebuggerTreeNode, NodeDescriptor, ValueDescriptor}
import com.intellij.openapi.project.Project
import com.intellij.psi.{JavaPsiFacade, PsiExpression}
import com.intellij.util.{IncorrectOperationException, StringBuilderSpinAllocator}
import com.sun.jdi._
import com.sun.tools.jdi.ObjectReferenceImpl
import org.jetbrains.plugins.scala.debugger.filters.ScalaDebuggerSettings
import org.jetbrains.plugins.scala.debugger.ui.NonStrictCollectionsRenderer.{CollectionElementNodeDescriptor, Fail, SimpleMethodInvocationResult}

/**
  * User: Dmitry Naydanov
  * Date: 9/3/12
  */
class NonStrictCollectionsRenderer extends NodeRendererImpl {
  import org.jetbrains.plugins.scala.debugger.ui.NonStrictCollectionsRenderer.{MethodNotFound, Success}
  import org.jetbrains.plugins.scala.debugger.ui.{NonStrictCollectionsRenderer => companionObject}

  def getStartIndex =
    ScalaDebuggerSettings.getInstance().COLLECTION_START_INDEX.intValue()
  def getEndIndex =
    ScalaDebuggerSettings.getInstance().COLLECTION_END_INDEX.intValue()

  def getUniqueId = "NonStrictCollectionsRenderer"

  override def getName = "Scala streams as collections"

  override def setName(text: String) { /*do nothing*/ }

  override def isEnabled: Boolean = !mustNotExpandStreams

  override def setEnabled(enabled: Boolean) {
    /*see ScalaDebuggerSettingsConfigurable */
  }

  private def mustNotExpandStreams: Boolean =
    ScalaDebuggerSettings.getInstance().DO_NOT_DISPLAY_STREAMS

  private def invokeLengthMethodByName(objectRef: ObjectReference,
                                       methodName: String,
                                       signature: Char,
                                       context: EvaluationContext) = {
    val suitableMethods =
      objectRef.referenceType().methodsByName(methodName, "()" + signature)
    if (suitableMethods.size() > 0) {
      companionObject
        .invokeEmptyArgsMethod(objectRef, suitableMethods get 0, context)
    } else {
      MethodNotFound()
    }
  }

  private def tryToGetSize(
      objectRef: ObjectReference,
      context: EvaluationContext): SimpleMethodInvocationResult[_] = {
    @inline def invoke(name: String) =
      invokeLengthMethodByName(objectRef, name, 'I', context)

    try {
      if (!ScalaCollectionRenderer.hasDefiniteSize(objectRef, context) ||
          isStreamView(objectRef.referenceType())) return Success[String]("?")
    } catch {
      case e: EvaluateException => return Fail(e)
    }

    invoke("size") match {
      case result @ Success(_) => result
      case a => a
    }
  }

  def buildChildren(value: Value,
                    builder: ChildrenBuilder,
                    evaluationContext: EvaluationContext) {
    def invokeEmptyArgsMethod(obj: ObjectReference,
                              actualRefType: ReferenceType,
                              methodName: String): Value = {
      val suitableMethods = actualRefType methodsByName methodName
      if (suitableMethods.size() == 0) return null

      try {
        evaluationContext.getDebugProcess.invokeMethod(
            evaluationContext,
            obj,
            suitableMethods get 0,
            companionObject.EMPTY_ARGS)
      } catch {
        case (_: EvaluateException | _: InvocationException |
            _: InvalidTypeException | _: IncompatibleThreadStateException |
            _: ClassNotLoadedException) =>
          null
      }
    }

    @inline
    def getTail(objRef: ObjectReference, actualRefType: ReferenceType) =
      invokeEmptyArgsMethod(objRef, actualRefType, "tail")
    @inline
    def getHead(objRef: ObjectReference, actualRefType: ReferenceType) =
      invokeEmptyArgsMethod(objRef, actualRefType, "head")
    @inline def getAll(objRef: ObjectReference, actualType: ReferenceType) =
      (getHead(objRef, actualType), getTail(objRef, actualType))

    val myChildren = new util.ArrayList[DebuggerTreeNode]()

    def returnChildren() {
      builder.setChildren(myChildren)
    }
    value match {
      case objectRef: ObjectReference
          if ScalaCollectionRenderer.nonEmpty(objectRef, evaluationContext) =>
        var currentTail = objectRef

        for (i <- 0 until getStartIndex) {
          getTail(currentTail, currentTail.referenceType()) match {
            case newTail: ObjectReference => currentTail = newTail
            case _ => return //ourCollection.size < startIndex
          }
        }

        var indexCount = getStartIndex
        for (i <- getStartIndex to getEndIndex) {
          getAll(currentTail, currentTail.referenceType()) match {
            case (newHead: ObjectReference, newTail: ObjectReference) =>
              val newNode = builder.getNodeManager.createNode(
                  new CollectionElementNodeDescriptor(
                      indexCount.toString,
                      evaluationContext.getProject,
                      newHead),
                  evaluationContext)
              myChildren add newNode
              currentTail = newTail
              indexCount += 1
            case _ => returnChildren(); return
          }
        }

      case _ =>
    }

    returnChildren()
  }

  def getChildValueExpression(node: DebuggerTreeNode,
                              context: DebuggerContext): PsiExpression = {
    node.getDescriptor match {
      case watch: WatchItemDescriptor =>
        JavaPsiFacade
          .getInstance(node.getProject)
          .getElementFactory
          .createExpressionFromText(watch.calcValueName(), null)
      case collectionItem: CollectionElementNodeDescriptor =>
        collectionItem.getDescriptorEvaluation(context)
      case _ => null
    }
  }

  def isExpandable(value: Value,
                   evaluationContext: EvaluationContext,
                   parentDescriptor: NodeDescriptor) = value match {
    case objectRef: ObjectReferenceImpl =>
      ScalaCollectionRenderer.nonEmpty(objectRef, evaluationContext)
    case _ => false
  }

  def isApplicable(tpe: Type): Boolean = tpe match {
    case refType: ReferenceType if !mustNotExpandStreams =>
      isStream(tpe) || isView(tpe)
    case _ => false
  }

  private def isView(tpe: Type): Boolean =
    DebuggerUtils.instanceOf(tpe, ScalaCollectionRenderer.viewClassName)

  private def isStream(tpe: Type): Boolean =
    DebuggerUtils.instanceOf(tpe, ScalaCollectionRenderer.streamClassName)

  private def isStreamView(tpe: Type): Boolean =
    DebuggerUtils.instanceOf(tpe, ScalaCollectionRenderer.streamViewClassName)

  def calcLabel(descriptor: ValueDescriptor,
                context: EvaluationContext,
                listener: DescriptorLabelListener) = {
    val stringBuilder = StringBuilderSpinAllocator.alloc()

    descriptor.getValue match {
      case obj: ObjectReference =>
        val tpe = obj.referenceType()
        val sizeString =
          " size = " + (tryToGetSize(obj, context) match {
                case Success(value: Int) => value
                case _ => "?"
              })

        stringBuilder append
          (if (tpe != null)
             ScalaCollectionRenderer.transformName(tpe.name) + sizeString
           else "{...}")
      case _ => stringBuilder append "{...}"
    }

    val label = stringBuilder.toString
    StringBuilderSpinAllocator.dispose(stringBuilder)
    label
  }
}

object NonStrictCollectionsRenderer {
  private val EMPTY_ARGS =
    util.Collections.unmodifiableList(new util.ArrayList[Value]())

  //it considers only part of cases so it is not intended to be used outside 
  private def invokeEmptyArgsMethod(
      obj: ObjectReference,
      method: Method,
      context: EvaluationContext): SimpleMethodInvocationResult[_] = {
    try {
      context.getDebugProcess
        .invokeMethod(context, obj, method, EMPTY_ARGS) match {
        case intValue: IntegerValue => Success[Int](intValue.intValue())
        case boolValue: BooleanValue =>
          Success[Boolean](boolValue.booleanValue())
        case objValue: ObjectReference => Success[Value](objValue)
        case _ => MethodNotFound()
      }
    } catch {
      case e @ (_: EvaluateException | _: InvocationException |
          _: InvalidTypeException | _: IncompatibleThreadStateException |
          _: ClassNotLoadedException) =>
        Fail[Throwable](e)
    }
  }

// jdi sucks :(
//  private def invokeEmptyArgsMethodWithTimeout(obj: ObjectReference, method: Method, context: EvaluationContext) = {
//    var result: SimpleMethodInvocationResult[_] = null
//    TimeoutUtil.executeWithTimeout(10000, new Runnable {def run() {result = invokeEmptyArgsMethod(obj, method, context)}})
//    if (result == null) TimeoutExceeded() else result
//  }
// private case class TimeoutExceeded() extends SimpleMethodInvocationResult[Nothing]

  private class SimpleMethodInvocationResult[R]
  private case class MethodNotFound()
      extends SimpleMethodInvocationResult[Nothing]
  private case class Success[R](value: R)
      extends SimpleMethodInvocationResult[R]
  private case class Fail[E <: Throwable](exc: E)
      extends SimpleMethodInvocationResult[E]

  private class CollectionElementNodeDescriptor(name: String,
                                                project: Project,
                                                value: Value)
      extends ValueDescriptorImpl(project, value) {
    def calcValue(evaluationContext: EvaluationContextImpl) = value

    def getDescriptorEvaluation(context: DebuggerContext): PsiExpression = {
      try {
        JavaPsiFacade
          .getInstance(project)
          .getElementFactory
          .createExpressionFromText(name,
                                    PositionUtil getContextElement context)
      } catch {
        case e: IncorrectOperationException => null
      }
    }

    override def getName: String = name
  }
}
