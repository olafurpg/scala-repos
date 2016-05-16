/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm
import asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.collection.concurrent
import scala.tools.asm.Attribute
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.ClassFileLookup
import BytecodeUtils._
import ByteCodeRepository._
import BTypes.InternalName
import java.util.concurrent.atomic.AtomicLong

/**
  * The ByteCodeRepository provides utilities to read the bytecode of classfiles from the compilation
  * classpath. Parsed classes are cached in the `classes` map.
  *
  * @param classPath The compiler classpath where classfiles are searched and read from.
  */
class ByteCodeRepository[BT <: BTypes](
    val classPath: ClassFileLookup[AbstractFile], val btypes: BT) {
  import btypes._

  /**
    * ClassNodes for classes being compiled in the current compilation run.
    */
  val compilingClasses: concurrent.Map[InternalName, ClassNode] =
    recordPerRunCache(concurrent.TrieMap.empty)

  /**
    * Cache for parsed ClassNodes.
    * The `Long` field encodes the age of the node in the map, which allows removing old entries when
    * the map grows too large (see limitCacheSize).
    * For Java classes in mixed compilation, the map contains an error message: no ClassNode is
    * generated by the backend and also no classfile that could be parsed.
    */
  val parsedClasses: concurrent.Map[
      InternalName, Either[ClassNotFound, (ClassNode, Long)]] =
    recordPerRunCache(concurrent.TrieMap.empty)

  private val maxCacheSize = 1500
  private val targetSize = 500

  private object lruCounter
      extends AtomicLong(0l)
      with collection.generic.Clearable {
    def clear(): Unit = { this.set(0l) }
  }
  recordPerRunCache(lruCounter)

  /**
    * Prevent the code repository from growing too large. Profiling reveals that the average size
    * of a ClassNode is about 30 kb. I observed having 17k+ classes in the cache, i.e., 500 mb.
    */
  private def limitCacheSize(): Unit = {
    if (parsedClasses.size > maxCacheSize) {
      // OK if multiple threads get here
      val minimalLRU = parsedClasses.valuesIterator
        .collect({
          case Right((_, lru)) => lru
        })
        .toList
        .sorted(Ordering.Long.reverse)
        .drop(targetSize)
        .headOption
        .getOrElse(Long.MaxValue)
      parsedClasses retain {
        case (_, Right((_, lru))) => lru > minimalLRU
        case _ => false
      }
    }
  }

  def add(classNode: ClassNode, source: Source) = {
    if (source == CompilationUnit) compilingClasses(classNode.name) = classNode
    else
      parsedClasses(classNode.name) = Right(
          (classNode, lruCounter.incrementAndGet()))
  }

  /**
    * The class node and source for an internal name. If the class node is not yet available, it is
    * parsed from the classfile on the compile classpath.
    */
  def classNodeAndSource(internalName: InternalName)
    : Either[ClassNotFound, (ClassNode, Source)] = {
    classNode(internalName) map
    (n => {
          val source =
            if (compilingClasses contains internalName) CompilationUnit
            else Classfile
          (n, source)
        })
  }

  /**
    * The class node for an internal name. If the class node is not yet available, it is parsed from
    * the classfile on the compile classpath.
    */
  def classNode(internalName: InternalName): Either[ClassNotFound, ClassNode] = {
    compilingClasses.get(internalName).map(Right(_)) getOrElse {
      val r = parsedClasses.get(internalName) match {
        case Some(l @ Left(_)) => l
        case Some(r @ Right((classNode, _))) =>
          parsedClasses(internalName) =
            Right((classNode, lruCounter.incrementAndGet()))
          r
        case None =>
          limitCacheSize()
          val res =
            parseClass(internalName).map((_, lruCounter.incrementAndGet()))
          parsedClasses(internalName) = res
          res
      }
      r.map(_._1)
    }
  }

  /**
    * The field node for a field matching `name` and `descriptor`, accessed in class `classInternalName`.
    * The declaration of the field may be in one of the superclasses.
    *
    * @return The [[FieldNode]] of the requested field and the [[InternalName]] of its declaring
    *         class, or an error message if the field could not be found
    */
  def fieldNode(
      classInternalName: InternalName,
      name: String,
      descriptor: String): Either[FieldNotFound, (FieldNode, InternalName)] = {
    def fieldNodeImpl(parent: InternalName)
      : Either[FieldNotFound, (FieldNode, InternalName)] = {
      classNode(parent) match {
        case Left(e) =>
          Left(FieldNotFound(name, descriptor, classInternalName, Some(e)))
        case Right(c) =>
          c.fields.asScala.find(f => f.name == name && f.desc == descriptor) match {
            case Some(f) => Right((f, parent))
            case None =>
              if (c.superName == null)
                Left(FieldNotFound(name, descriptor, classInternalName, None))
              else fieldNode(c.superName, name, descriptor)
          }
      }
    }
    fieldNodeImpl(classInternalName)
  }

  /**
    * The method node for a method matching `name` and `descriptor`, accessed in class `ownerInternalNameOrArrayDescriptor`.
    * The declaration of the method may be in one of the parents.
    *
    * TODO: make sure we always return the right method, the one being invoked. write tests.
    *   - if there's an abstract and a concrete one. could possibly somehow the abstract be returned?
    *   - with traits and default methods, if there is more than one default method inherited and
    *     no override: what should be returned? We should not just inline one of the two.
    *
    * @return The [[MethodNode]] of the requested method and the [[InternalName]] of its declaring
    *         class, or an error message if the method could not be found.
    */
  def methodNode(ownerInternalNameOrArrayDescriptor: String,
                 name: String,
                 descriptor: String)
    : Either[MethodNotFound, (MethodNode, InternalName)] = {
    // on failure, returns a list of class names that could not be found on the classpath
    def methodNodeImpl(ownerInternalName: InternalName)
      : Either[List[ClassNotFound], (MethodNode, InternalName)] = {
      classNode(ownerInternalName) match {
        case Left(e) => Left(List(e))
        case Right(c) =>
          c.methods.asScala.find(m => m.name == name && m.desc == descriptor) match {
            case Some(m) => Right((m, ownerInternalName))
            case None =>
              findInParents(
                  Option(c.superName) ++: c.interfaces.asScala.toList, Nil)
          }
      }
    }

    // find the MethodNode in one of the parent classes
    def findInParents(
        parents: List[InternalName], failedClasses: List[ClassNotFound])
      : Either[List[ClassNotFound], (MethodNode, InternalName)] =
      parents match {
        case x :: xs =>
          methodNodeImpl(x).left.flatMap(failed =>
                findInParents(xs, failed ::: failedClasses))
        case Nil => Left(failedClasses)
      }

    // In a MethodInsnNode, the `owner` field may be an array descriptor, for example when invoking `clone`. We don't have a method node to return in this case.
    if (ownerInternalNameOrArrayDescriptor.charAt(0) == '[')
      Left(MethodNotFound(
              name, descriptor, ownerInternalNameOrArrayDescriptor, Nil))
    else
      methodNodeImpl(ownerInternalNameOrArrayDescriptor).left.map(
          MethodNotFound(
              name, descriptor, ownerInternalNameOrArrayDescriptor, _))
  }

  private def parseClass(
      internalName: InternalName): Either[ClassNotFound, ClassNode] = {
    val fullName = internalName.replace('/', '.')
    classPath.findClassFile(fullName) map { classFile =>
      val classNode = new asm.tree.ClassNode()
      val classReader = new asm.ClassReader(classFile.toByteArray)

      // Passing the InlineInfoAttributePrototype makes the ClassReader invoke the specific `read`
      // method of the InlineInfoAttribute class, instead of putting the byte array into a generic
      // Attribute.
      // We don't need frames when inlining, but we want to keep the local variable table, so we
      // don't use SKIP_DEBUG.
      classReader.accept(classNode,
                         Array[Attribute](InlineInfoAttributePrototype),
                         asm.ClassReader.SKIP_FRAMES)
      // SKIP_FRAMES leaves line number nodes. Remove them because they are not correct after
      // inlining.
      // TODO: we need to remove them also for classes that are not parsed from classfiles, why not simplify and do it once when inlining?
      // OR: instead of skipping line numbers for inlined code, use write a SourceDebugExtension
      // attribute that contains JSR-45 data that encodes debugging info.
      //   http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.11
      //   https://jcp.org/aboutJava/communityprocess/final/jsr045/index.html
      removeLineNumberNodes(classNode)
      classNode
    } match {
      case Some(node) => Right(node)
      case None =>
        Left(ClassNotFound(internalName, javaDefinedClasses(internalName)))
    }
  }
}

object ByteCodeRepository {

  /**
    * The source of a ClassNode in the ByteCodeRepository. Can be either [[CompilationUnit]] if the
    * class is being compiled or [[Classfile]] if the class was parsed from the compilation classpath.
    */
  sealed trait Source
  object CompilationUnit extends Source
  object Classfile extends Source
}
