package org.jetbrains.sbt
package project

import com.intellij.openapi.roots.DependencyScope
import com.intellij.pom.java.LanguageLevel
import org.jetbrains.sbt.project.data.Sdk

import scala.language.implicitConversions

/**
  * @author Nikolay Obedin
  * @since 8/4/15.
  */
object ProjectStructureDsl

  import DslUtils._

  trait ProjectAttribute
  trait ModuleAttribute
  trait LibraryAttribute
  trait DependencyAttribute

  val libraries = new Attribute[Seq[library]]("libraries")
  with ProjectAttribute with ModuleAttribute
  val modules = new Attribute[Seq[module]]("modules") with ProjectAttribute
  val sdk = new Attribute[Sdk]("sdk") with ProjectAttribute
  with ModuleAttribute
  val languageLevel = new Attribute[LanguageLevel]("languageLevel")
  with ProjectAttribute with ModuleAttribute

  val contentRoots = new Attribute[Seq[String]]("contentRoots")
  with ModuleAttribute
  val sources = new Attribute[Seq[String]]("sources") with ModuleAttribute
  with LibraryAttribute
  val testSources = new Attribute[Seq[String]]("testSources")
  with ModuleAttribute
  val resources = new Attribute[Seq[String]]("resources") with ModuleAttribute
  val testResources = new Attribute[Seq[String]]("testResources")
  with ModuleAttribute
  val excluded = new Attribute[Seq[String]]("excluded") with ModuleAttribute
  val moduleDependencies =
    new Attribute[Seq[dependency[module]]]("moduleDependencies")
    with ModuleAttribute
  val libraryDependencies =
    new Attribute[Seq[dependency[library]]]("libraryDependencies")
    with ModuleAttribute

  val classes = new Attribute[Seq[String]]("classes") with LibraryAttribute
  val javadocs = new Attribute[Seq[String]]("javadocs") with LibraryAttribute

  val isExported = new Attribute[Boolean]("isExported")
  with DependencyAttribute
  val scope = new Attribute[DependencyScope]("scope") with DependencyAttribute

  sealed trait Attributed
    protected val attributes = new AttributeMap

    def foreach[T : Manifest](attribute: Attribute[T])(body: T => Unit): Unit =
      attributes.get(attribute).foreach(body)

  trait Named
    val name: String

  class project(val name: String) extends Attributed
    protected implicit def defineAttribute[T : Manifest](
        attribute: Attribute[T] with ProjectAttribute): AttributeDef[T] =
      new AttributeDef(attribute, attributes)
    protected implicit def defineAttributeSeq[T](
        attribute: Attribute[Seq[T]] with ProjectAttribute)(
        implicit m: Manifest[Seq[T]]): AttributeSeqDef[T] =
      new AttributeSeqDef(attribute, attributes)

  class module(val name: String) extends Attributed with Named
    protected implicit def defineAttribute[T : Manifest](
        attribute: Attribute[T] with ModuleAttribute): AttributeDef[T] =
      new AttributeDef(attribute, attributes)
    protected implicit def defineAttributeSeq[T](
        attribute: Attribute[Seq[T]] with ModuleAttribute)(
        implicit m: Manifest[Seq[T]]): AttributeSeqDef[T] =
      new AttributeSeqDef(attribute, attributes)

  class library(val name: String) extends Attributed with Named
    protected implicit def defineAttribute[T : Manifest](
        attribute: Attribute[T] with LibraryAttribute): AttributeDef[T] =
      new AttributeDef(attribute, attributes)
    protected implicit def defineAttributeSeq[T](
        attribute: Attribute[Seq[T]] with LibraryAttribute)(
        implicit m: Manifest[Seq[T]]): AttributeSeqDef[T] =
      new AttributeSeqDef(attribute, attributes)

  class dependency[D <: Named](val reference: D)
      extends Attributed with Named
    override val name: String = reference.name
    protected implicit def defineAttribute[T : Manifest](
        attribute: Attribute[T] with DependencyAttribute): AttributeDef[T] =
      new AttributeDef(attribute, attributes)

  implicit def module2moduleDependency(module: module): dependency[module] =
    new dependency(module)

  implicit def library2libraryDependency(
      library: library): dependency[library] =
    new dependency(library)
