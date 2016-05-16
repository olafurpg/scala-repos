package org.jetbrains.sbt
package project.data
package service

import com.intellij.compiler.impl.javaCompiler.javac.JavacConfiguration
import com.intellij.compiler.{CompilerConfiguration, CompilerConfigurationImpl}
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.project.IdeModifiableModelsProvider
import com.intellij.openapi.externalSystem.util.{ExternalSystemApiUtil, ExternalSystemUtil}
import com.intellij.openapi.module.{ModuleManager, ModuleUtil}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{LanguageLevelProjectExtension, ProjectRootManager}
import org.jetbrains.plugins.scala.project.IncrementalityType
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration
import org.jetbrains.plugins.scala.settings.ScalaProjectSettings
import org.jetbrains.sbt.project.sources.SharedSourcesModuleType
import org.jetbrains.sbt.settings.SbtSystemSettings

import scala.collection.JavaConverters._

/**
  * @author Pavel Fatin
  */
class SbtProjectDataService
    extends AbstractDataService[SbtProjectData, Project](SbtProjectData.Key) {
  override def createImporter(
      toImport: Seq[DataNode[SbtProjectData]],
      projectData: ProjectData,
      project: Project,
      modelsProvider: IdeModifiableModelsProvider): Importer[SbtProjectData] =
    new SbtProjectDataService.Importer(
        toImport, projectData, project, modelsProvider)
}

object SbtProjectDataService {

  private class Importer(dataToImport: Seq[DataNode[SbtProjectData]],
                         projectData: ProjectData,
                         project: Project,
                         modelsProvider: IdeModifiableModelsProvider)
      extends AbstractImporter[SbtProjectData](
          dataToImport, projectData, project, modelsProvider) {

    override def importData(): Unit =
      dataToImport.foreach(node => doImport(node.getData))

    private def doImport(data: SbtProjectData): Unit = {
      ScalaProjectSettings
        .getInstance(project)
        .setBasePackages(data.basePackages.asJava)
      configureJdk(project, data)
      updateJavaCompilerOptionsIn(project, data.javacOptions)
      setLanguageLevel(project, data)
      setSbtVersion(project, data)
      updateIncrementalityType(project)
    }

    private def configureJdk(project: Project, data: SbtProjectData): Unit =
      executeProjectChangeAction {
        val existingJdk =
          Option(ProjectRootManager.getInstance(project).getProjectSdk)
        val projectJdk = data.jdk
          .flatMap(SdkUtils.findProjectSdk)
          .orElse(existingJdk)
          .orElse(SdkUtils.allJdks.headOption)
        projectJdk.foreach(
            ProjectRootManager.getInstance(project).setProjectSdk)
      }

    private def setLanguageLevel(
        project: Project, data: SbtProjectData): Unit =
      executeProjectChangeAction {
        val projectJdk =
          Option(ProjectRootManager.getInstance(project).getProjectSdk)
        val javaLanguageLevel = SdkUtils
          .javaLanguageLevelFrom(data.javacOptions)
          .orElse(projectJdk.flatMap(SdkUtils.defaultJavaLanguageLevelIn))
        javaLanguageLevel.foreach { level =>
          val extension = LanguageLevelProjectExtension.getInstance(project)
          extension.setLanguageLevel(level)
          extension.setDefault(false)
        }
      }

    private def setSbtVersion(project: Project, data: SbtProjectData): Unit =
      Option(SbtSystemSettings
            .getInstance(project)
            .getLinkedProjectSettings(data.projectPath)).foreach(s =>
            s.sbtVersion = data.sbtVersion)

    private def updateIncrementalityType(project: Project): Unit = {
      if (getModules.exists(it =>
                ModuleUtil.getModuleType(it) == SharedSourcesModuleType.instance))
        ScalaCompilerConfiguration.instanceIn(project).incrementalityType =
          IncrementalityType.SBT
    }

    private def updateJavaCompilerOptionsIn(
        project: Project, options: Seq[String]): Unit =
      executeProjectChangeAction {
        val settings =
          JavacConfiguration.getOptions(project, classOf[JavacConfiguration])

        def contains(values: String*) = values.exists(options.contains)

        def valueOf(name: String): Option[String] =
          Option(options.indexOf(name))
            .filterNot(-1 == _)
            .flatMap(i => options.lift(i + 1))

        if (contains("-g:none")) {
          settings.DEBUGGING_INFO = false
        }

        if (contains("-nowarn", "-Xlint:none")) {
          settings.GENERATE_NO_WARNINGS = true
        }

        if (contains("-deprecation", "-Xlint:deprecation")) {
          settings.DEPRECATION = true
        }

        valueOf("-target").foreach { target =>
          val compilerSettings = CompilerConfiguration
            .getInstance(project)
            .asInstanceOf[CompilerConfigurationImpl]
          compilerSettings.setProjectBytecodeTarget(target)
        }

        val customOptions = additionalOptionsFrom(options)

        settings.ADDITIONAL_OPTIONS_STRING = customOptions.mkString(" ")
      }

    private def additionalOptionsFrom(options: Seq[String]): Seq[String] = {
      val handledOptions = Set("-g:none",
                               "-nowarn",
                               "-Xlint:none",
                               "-deprecation",
                               "-Xlint:deprecation")

      def removePair(name: String, options: Seq[String]): Seq[String] = {
        val index = options.indexOf(name)

        if (index == -1) options
        else {
          val (prefix, suffix) = options.splitAt(index)
          prefix ++ suffix.drop(2)
        }
      }

      removePair(
          "-source",
          removePair("-target", options.filterNot(handledOptions.contains)))
    }
  }
}
