package org.jetbrains.sbt.project.data
package service

import java.io.File

import com.intellij.compiler.CompilerConfiguration
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.notification.{ExternalSystemNotificationManager, NotificationCategory, NotificationData, NotificationSource}
import com.intellij.openapi.externalSystem.service.project.IdeModifiableModelsProvider
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.LanguageLevelModuleExtensionImpl
import com.intellij.openapi.roots.libraries.Library
import org.jetbrains.plugins.scala.project._
import org.jetbrains.sbt.SbtBundle
import org.jetbrains.sbt.project.SbtProjectSystem

/**
  * @author Pavel Fatin
  */
class ModuleExtDataService
    extends AbstractDataService[ModuleExtData, Library](ModuleExtData.Key)
  override def createImporter(
      toImport: Seq[DataNode[ModuleExtData]],
      projectData: ProjectData,
      project: Project,
      modelsProvider: IdeModifiableModelsProvider): Importer[ModuleExtData] =
    new ModuleExtDataService.Importer(
        toImport, projectData, project, modelsProvider)

object ModuleExtDataService
  private class Importer(dataToImport: Seq[DataNode[ModuleExtData]],
                         projectData: ProjectData,
                         project: Project,
                         modelsProvider: IdeModifiableModelsProvider)
      extends AbstractImporter[ModuleExtData](
          dataToImport, projectData, project, modelsProvider)

    override def importData(): Unit =
      dataToImport.foreach(doImport)

    private def doImport(dataNode: DataNode[ModuleExtData]): Unit =
      for
        module <- getIdeModuleByNode(dataNode)
        data = dataNode.getData
      
        module.configureScalaCompilerSettingsFrom("SBT", data.scalacOptions)
        data.scalaVersion.foreach(version =>
              configureScalaSdk(module, version, data.scalacClasspath))
        configureOrInheritSdk(module, data.jdk)
        configureLanguageLevel(module, data.javacOptions)
        configureJavacOptions(module, data.javacOptions)

    private def configureScalaSdk(module: Module,
                                  compilerVersion: Version,
                                  compilerClasspath: Seq[File]): Unit =
      val scalaLibraries = getScalaLibraries(module)
      if (scalaLibraries.nonEmpty)
        val scalaLibrary = scalaLibraries
          .find(_.scalaVersion.contains(compilerVersion))
          .orElse(scalaLibraries.find(_.scalaVersion.exists(
                      _.toLanguageLevel == compilerVersion.toLanguageLevel)))

        scalaLibrary match
          case Some(library) if !library.isScalaSdk =>
            convertToScalaSdk(library,
                              library.scalaLanguageLevel.getOrElse(
                                  ScalaLanguageLevel.Default),
                              compilerClasspath)
          case None =>
            showWarning(
                SbtBundle("sbt.dataService.scalaLibraryIsNotFound",
                          compilerVersion.number,
                          module.getName))
          case _ => // do nothing

    private def configureOrInheritSdk(module: Module, sdk: Option[Sdk]): Unit =
      val model = getModifiableRootModel(module)
      model.inheritSdk()
      sdk.flatMap(SdkUtils.findProjectSdk).foreach(model.setSdk)

    private def configureLanguageLevel(
        module: Module, javacOptions: Seq[String]): Unit =
      val model = getModifiableRootModel(module)
      val moduleSdk = Option(model.getSdk)
      val languageLevel = SdkUtils
        .javaLanguageLevelFrom(javacOptions)
        .orElse(moduleSdk.flatMap(SdkUtils.defaultJavaLanguageLevelIn))
      languageLevel.foreach  level =>
        val extension =
          model.getModuleExtension(classOf[LanguageLevelModuleExtensionImpl])
        extension.setLanguageLevel(level)

    private def configureJavacOptions(
        module: Module, javacOptions: Seq[String]): Unit =
      for
        targetPos <- Option(javacOptions.indexOf("-target")).filterNot(_ == -1)
        targetValue <- javacOptions.lift(targetPos + 1)
        compilerSettings = CompilerConfiguration.getInstance(module.getProject)
      
        executeProjectChangeAction(
            compilerSettings.setBytecodeTargetLevel(module, targetValue))

    private def showWarning(message: String): Unit =
      val notification = new NotificationData(
          SbtBundle("sbt.notificationGroupTitle"),
          message,
          NotificationCategory.WARNING,
          NotificationSource.PROJECT_SYNC)
      notification.setBalloonGroup(SbtBundle("sbt.notificationGroupName"))
      ExternalSystemNotificationManager
        .getInstance(project)
        .showNotification(SbtProjectSystem.Id, notification)
