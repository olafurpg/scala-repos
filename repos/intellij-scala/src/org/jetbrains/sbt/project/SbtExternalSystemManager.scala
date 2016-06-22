package org.jetbrains.sbt
package project

import java.io.File
import java.net.URL
import java.util

import com.intellij.execution.configurations.SimpleJavaParameters
import com.intellij.openapi.application.{ApplicationManager, PathManager}
import com.intellij.openapi.externalSystem.model.ExternalSystemException
import com.intellij.openapi.externalSystem.service.project.autoimport.CachingExternalSystemAutoImportAware
import com.intellij.openapi.externalSystem.util._
import com.intellij.openapi.externalSystem.{ExternalSystemAutoImportAware, ExternalSystemConfigurableAware, ExternalSystemManager}
import com.intellij.openapi.options.Configurable
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.impl.JavaAwareProjectJdkTableImpl
import com.intellij.openapi.projectRoots.{JavaSdkType, ProjectJdkTable}
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.testFramework.IdeaTestUtil
import com.intellij.util.net.HttpConfigurable
import org.jetbrains.android.sdk.AndroidSdkType
import org.jetbrains.sbt.project.settings._
import org.jetbrains.sbt.settings.{SbtExternalSystemConfigurable, SbtSystemSettings}

import scala.collection.mutable

/**
  * @author Pavel Fatin
  */
class SbtExternalSystemManager
    extends ExternalSystemManager[SbtProjectSettings,
                                  SbtProjectSettingsListener,
                                  SbtSystemSettings,
                                  SbtLocalSettings,
                                  SbtExecutionSettings]
    with ExternalSystemConfigurableAware {

  def enhanceLocalProcessing(urls: util.List[URL]) {
    urls.add(jarWith[scala.App].toURI.toURL)
  }

  def enhanceRemoteProcessing(parameters: SimpleJavaParameters) {
    val classpath = parameters.getClassPath

    classpath.add(jarWith[this.type])
    classpath.add(jarWith[org.jetbrains.sbt.structure.XmlSerializer[_]])
    classpath.add(jarWith[scala.App])
    classpath.add(jarWith[scala.xml.Node])

    parameters.getVMParametersList.addProperty(
        ExternalSystemConstants.EXTERNAL_SYSTEM_ID_KEY,
        SbtProjectSystem.Id.getId)

    parameters.getVMParametersList
      .addProperty(PathManager.PROPERTY_LOG_PATH, PathManager.getLogPath)
  }

  def getSystemId = SbtProjectSystem.Id

  def getSettingsProvider = SbtSystemSettings.getInstance _

  def getLocalSettingsProvider = SbtLocalSettings.getInstance _

  def getExecutionSettingsProvider =
    SbtExternalSystemManager.executionSettingsFor _

  def getProjectResolverClass = classOf[SbtProjectResolver]

  def getTaskManagerClass = classOf[SbtTaskManager]

  def getExternalProjectDescriptor = new SbtOpenProjectDescriptor()

  def getConfigurable(project: Project): Configurable =
    new SbtExternalSystemConfigurable(project)
}

object SbtExternalSystemManager {
  def executionSettingsFor(project: Project, path: String) = {
    val settings = SbtSystemSettings.getInstance(project)
    val projectSettings = Option(settings.getLinkedProjectSettings(path))
      .getOrElse(SbtProjectSettings.default)

    val customLauncher = settings.customLauncherEnabled
      .option(settings.getCustomLauncherPath)
      .map(_.toFile)
    val customSbtStructureFile = settings.customSbtStructurePath.nonEmpty
      .option(settings.customSbtStructurePath.toFile)

    val realProjectPath =
      Option(projectSettings.getExternalProjectPath).getOrElse(path)
    val projectJdkName = getProjectJdkName(project, projectSettings)
    val vmExecutable = getVmExecutable(projectJdkName, settings)
    val vmOptions = getVmOptions(settings)
    val environment =
      Map.empty ++ getAndroidEnvironmentVariables(projectJdkName)

    new SbtExecutionSettings(realProjectPath,
                             vmExecutable,
                             vmOptions,
                             environment,
                             customLauncher,
                             customSbtStructureFile,
                             projectJdkName,
                             projectSettings.resolveClassifiers,
                             projectSettings.resolveJavadocs,
                             projectSettings.resolveSbtClassifiers)
  }

  private def getProjectJdkName(
      project: Project,
      projectSettings: SbtProjectSettings): Option[String] = {
    val jdkInProject = Option(
        ProjectRootManager.getInstance(project).getProjectSdk).map(_.getName)
    val jdkInImportSettings = projectSettings.jdkName
    jdkInImportSettings.orElse(jdkInProject)
  }

  private def getVmExecutable(projectJdkName: Option[String],
                              settings: SbtSystemSettings): File =
    if (!ApplicationManager.getApplication.isUnitTestMode)
      getRealVmExecutable(projectJdkName, settings)
    else getUnitTestVmExecutable

  private def getUnitTestVmExecutable: File = {
    val internalSdk = JavaAwareProjectJdkTableImpl.getInstanceEx.getInternalJdk
    val sdk =
      if (internalSdk == null) IdeaTestUtil.getMockJdk17 else internalSdk
    val sdkType = sdk.getSdkType.asInstanceOf[JavaSdkType]
    new File(sdkType.getVMExecutablePath(sdk))
  }

  private def getRealVmExecutable(projectJdkName: Option[String],
                                  settings: SbtSystemSettings): File = {
    val customVmFile = new File(settings.getCustomVMPath) / "bin" / "java"
    val customVmExecutable = settings.customVMEnabled.option(customVmFile)

    customVmExecutable.orElse {
      val projectSdk = projectJdkName.flatMap(name =>
            Option(ProjectJdkTable.getInstance().findJdk(name)))
      projectSdk.map { sdk =>
        sdk.getSdkType match {
          case sdkType: JavaSdkType =>
            new File(sdkType.getVMExecutablePath(sdk))
          case _ =>
            throw new ExternalSystemException(
                SbtBundle("sbt.import.noProjectJvmFound"))
        }
      }
    } getOrElse {
      throw new ExternalSystemException(
          SbtBundle("sbt.import.noCustomJvmFound"))
    }
  }

  private def getAndroidEnvironmentVariables(
      projectJdkName: Option[String]): Map[String, String] =
    projectJdkName
      .flatMap(name => Option(ProjectJdkTable.getInstance().findJdk(name)))
      .flatMap { sdk =>
        try {
          sdk.getSdkType
            .isInstanceOf[AndroidSdkType]
            .option(Map("ANDROID_HOME" -> sdk.getSdkModificator.getHomePath))
        } catch {
          case _: NoClassDefFoundError => None
        }
      }
      .getOrElse(Map.empty)

  private def getVmOptions(settings: SbtSystemSettings): Seq[String] = {
    val userOptions = settings.getVmParameters.split("\\s+").toSeq
    val ideaProxyOptions =
      proxyOptionsFor(HttpConfigurable.getInstance).filterNot { opt =>
        val optName = opt.split('=').head + "="
        userOptions.exists(_.startsWith(optName))
      }
    Seq(s"-Xmx${settings.getMaximumHeapSize}M") ++ userOptions ++ ideaProxyOptions
  }

  private def proxyOptionsFor(http: HttpConfigurable): Seq[String] = {
    val useProxy = http.USE_HTTP_PROXY && !http.PROXY_TYPE_IS_SOCKS
    val useCredentials = useProxy && http.PROXY_AUTHENTICATION

    useProxy.seq(s"-Dhttp.proxyHost=${http.PROXY_HOST}",
                 s"-Dhttp.proxyPort=${http.PROXY_PORT}") ++ useCredentials.seq(
        s"-Dhttp.proxyUser=${http.PROXY_LOGIN}",
        s"-Dhttp.proxyPassword=${http.getPlainProxyPassword}")
  }
}
