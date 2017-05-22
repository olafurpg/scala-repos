package org.jetbrains.sbt.settings

import com.intellij.openapi.components.ApplicationComponent
import com.intellij.openapi.externalSystem.util.ExternalSystemConstants
import com.intellij.openapi.util.registry.Registry
import org.jetbrains.sbt.project.SbtProjectSystem

/**
  * @author Nikolay Obedin
  * @since 12/18/15.
  */
class StartupRoutine extends ApplicationComponent.Adapter
  override def initComponent(): Unit =
    setUpExternalSystemToPerformImportInIdeaProcess()

  private def setUpExternalSystemToPerformImportInIdeaProcess(): Unit =
    Registry
      .get(SbtProjectSystem.Id +
          ExternalSystemConstants.USE_IN_PROCESS_COMMUNICATION_REGISTRY_KEY_SUFFIX)
      .setValue(true)
