package org.jetbrains.plugins.scala
package project

import com.intellij.ProjectTopics
import com.intellij.openapi.components.AbstractProjectComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.{ModuleRootAdapter, ModuleRootEvent}

/**
  * @author Pavel Fatin
  */
class ScalaProjectEvents(project: Project)
    extends AbstractProjectComponent(project) {
  private var listeners: List[ScalaProjectListener] = Nil

  private val connection = project.getMessageBus.connect()

  override def projectOpened() = {
    connection.subscribe(ProjectTopics.PROJECT_ROOTS, new ModuleRootAdapter {
      override def rootsChanged(event: ModuleRootEvent) {
        listeners.foreach(_.onScalaProjectChanged())
      }
    })
  }

  override def projectClosed() {
    listeners = Nil
    connection.disconnect()
  }

  def addScalaProjectListener(listener: ScalaProjectListener) {
    listeners ::= listener
  }

  def removeScalaProjectListener(listener: ScalaProjectListener) {
    listeners = listeners.filterNot(_ == listener)
  }
}

trait ScalaProjectListener {
  def onScalaProjectChanged()
}
