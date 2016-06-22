package mesosphere.marathon.upgrade

import akka.actor.Props
import akka.event.EventStream
import mesosphere.marathon.core.task.Task
import mesosphere.marathon.core.task.tracker.TaskTracker
import mesosphere.marathon.state.PathId
import org.apache.mesos.SchedulerDriver

import scala.collection.mutable
import scala.concurrent.Promise

class TaskKillActor(val driver: SchedulerDriver,
                    val appId: PathId,
                    val taskTracker: TaskTracker,
                    val eventBus: EventStream,
                    tasksToKill: Iterable[Task.Id],
                    val promise: Promise[Unit])
    extends StoppingBehavior {

  override var idsToKill = tasksToKill.to[mutable.Set]

  def initializeStop(): Unit = {
    log.info(s"Killing ${tasksToKill.size} instances")
    for (taskId <- tasksToKill) {
      driver.killTask(taskId.mesosTaskId)
    }
  }
}

object TaskKillActor {
  def props(driver: SchedulerDriver,
            appId: PathId,
            taskTracker: TaskTracker,
            eventBus: EventStream,
            tasksToKill: Iterable[Task.Id],
            promise: Promise[Unit]): Props = {
    Props(
        new TaskKillActor(driver,
                          appId,
                          taskTracker,
                          eventBus,
                          tasksToKill,
                          promise))
  }
}
