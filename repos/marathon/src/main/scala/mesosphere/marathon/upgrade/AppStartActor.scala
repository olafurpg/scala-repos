package mesosphere.marathon.upgrade

import akka.actor._
import akka.event.EventStream
import mesosphere.marathon.core.launchqueue.LaunchQueue
import mesosphere.marathon.core.task.tracker.TaskTracker
import mesosphere.marathon.state.AppDefinition
import mesosphere.marathon.{AppStartCanceledException, SchedulerActions}
import org.apache.mesos.SchedulerDriver

import scala.concurrent.Promise
import scala.util.control.NonFatal

class AppStartActor(val driver: SchedulerDriver,
                    val scheduler: SchedulerActions,
                    val taskQueue: LaunchQueue,
                    val taskTracker: TaskTracker,
                    val eventBus: EventStream,
                    val app: AppDefinition,
                    val scaleTo: Int,
                    promise: Promise[Unit])
    extends Actor
    with ActorLogging
    with StartingBehavior {

  val nrToStart: Int = scaleTo

  def initializeStart(): Unit = {
    scheduler.startApp(driver, app.copy(instances = scaleTo))
  }

  override def postStop(): Unit = {
    eventBus.unsubscribe(self)
    if (!promise.isCompleted) {
      if (promise.tryFailure(new AppStartCanceledException(
              "The app start has been cancelled"))) {
        scheduler
          .stopApp(driver, app)
          .onFailure {
            case NonFatal(e) => log.error(s"while stopping app ${app.id}", e)
          }(context.dispatcher)
      }
    }
  }

  def success(): Unit = {
    log.info(s"Successfully started $scaleTo instances of ${app.id}")
    promise.success(())
    context.stop(self)
  }
}
