package mesosphere.marathon.core.task.tracker.impl

import akka.actor.ActorRef
import mesosphere.marathon.core.task.Task
import mesosphere.marathon.state.{PathId, Timestamp}
import org.apache.mesos.Protos.TaskStatus

import scala.concurrent.{ExecutionContext, Future}

private[tracker] object TaskOpProcessor
  case class Operation(
      deadline: Timestamp, sender: ActorRef, taskId: Task.Id, action: Action)
    def appId: PathId = taskId.appId

  sealed trait Action

  object Action

    /** Update an existing task or create a new task. */
    case class Update(task: Task) extends Action
      override def toString: String = "Update/Create"

    /** Remove a task. */
    case object Expunge extends Action

    /**
      * Update a task according to a status update.
      *
      * Internally, this op is mapped to another action after inspecting the current task state.
      */
    case class UpdateStatus(status: TaskStatus) extends Action
      override def toString: String = s"UpdateStatus ${status.getState}"

    /** No change and signal success to sender. */
    private[impl] case object Noop extends Action

    /** No change and signal failure to sender. */
    private[impl] case class Fail(cause: Throwable) extends Action

/**
  * Processes durable operations on tasks.
  */
private[tracker] trait TaskOpProcessor
  def process(op: TaskOpProcessor.Operation)(
      implicit ec: ExecutionContext): Future[Unit]
