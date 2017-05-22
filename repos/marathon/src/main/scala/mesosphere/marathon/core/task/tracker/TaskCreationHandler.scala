package mesosphere.marathon.core.task.tracker

import mesosphere.marathon.core.task.Task

import scala.concurrent.Future

/**
  * Notifies the [[TaskTracker]] of task creation and termination.
  */
trait TaskCreationHandler

  /**
    * Create a new task.
    *
    * If the task exists already, the existing task will be overwritten so make sure
    * that you generate unique IDs.
    */
  def created(task: Task): Future[Task]

  /**
    * Remove the task for the given app with the given ID completely.
    *
    * If the task does not exist, the returned Future will not fail.
    */
  def terminated(taskId: Task.Id): Future[_]
