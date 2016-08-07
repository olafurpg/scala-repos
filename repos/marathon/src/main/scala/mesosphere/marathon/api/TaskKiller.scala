package mesosphere.marathon.api

import javax.inject.Inject

import mesosphere.marathon.core.task.Task
import mesosphere.marathon.core.task.tracker.TaskTracker
import mesosphere.marathon.plugin.auth.{
  Identity, UpdateApp, Authenticator, Authorizer
}
import mesosphere.marathon.state._
import mesosphere.marathon.upgrade.DeploymentPlan
import mesosphere.marathon.{
  MarathonConf, MarathonSchedulerService, UnknownAppException
}

import scala.concurrent.Future

class TaskKiller @Inject()(taskTracker: TaskTracker,
                           groupManager: GroupManager,
                           service: MarathonSchedulerService,
                           val config: MarathonConf,
                           val authenticator: Authenticator,
                           val authorizer: Authorizer)
    extends AuthResource {

  def kill(appId: PathId, findToKill: (Iterable[Task] => Iterable[Task]))(
      implicit identity: Identity): Future[Iterable[Task]] = {
    result(groupManager.app(appId)) match {
      case Some(app) =>
        checkAuthorization(UpdateApp, app)

        val tasks = taskTracker.appTasksLaunchedSync(appId)
        val toKill = findToKill(tasks)
        if (toKill.nonEmpty) service.killTasks(appId, toKill)

        Future.successful(toKill)
      case None => Future.failed(UnknownAppException(appId))
    }
  }

  def killAndScale(
      appId: PathId,
      findToKill: (Iterable[Task] => Iterable[Task]),
      force: Boolean)(implicit identity: Identity): Future[DeploymentPlan] = {
    killAndScale(
        Map(appId -> findToKill(taskTracker.appTasksLaunchedSync(appId))),
        force)
  }

  def killAndScale(appTasks: Map[PathId, Iterable[Task]], force: Boolean)(
      implicit identity: Identity): Future[DeploymentPlan] = {
    def scaleApp(app: AppDefinition): AppDefinition = {
      checkAuthorization(UpdateApp, app)
      appTasks.get(app.id).fold(app) { toKill =>
        app.copy(instances = app.instances - toKill.size)
      }
    }

    def updateGroup(group: Group): Group = {
      group.copy(apps = group.apps.map(scaleApp),
                 groups = group.groups.map(updateGroup))
    }

    def killTasks = groupManager.update(
        PathId.empty,
        updateGroup,
        Timestamp.now(),
        force = force,
        toKill = appTasks
    )

    appTasks.keys
      .find(id => !taskTracker.hasAppTasksSync(id))
      .map(id => Future.failed(UnknownAppException(id)))
      .getOrElse(killTasks)
  }
}
