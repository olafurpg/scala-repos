package mesosphere.marathon.upgrade

import java.net.URL
import java.util.UUID

import com.wix.accord.dsl._
import com.wix.accord._
import mesosphere.marathon.Protos
import mesosphere.marathon.core.task.Task
import mesosphere.marathon.state._
import mesosphere.marathon.api.v2.Validation._
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.collection.SortedMap
import scala.collection.immutable.Seq

sealed trait DeploymentAction {
  def app: AppDefinition
}

// application has not been started before
final case class StartApplication(app: AppDefinition, scaleTo: Int)
    extends DeploymentAction

// application is started, but the instance count should be changed
final case class ScaleApplication(app: AppDefinition,
                                  scaleTo: Int,
                                  sentencedToDeath: Option[Iterable[Task]] =
                                    None)
    extends DeploymentAction

// application is started, but shall be completely stopped
final case class StopApplication(app: AppDefinition) extends DeploymentAction

// application is there but should be replaced
final case class RestartApplication(app: AppDefinition)
    extends DeploymentAction

// resolve and store artifacts for given app
final case class ResolveArtifacts(app: AppDefinition,
                                  url2Path: Map[URL, String])
    extends DeploymentAction

/**
  * One step in a deployment plan.
  * The contained actions may be executed in parallel.
  *
  * @param actions the actions of this step that maybe executed in parallel
  */
final case class DeploymentStep(actions: Seq[DeploymentAction]) {
  def +(step: DeploymentStep): DeploymentStep =
    DeploymentStep(actions ++ step.actions)
  def nonEmpty(): Boolean = actions.nonEmpty
}

/**
  * A deployment plan consists of the [[mesosphere.marathon.upgrade.DeploymentStep]]s necessary to
  * change the group state from original to target.
  *
  * The steps are executed sequentially after each other. The actions within a
  * step maybe executed in parallel.
  *
  * See `mesosphere.marathon.upgrade.DeploymentPlan.appsGroupedByLongestPath` to
  * understand how we can guarantee that all dependencies for a step are fulfilled
  * by prior steps.
  */
final case class DeploymentPlan(id: String,
                                original: Group,
                                target: Group,
                                steps: Seq[DeploymentStep],
                                version: Timestamp)
    extends MarathonState[Protos.DeploymentPlanDefinition, DeploymentPlan] {

  /**
    * Reverts this plan by applying the reverse changes to the given Group.
    */
  def revert(group: Group): Group =
    DeploymentPlanReverter.revert(original, target)(group)

  def isEmpty: Boolean = steps.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def affectedApplications: Set[AppDefinition] =
    steps.flatMap(_.actions.map(_.app)).toSet

  /** @return all ids of apps which are referenced in any deployment actions */
  lazy val affectedApplicationIds: Set[PathId] =
    steps.flatMap(_.actions.map(_.app.id)).toSet

  def isAffectedBy(other: DeploymentPlan): Boolean =
    // FIXME: check for group change conflicts?
    affectedApplicationIds.intersect(other.affectedApplicationIds).nonEmpty

  def createdOrUpdatedApps: Seq[AppDefinition] = {
    target.transitiveApps.toIndexedSeq.filter(app =>
      affectedApplicationIds(app.id))
  }

  override def toString: String = {
    def appString(app: AppDefinition): String = {
      val cmdString = app.cmd.fold("")(cmd => ", cmd=\"" + cmd + "\"")
      val argsString =
        app.args.fold("")(args => ", args=\"" + args.mkString(" ") + "\"")
      val maybeDockerImage: Option[String] =
        app.container.flatMap(_.docker.map(_.image))
      val dockerImageString =
        maybeDockerImage.fold("")(image => ", image=\"" + image + "\"")

      s"App(${app.id}$dockerImageString$cmdString$argsString))"
    }
    def actionString(a: DeploymentAction): String = a match {
      case StartApplication(app, scale) =>
        s"Start(${appString(app)}, instances=$scale)"
      case StopApplication(app) => s"Stop(${appString(app)})"
      case ScaleApplication(app, scale, toKill) =>
        val killTasksString = toKill
          .filter(_.nonEmpty)
          .map(", killTasks=" + _.map(_.taskId.idString).mkString(","))
          .getOrElse("")
        s"Scale(${appString(app)}, instances=$scale$killTasksString)"
      case RestartApplication(app) => s"Restart(${appString(app)})"
      case ResolveArtifacts(app, urls) => s"Resolve(${appString(app)}, $urls})"
    }
    val stepString =
      if (steps.nonEmpty) {
        steps
          .map {
            _.actions.map(actionString).mkString("  * ", "\n  * ", "")
          }
          .zipWithIndex
          .map {
            case (stepsString, index) => s"step ${index + 1}:\n$stepsString"
          }
          .mkString("\n", "\n", "")
      } else " NO STEPS"
    s"DeploymentPlan $version$stepString\n"
  }

  override def mergeFromProto(bytes: Array[Byte]): DeploymentPlan =
    mergeFromProto(Protos.DeploymentPlanDefinition.parseFrom(bytes))

  override def mergeFromProto(
      msg: Protos.DeploymentPlanDefinition): DeploymentPlan =
    DeploymentPlan(
      original = Group.empty.mergeFromProto(msg.getOriginal),
      target = Group.empty.mergeFromProto(msg.getTarget),
      version = Timestamp(msg.getVersion)
    ).copy(id = msg.getId)

  override def toProto: Protos.DeploymentPlanDefinition =
    Protos.DeploymentPlanDefinition.newBuilder
      .setId(id)
      .setOriginal(original.toProto)
      .setTarget(target.toProto)
      .setVersion(version.toString)
      .build()
}

object DeploymentPlan {
  private val log = LoggerFactory.getLogger(getClass)

  def empty: DeploymentPlan =
    DeploymentPlan(UUID.randomUUID().toString,
                   Group.empty,
                   Group.empty,
                   Nil,
                   Timestamp.now())

  def fromProto(message: Protos.DeploymentPlanDefinition): DeploymentPlan =
    empty.mergeFromProto(message)

  /**
    * Returns a sorted map where each value is a subset of the supplied group's
    * apps and for all members of each subset, the longest path in the group's
    * dependency graph starting at that member is the same size.  The result
    * map is sorted by its keys, which are the lengths of the longest path
    * starting at the value set's elements.
    *
    * Rationale:
    *
    * #: AppDefinition → ℤ is an equivalence relation on AppDefinition where
    * the members of each equivalence class can be concurrently deployed.
    *
    * This follows naturally:
    *
    * The dependency graph is guaranteed to be free of cycles.
    * By definition for all α, β in some class X, # α = # β.
    * Choose any two apps α and β in a class X.
    * Suppose α transitively depends on β.
    * Then # α must be greater than # β.
    * Which is absurd.
    *
    * Furthermore, for any two apps α in class X and β in a class Y, X ≠ Y
    * where # α is less than # β: α does not transitively depend on β, by
    * similar logic.
    */
  private[upgrade] def appsGroupedByLongestPath(
      group: Group): SortedMap[Int, Set[AppDefinition]] = {

    import org.jgrapht.DirectedGraph
    import org.jgrapht.graph.DefaultEdge

    def longestPathFromVertex[V](g: DirectedGraph[V, DefaultEdge],
                                 vertex: V): Seq[V] = {
      val outgoingEdges: Set[DefaultEdge] =
        if (g.containsVertex(vertex)) g.outgoingEdgesOf(vertex).asScala.toSet
        else Set[DefaultEdge]()

      if (outgoingEdges.isEmpty) Seq(vertex)
      else
        outgoingEdges
          .map { e =>
            vertex +: longestPathFromVertex(g, g.getEdgeTarget(e))
          }
          .maxBy(_.length)
    }

    val unsortedEquivalenceClasses = group.transitiveApps.groupBy { app =>
      longestPathFromVertex(group.dependencyGraph, app).length
    }

    SortedMap(unsortedEquivalenceClasses.toSeq: _*)
  }

  /**
    * Returns a sequence of deployment steps, the order of which is derived
    * from the topology of the target group's dependency graph.
    */
  def dependencyOrderedSteps(
      original: Group,
      target: Group,
      toKill: Map[PathId, Iterable[Task]]): Seq[DeploymentStep] = {
    val originalApps: Map[PathId, AppDefinition] =
      original.transitiveApps.map(app => app.id -> app).toMap

    val appsByLongestPath: SortedMap[Int, Set[AppDefinition]] =
      appsGroupedByLongestPath(target)

    appsByLongestPath.valuesIterator
      .map { (equivalenceClass: Set[AppDefinition]) =>
        val actions: Set[DeploymentAction] = equivalenceClass.flatMap {
          (newApp: AppDefinition) =>
            originalApps.get(newApp.id) match {
              // New app.
              case None =>
                Some(ScaleApplication(newApp, newApp.instances))

              // Scale-only change.
              case Some(oldApp) if oldApp.isOnlyScaleChange(newApp) =>
                Some(
                  ScaleApplication(newApp,
                                   newApp.instances,
                                   toKill.get(newApp.id)))

              // Update or restart an existing app.
              case Some(oldApp) if oldApp.needsRestart(newApp) =>
                Some(RestartApplication(newApp))

              // Other cases require no action.
              case _ =>
                None
            }
        }

        DeploymentStep(actions.to[Seq])
      }
      .to[Seq]
  }

  /**
    * @param original the root group before the deployment
    * @param target the root group after the deployment
    * @param resolveArtifacts artifacts to resolve
    * @param version the version to use for new AppDefinitions (should be very close to now)
    * @param toKill specific tasks that should be killed
    * @return The deployment plan containing the steps necessary to get from the original to the target group definition
    */
  //scalastyle:off method.length
  def apply(
      original: Group,
      target: Group,
      resolveArtifacts: Seq[ResolveArtifacts] = Seq.empty,
      version: Timestamp = Timestamp.now(),
      toKill: Map[PathId, Iterable[Task]] = Map.empty): DeploymentPlan = {

    // Lookup maps for original and target apps.
    val originalApps: Map[PathId, AppDefinition] =
      original.transitiveApps.map(app => app.id -> app).toMap

    val targetApps: Map[PathId, AppDefinition] =
      target.transitiveApps.map(app => app.id -> app).toMap

    // A collection of deployment steps for this plan.
    val steps = Seq.newBuilder[DeploymentStep]

    // 0. Resolve artifacts.
    steps += DeploymentStep(resolveArtifacts)

    // 1. Destroy apps that do not exist in the target.
    steps += DeploymentStep(
      (originalApps -- targetApps.keys).valuesIterator
        .map { oldApp =>
          StopApplication(oldApp)
        }
        .to[Seq]
    )

    // 2. Start apps that do not exist in the original, requiring only 0
    //    instances.  These are scaled as needed in the dependency-ordered
    //    steps that follow.
    steps += DeploymentStep(
      (targetApps -- originalApps.keys).valuesIterator
        .map { newApp =>
          StartApplication(newApp, 0)
        }
        .to[Seq]
    )

    // 3. For each app in each dependency class,
    //
    //      A. If this app is new, scale to the target number of instances.
    //
    //      B. If this is a scale change only, scale to the target number of
    //         instances.
    //
    //      C. Otherwise, if this is an app update:
    //         i. Scale down to the target minimumHealthCapacity fraction of
    //            the old app or the new app, whichever is less.
    //         ii. Restart the app, up to the new target number of instances.
    //
    steps ++= dependencyOrderedSteps(original, target, toKill)

    // Build the result.
    val result = DeploymentPlan(
      UUID.randomUUID().toString,
      original,
      target,
      steps.result().filter(_.actions.nonEmpty),
      version
    )

    result
  }

  implicit lazy val deploymentPlanIsValid: Validator[DeploymentPlan] =
    validator[DeploymentPlan] { plan =>
      plan.createdOrUpdatedApps as "app" is every(
        valid(AppDefinition.updateIsValid(plan.original)))
    }
}
