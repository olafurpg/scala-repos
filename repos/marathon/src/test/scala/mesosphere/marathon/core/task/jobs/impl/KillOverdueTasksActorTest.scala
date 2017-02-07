package mesosphere.marathon.core.task.jobs.impl

import akka.actor._
import akka.testkit.TestProbe
import mesosphere.marathon
import mesosphere.marathon.core.base.ConstantClock
import mesosphere.marathon.core.task.tracker.TaskTracker
import mesosphere.marathon.core.task.tracker.TaskTracker.TasksByApp
import mesosphere.marathon.state.{PathId, Timestamp}
import mesosphere.marathon.{
  MarathonSchedulerDriverHolder,
  MarathonSpec,
  MarathonTestHelper
}
import mesosphere.mesos.protos.TaskID
import org.apache.mesos.Protos.{TaskState, TaskStatus}
import org.apache.mesos.{Protos => MesosProtos, SchedulerDriver}
import org.mockito.Mockito
import org.mockito.Mockito._
import org.scalatest.GivenWhenThen
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._

class KillOverdueTasksActorTest
    extends MarathonSpec
    with GivenWhenThen
    with marathon.test.Mockito
    with ScalaFutures {
  implicit var actorSystem: ActorSystem = _
  var taskTracker: TaskTracker = _
  var driver: SchedulerDriver = _
  var checkActor: ActorRef = _
  val clock = ConstantClock()

  before {
    actorSystem = ActorSystem()
    taskTracker = mock[TaskTracker]
    driver = mock[SchedulerDriver]
    val driverHolder = new MarathonSchedulerDriverHolder()
    driverHolder.driver = Some(driver)
    val config = MarathonTestHelper.defaultConfig()
    checkActor = actorSystem.actorOf(
      KillOverdueTasksActor.props(config, taskTracker, driverHolder, clock),
      "check")
  }

  after {
    def waitForActorProcessingAllAndDying(): Unit = {
      checkActor ! PoisonPill
      val probe = TestProbe()
      probe.watch(checkActor)
      val terminated = probe.expectMsgAnyClassOf(classOf[Terminated])
      assert(terminated.actor == checkActor)
    }

    waitForActorProcessingAllAndDying()

    actorSystem.shutdown()
    actorSystem.awaitTermination()

    noMoreInteractions(taskTracker, driver)
  }

  test("no overdue tasks") {
    Given("no tasks")
    taskTracker.tasksByAppSync.returns(TasksByApp.empty)

    When("a check is performed")
    val testProbe = TestProbe()
    testProbe.send(checkActor,
                   KillOverdueTasksActor.Check(maybeAck = Some(testProbe.ref)))
    testProbe.expectMsg(3.seconds, Status.Success(()))

    Then("eventually list was called")
    verify(taskTracker).tasksByAppSync
    And("no kill calls are issued")
    noMoreInteractions(driver)
  }

  test("some overdue tasks") {
    Given("one overdue task")
    val mockTask = MarathonTestHelper.stagedTaskProto("someId")
    val app = TaskTracker.AppTasks(PathId("/some"), Iterable(mockTask))
    taskTracker.tasksByAppSync.returns(TasksByApp.of(app))

    When("the check is initiated")
    checkActor ! KillOverdueTasksActor.Check(maybeAck = None)

    Then("the task kill gets initiated")
    verify(taskTracker, Mockito.timeout(1000)).tasksByAppSync
    import mesosphere.mesos.protos.Implicits._
    verify(driver, Mockito.timeout(1000)).killTask(TaskID("someId"))
  }

  // sounds strange, but this is how it currently works: determineOverdueTasks will consider a missing startedAt to
  // determine whether a task is in staging and might need to be killed if it exceeded the taskLaunchTimeout
  test("ensure that check kills tasks disregarding the stagedAt property") {
    import scala.language.implicitConversions
    implicit def toMillis(timestamp: Timestamp): Long =
      timestamp.toDateTime.getMillis

    val now = clock.now()
    val config = MarathonTestHelper.defaultConfig()

    def statusBuilder(id: String, state: TaskState) =
      TaskStatus
        .newBuilder()
        .setTaskId(MesosProtos.TaskID.newBuilder().setValue(id))
        .setState(state)

    val overdueUnstagedTask = MarathonTestHelper.startingTaskProto("unstaged")
    assert(overdueUnstagedTask.getStartedAt == 0,
           "The startedAt property of an unstaged task has a value of 0")

    val unconfirmedNotOverdueTask = MarathonTestHelper.startingTaskProto(
      "unconfirmed",
      stagedAt = now - config.taskLaunchConfirmTimeout().millis)

    val unconfirmedOverdueTask = MarathonTestHelper.startingTaskProto(
      "unconfirmedOverdue",
      stagedAt = now - config.taskLaunchConfirmTimeout().millis - 1.millis
    )

    val overdueStagedTask = MarathonTestHelper.stagedTaskProto(
      "overdueStagedTask",
      stagedAt = now - 10.days
    )

    val stagedTask = MarathonTestHelper.stagedTaskProto(
      "staged",
      stagedAt = now - 10.seconds
    )

    val runningTask =
      MarathonTestHelper.runningTaskProto("running",
                                          stagedAt = now - 5.seconds,
                                          startedAt = now - 2.seconds)

    Given("Several somehow overdue tasks plus some not overdue tasks")
    val appId = PathId("/ignored")
    val app = TaskTracker.AppTasks(
      appId,
      Iterable(
        unconfirmedOverdueTask,
        unconfirmedNotOverdueTask,
        overdueUnstagedTask,
        overdueStagedTask,
        stagedTask,
        runningTask
      )
    )
    taskTracker.tasksByAppSync.returns(TasksByApp.of(app))

    When(
      "We check which tasks should be killed because they're not yet staged or unconfirmed")
    val testProbe = TestProbe()
    testProbe.send(checkActor,
                   KillOverdueTasksActor.Check(maybeAck = Some(testProbe.ref)))
    testProbe.expectMsg(3.seconds, Status.Success(()))

    Then("The task tracker gets queried")
    verify(taskTracker).tasksByAppSync

    And("All somehow overdue tasks are killed")
    verify(driver).killTask(
      MesosProtos.TaskID
        .newBuilder()
        .setValue(unconfirmedOverdueTask.getId)
        .build())
    verify(driver).killTask(
      MesosProtos.TaskID
        .newBuilder()
        .setValue(overdueUnstagedTask.getId)
        .build())
    verify(driver).killTask(
      MesosProtos.TaskID
        .newBuilder()
        .setValue(overdueStagedTask.getId)
        .build())

    And("but not more")
    verifyNoMoreInteractions(driver)
  }
}
