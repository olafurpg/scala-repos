package mesosphere.marathon.upgrade

import akka.actor.Props
import akka.testkit.TestActorRef
import mesosphere.marathon.core.task.Task
import mesosphere.marathon.core.task.tracker.TaskTracker
import mesosphere.marathon.event.{AppTerminatedEvent, HistoryActor, MesosStatusUpdateEvent}
import mesosphere.marathon.state.{AppDefinition, PathId, TaskFailure, TaskFailureRepository}
import mesosphere.marathon.test.MarathonActorSupport
import mesosphere.marathon.upgrade.StoppingBehavior.SynchronizeTasks
import mesosphere.marathon.{MarathonSpec, MarathonTestHelper, TaskUpgradeCanceledException}
import org.apache.mesos.SchedulerDriver
import org.mockito.Matchers.any
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterAll, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}

class AppStopActorTest
    extends MarathonActorSupport with MarathonSpec with Matchers
    with BeforeAndAfterAll with MockitoSugar

  var driver: SchedulerDriver = _
  var taskTracker: TaskTracker = _
  var taskFailureRepository: TaskFailureRepository = _

  before
    driver = mock[SchedulerDriver]
    taskTracker = mock[TaskTracker]
    taskFailureRepository = mock[TaskFailureRepository]

  test("Stop App")
    val app = AppDefinition(id = PathId("app"), instances = 2)
    val promise = Promise[Unit]()
    val tasks = Set(MarathonTestHelper.runningTask("task_a"),
                    MarathonTestHelper.runningTask("task_b"))

    when(taskTracker.appTasksLaunchedSync(app.id)).thenReturn(tasks)

    val ref = TestActorRef[AppStopActor](
        Props(
            new AppStopActor(
                driver,
                taskTracker,
                system.eventStream,
                app,
                promise
            ))
    )
    watch(ref)

    val historyRef = TestActorRef[HistoryActor](
        Props(
            new HistoryActor(
                system.eventStream,
                taskFailureRepository
            )
        )
    )

    val statusUpdateEventA = MesosStatusUpdateEvent(
        slaveId = "",
        taskId = Task.Id("task_a"),
        taskStatus = "TASK_FAILED",
        message = "",
        appId = app.id,
        host = "",
        ipAddresses = Nil,
        ports = Nil,
        version = app.version.toString
    )

    val statusUpdateEventB = MesosStatusUpdateEvent(
        slaveId = "",
        taskId = Task.Id("task_b"),
        taskStatus = "TASK_LOST",
        message = "",
        appId = app.id,
        host = "",
        ipAddresses = Nil,
        ports = Nil,
        version = app.version.toString
    )

    val Some(taskFailureA) =
      TaskFailure.FromMesosStatusUpdateEvent(statusUpdateEventA)

    val Some(taskFailureB) =
      TaskFailure.FromMesosStatusUpdateEvent(statusUpdateEventB)

    system.eventStream.publish(statusUpdateEventA)
    system.eventStream.publish(statusUpdateEventB)

    Await.result(promise.future, 5.seconds)

    verify(driver, times(2)).killTask(any())

    system.eventStream.publish(AppTerminatedEvent(app.id))

    expectTerminated(ref)

    watch(historyRef)
    verify(taskFailureRepository, times(1)).store(app.id, taskFailureA)
    verify(taskFailureRepository, times(1)).store(app.id, taskFailureB)

    verify(taskFailureRepository, times(1)).expunge(app.id)

  test("Stop App without running tasks")
    val app = AppDefinition(id = PathId("app"), instances = 2)
    val promise = Promise[Unit]()

    when(taskTracker.appTasksLaunchedSync(app.id))
      .thenReturn(Iterable.empty[Task])

    val ref = TestActorRef[AppStopActor](
        Props(
            new AppStopActor(
                driver,
                taskTracker,
                system.eventStream,
                app,
                promise
            ))
    )
    watch(ref)

    Await.result(promise.future, 5.seconds)

    verify(driver, times(0)).killTask(any())
    expectTerminated(ref)

  test("Failed")
    val app = AppDefinition(id = PathId("app"), instances = 2)
    val promise = Promise[Unit]()
    val tasks = Set(MarathonTestHelper.runningTask("task_a"),
                    MarathonTestHelper.runningTask("task_b"))

    when(taskTracker.appTasksLaunchedSync(app.id)).thenReturn(tasks)

    val ref = TestActorRef[AppStopActor](
        Props(
            new AppStopActor(
                driver,
                taskTracker,
                system.eventStream,
                app,
                promise
            ))
    )
    watch(ref)

    ref.stop()

    intercept[TaskUpgradeCanceledException]
      Await.result(promise.future, 5.seconds)

    verify(driver, times(2)).killTask(any())
    expectTerminated(ref)

  test("Task synchronization")
    val app = AppDefinition(id = PathId("app"), instances = 2)
    val promise = Promise[Unit]()
    val tasks = Set(MarathonTestHelper.runningTask("task_a"),
                    MarathonTestHelper.runningTask("task_b"))

    when(taskTracker.appTasksLaunchedSync(app.id))
      .thenReturn(tasks)
      .thenReturn(Iterable.empty[Task])

    val ref = TestActorRef[AppStopActor](
        Props(
            classOf[AppStopActor],
            driver,
            taskTracker,
            system.eventStream,
            app,
            promise
        )
    )
    watch(ref)

    ref.underlyingActor.periodicalCheck.cancel()

    ref ! SynchronizeTasks

    Await.result(promise.future, 10.seconds) should be(())

    verify(driver, times(2)).killTask(any())
    expectTerminated(ref)
