package mesosphere.marathon.tasks

import mesosphere.marathon.core.launcher.impl.TaskOpFactoryHelper
import mesosphere.marathon.test.Mockito
import mesosphere.marathon.{MarathonSpec, MarathonTestHelper}
import org.apache.mesos.{Protos => Mesos}
import org.scalatest.{GivenWhenThen, Matchers}

class TaskOpFactoryHelperTest
    extends MarathonSpec
    with GivenWhenThen
    with Mockito
    with Matchers {

  test("exception when newTask.taskId and taskInfo.id don't match") {
    val f = new Fixture

    Given("A non-matching task and taskInfo")
    val task = MarathonTestHelper.mininimalTask("123")
    val taskInfo = Mesos.TaskInfo.getDefaultInstance

    When("We create a launch operation")
    val error = intercept[AssertionError] {
      f.helper.launch(taskInfo, task)
    }

    Then("An exception is thrown")
    error.getMessage shouldEqual "assumption failed: marathon task id and mesos task id must be equal"
  }

  test("Create a launch TaskOp") {
    val f = new Fixture

    Given("a task and a taskInfo")
    val task = MarathonTestHelper.mininimalTask("123")
    val taskInfo =
      MarathonTestHelper.makeOneCPUTask(task.taskId.idString).build()

    When("We create a launch operation")
    val launch = f.helper.launch(taskInfo, task)

    Then("The result is as expected")
    launch.newTask shouldEqual task
    launch.taskInfo shouldEqual taskInfo
    launch.oldTask shouldBe empty
    (launch.offerOperations should have).size(1)
    launch.offerOperations.head.getType shouldEqual Mesos.Offer.Operation.Type.LAUNCH
  }

  class Fixture {
    val helper = new TaskOpFactoryHelper(Some("principal"), Some("role"))
  }
}
