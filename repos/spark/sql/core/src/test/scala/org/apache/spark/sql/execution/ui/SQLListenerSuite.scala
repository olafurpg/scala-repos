/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.spark.sql.execution.ui

import java.util.Properties

import org.mockito.Mockito.{mock, when}

import org.apache.spark.{
  SparkConf,
  SparkContext,
  SparkException,
  SparkFunSuite
}
import org.apache.spark.executor.TaskMetrics
import org.apache.spark.scheduler._
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.sql.catalyst.util.quietly
import org.apache.spark.sql.execution.{SparkPlanInfo, SQLExecution}
import org.apache.spark.sql.execution.metric.{LongSQLMetricValue, SQLMetrics}
import org.apache.spark.sql.test.SharedSQLContext
import org.apache.spark.ui.SparkUI

class SQLListenerSuite extends SparkFunSuite with SharedSQLContext {
  import testImplicits._

  private def createTestDataFrame: DataFrame = {
    Seq(
      (1, 1),
      (2, 2)
    ).toDF().filter("_1 > 1")
  }

  private def createProperties(executionId: Long): Properties = {
    val properties = new Properties()
    properties.setProperty(SQLExecution.EXECUTION_ID_KEY, executionId.toString)
    properties
  }

  private def createStageInfo(stageId: Int, attemptId: Int): StageInfo =
    new StageInfo(
      stageId = stageId,
      attemptId = attemptId,
      // The following fields are not used in tests
      name = "",
      numTasks = 0,
      rddInfos = Nil,
      parentIds = Nil,
      details = ""
    )

  private def createTaskInfo(taskId: Int, attemptNumber: Int): TaskInfo =
    new TaskInfo(
      taskId = taskId,
      attemptNumber = attemptNumber,
      // The following fields are not used in tests
      index = 0,
      launchTime = 0,
      executorId = "",
      host = "",
      taskLocality = null,
      speculative = false
    )

  private def createTaskMetrics(
      accumulatorUpdates: Map[Long, Long]): TaskMetrics = {
    val metrics = mock(classOf[TaskMetrics])
    when(metrics.accumulatorUpdates()).thenReturn(
      accumulatorUpdates
        .map {
          case (id, update) =>
            new AccumulableInfo(id,
                                Some(""),
                                Some(new LongSQLMetricValue(update)),
                                value = None,
                                internal = true,
                                countFailedValues = true)
        }
        .toSeq)
    metrics
  }

  test("basic") {
    def checkAnswer(actual: Map[Long, String],
                    expected: Map[Long, Long]): Unit = {
      assert(actual === expected.mapValues(_.toString))
    }

    val listener = new SQLListener(sqlContext.sparkContext.conf)
    val executionId = 0
    val df = createTestDataFrame
    val accumulatorIds = SparkPlanGraph(
      SparkPlanInfo.fromSparkPlan(df.queryExecution.executedPlan)).allNodes
      .flatMap(_.metrics.map(_.accumulatorId))
    // Assume all accumulators are long
    var accumulatorValue = 0L
    val accumulatorUpdates = accumulatorIds
      .map { id =>
        accumulatorValue += 1L
        (id, accumulatorValue)
      }
      .toMap

    listener.onOtherEvent(
      SparkListenerSQLExecutionStart(
        executionId,
        "test",
        "test",
        df.queryExecution.toString,
        SparkPlanInfo.fromSparkPlan(df.queryExecution.executedPlan),
        System.currentTimeMillis()))

    val executionUIData = listener.executionIdToData(0)

    listener.onJobStart(
      SparkListenerJobStart(jobId = 0,
                            time = System.currentTimeMillis(),
                            stageInfos = Seq(
                              createStageInfo(0, 0),
                              createStageInfo(1, 0)
                            ),
                            createProperties(executionId)))
    listener.onStageSubmitted(
      SparkListenerStageSubmitted(createStageInfo(0, 0)))

    assert(listener.getExecutionMetrics(0).isEmpty)

    listener.onExecutorMetricsUpdate(SparkListenerExecutorMetricsUpdate(
      "",
      Seq(
        // (task id, stage id, stage attempt, accum updates)
        (0L, 0, 0, createTaskMetrics(accumulatorUpdates).accumulatorUpdates()),
        (1L, 0, 0, createTaskMetrics(accumulatorUpdates).accumulatorUpdates())
      )))

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 2))

    listener.onExecutorMetricsUpdate(
      SparkListenerExecutorMetricsUpdate(
        "",
        Seq(
          // (task id, stage id, stage attempt, accum updates)
          (0L,
           0,
           0,
           createTaskMetrics(accumulatorUpdates).accumulatorUpdates()),
          (1L,
           0,
           0,
           createTaskMetrics(accumulatorUpdates.mapValues(_ * 2))
             .accumulatorUpdates())
        )))

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 3))

    // Retrying a stage should reset the metrics
    listener.onStageSubmitted(
      SparkListenerStageSubmitted(createStageInfo(0, 1)))

    listener.onExecutorMetricsUpdate(SparkListenerExecutorMetricsUpdate(
      "",
      Seq(
        // (task id, stage id, stage attempt, accum updates)
        (0L, 0, 1, createTaskMetrics(accumulatorUpdates).accumulatorUpdates()),
        (1L, 0, 1, createTaskMetrics(accumulatorUpdates).accumulatorUpdates())
      )))

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 2))

    // Ignore the task end for the first attempt
    listener.onTaskEnd(
      SparkListenerTaskEnd(
        stageId = 0,
        stageAttemptId = 0,
        taskType = "",
        reason = null,
        createTaskInfo(0, 0),
        createTaskMetrics(accumulatorUpdates.mapValues(_ * 100))))

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 2))

    // Finish two tasks
    listener.onTaskEnd(
      SparkListenerTaskEnd(
        stageId = 0,
        stageAttemptId = 1,
        taskType = "",
        reason = null,
        createTaskInfo(0, 0),
        createTaskMetrics(accumulatorUpdates.mapValues(_ * 2))))
    listener.onTaskEnd(
      SparkListenerTaskEnd(
        stageId = 0,
        stageAttemptId = 1,
        taskType = "",
        reason = null,
        createTaskInfo(1, 0),
        createTaskMetrics(accumulatorUpdates.mapValues(_ * 3))))

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 5))

    // Summit a new stage
    listener.onStageSubmitted(
      SparkListenerStageSubmitted(createStageInfo(1, 0)))

    listener.onExecutorMetricsUpdate(SparkListenerExecutorMetricsUpdate(
      "",
      Seq(
        // (task id, stage id, stage attempt, accum updates)
        (0L, 1, 0, createTaskMetrics(accumulatorUpdates).accumulatorUpdates()),
        (1L, 1, 0, createTaskMetrics(accumulatorUpdates).accumulatorUpdates())
      )))

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 7))

    // Finish two tasks
    listener.onTaskEnd(
      SparkListenerTaskEnd(
        stageId = 1,
        stageAttemptId = 0,
        taskType = "",
        reason = null,
        createTaskInfo(0, 0),
        createTaskMetrics(accumulatorUpdates.mapValues(_ * 3))))
    listener.onTaskEnd(
      SparkListenerTaskEnd(
        stageId = 1,
        stageAttemptId = 0,
        taskType = "",
        reason = null,
        createTaskInfo(1, 0),
        createTaskMetrics(accumulatorUpdates.mapValues(_ * 3))))

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 11))

    assert(executionUIData.runningJobs === Seq(0))
    assert(executionUIData.succeededJobs.isEmpty)
    assert(executionUIData.failedJobs.isEmpty)

    listener.onJobEnd(
      SparkListenerJobEnd(
        jobId = 0,
        time = System.currentTimeMillis(),
        JobSucceeded
      ))
    listener.onOtherEvent(
      SparkListenerSQLExecutionEnd(executionId, System.currentTimeMillis()))

    assert(executionUIData.runningJobs.isEmpty)
    assert(executionUIData.succeededJobs === Seq(0))
    assert(executionUIData.failedJobs.isEmpty)

    checkAnswer(listener.getExecutionMetrics(0),
                accumulatorUpdates.mapValues(_ * 11))
  }

  test("onExecutionEnd happens before onJobEnd(JobSucceeded)") {
    val listener = new SQLListener(sqlContext.sparkContext.conf)
    val executionId = 0
    val df = createTestDataFrame
    listener.onOtherEvent(
      SparkListenerSQLExecutionStart(
        executionId,
        "test",
        "test",
        df.queryExecution.toString,
        SparkPlanInfo.fromSparkPlan(df.queryExecution.executedPlan),
        System.currentTimeMillis()))
    listener.onJobStart(
      SparkListenerJobStart(jobId = 0,
                            time = System.currentTimeMillis(),
                            stageInfos = Nil,
                            createProperties(executionId)))
    listener.onOtherEvent(
      SparkListenerSQLExecutionEnd(executionId, System.currentTimeMillis()))
    listener.onJobEnd(
      SparkListenerJobEnd(
        jobId = 0,
        time = System.currentTimeMillis(),
        JobSucceeded
      ))

    val executionUIData = listener.executionIdToData(0)
    assert(executionUIData.runningJobs.isEmpty)
    assert(executionUIData.succeededJobs === Seq(0))
    assert(executionUIData.failedJobs.isEmpty)
  }

  test("onExecutionEnd happens before multiple onJobEnd(JobSucceeded)s") {
    val listener = new SQLListener(sqlContext.sparkContext.conf)
    val executionId = 0
    val df = createTestDataFrame
    listener.onOtherEvent(
      SparkListenerSQLExecutionStart(
        executionId,
        "test",
        "test",
        df.queryExecution.toString,
        SparkPlanInfo.fromSparkPlan(df.queryExecution.executedPlan),
        System.currentTimeMillis()))
    listener.onJobStart(
      SparkListenerJobStart(jobId = 0,
                            time = System.currentTimeMillis(),
                            stageInfos = Nil,
                            createProperties(executionId)))
    listener.onJobEnd(
      SparkListenerJobEnd(
        jobId = 0,
        time = System.currentTimeMillis(),
        JobSucceeded
      ))

    listener.onJobStart(
      SparkListenerJobStart(jobId = 1,
                            time = System.currentTimeMillis(),
                            stageInfos = Nil,
                            createProperties(executionId)))
    listener.onOtherEvent(
      SparkListenerSQLExecutionEnd(executionId, System.currentTimeMillis()))
    listener.onJobEnd(
      SparkListenerJobEnd(
        jobId = 1,
        time = System.currentTimeMillis(),
        JobSucceeded
      ))

    val executionUIData = listener.executionIdToData(0)
    assert(executionUIData.runningJobs.isEmpty)
    assert(executionUIData.succeededJobs.sorted === Seq(0, 1))
    assert(executionUIData.failedJobs.isEmpty)
  }

  test("onExecutionEnd happens before onJobEnd(JobFailed)") {
    val listener = new SQLListener(sqlContext.sparkContext.conf)
    val executionId = 0
    val df = createTestDataFrame
    listener.onOtherEvent(
      SparkListenerSQLExecutionStart(
        executionId,
        "test",
        "test",
        df.queryExecution.toString,
        SparkPlanInfo.fromSparkPlan(df.queryExecution.executedPlan),
        System.currentTimeMillis()))
    listener.onJobStart(
      SparkListenerJobStart(jobId = 0,
                            time = System.currentTimeMillis(),
                            stageInfos = Seq.empty,
                            createProperties(executionId)))
    listener.onOtherEvent(
      SparkListenerSQLExecutionEnd(executionId, System.currentTimeMillis()))
    listener.onJobEnd(
      SparkListenerJobEnd(
        jobId = 0,
        time = System.currentTimeMillis(),
        JobFailed(new RuntimeException("Oops"))
      ))

    val executionUIData = listener.executionIdToData(0)
    assert(executionUIData.runningJobs.isEmpty)
    assert(executionUIData.succeededJobs.isEmpty)
    assert(executionUIData.failedJobs === Seq(0))
  }

  test("SPARK-11126: no memory leak when running non SQL jobs") {
    val previousStageNumber = sqlContext.listener.stageIdToStageMetrics.size
    sqlContext.sparkContext.parallelize(1 to 10).foreach(i => ())
    sqlContext.sparkContext.listenerBus.waitUntilEmpty(10000)
    // listener should ignore the non SQL stage
    assert(
      sqlContext.listener.stageIdToStageMetrics.size == previousStageNumber)

    sqlContext.sparkContext.parallelize(1 to 10).toDF().foreach(i => ())
    sqlContext.sparkContext.listenerBus.waitUntilEmpty(10000)
    // listener should save the SQL stage
    assert(
      sqlContext.listener.stageIdToStageMetrics.size == previousStageNumber +
        1)
  }

  test("SPARK-13055: history listener only tracks SQL metrics") {
    val listener =
      new SQLHistoryListener(sparkContext.conf, mock(classOf[SparkUI]))
    // We need to post other events for the listener to track our accumulators.
    // These are largely just boilerplate unrelated to what we're trying to test.
    val df = createTestDataFrame
    val executionStart = SparkListenerSQLExecutionStart(
      0,
      "",
      "",
      "",
      SparkPlanInfo.fromSparkPlan(df.queryExecution.executedPlan),
      0)
    val stageInfo = createStageInfo(0, 0)
    val jobStart =
      SparkListenerJobStart(0, 0, Seq(stageInfo), createProperties(0))
    val stageSubmitted = SparkListenerStageSubmitted(stageInfo)
    // This task has both accumulators that are SQL metrics and accumulators that are not.
    // The listener should only track the ones that are actually SQL metrics.
    val sqlMetric = SQLMetrics.createLongMetric(sparkContext, "beach umbrella")
    val nonSqlMetric = sparkContext.accumulator[Int](0, "baseball")
    val sqlMetricInfo = sqlMetric.toInfo(Some(sqlMetric.localValue), None)
    val nonSqlMetricInfo =
      nonSqlMetric.toInfo(Some(nonSqlMetric.localValue), None)
    val taskInfo = createTaskInfo(0, 0)
    taskInfo.accumulables ++= Seq(sqlMetricInfo, nonSqlMetricInfo)
    val taskEnd =
      SparkListenerTaskEnd(0, 0, "just-a-task", null, taskInfo, null)
    listener.onOtherEvent(executionStart)
    listener.onJobStart(jobStart)
    listener.onStageSubmitted(stageSubmitted)
    // Before SPARK-13055, this throws ClassCastException because the history listener would
    // assume that the accumulator value is of type Long, but this may not be true for
    // accumulators that are not SQL metrics.
    listener.onTaskEnd(taskEnd)
    val trackedAccums = listener.stageIdToStageMetrics.values.flatMap {
      stageMetrics =>
        stageMetrics.taskIdToMetricUpdates.values.flatMap(_.accumulatorUpdates)
    }
    // Listener tracks only SQL metrics, not other accumulators
    assert(trackedAccums.size === 1)
    assert(trackedAccums.head === sqlMetricInfo)
  }
}

class SQLListenerMemoryLeakSuite extends SparkFunSuite {

  test("no memory leak") {
    quietly {
      val conf = new SparkConf()
        .setMaster("local")
        .setAppName("test")
        .set("spark.task.maxFailures", "1") // Don't retry the tasks to run this test quickly
        .set("spark.sql.ui.retainedExecutions", "50") // Set it to 50 to run this test quickly
      val sc = new SparkContext(conf)
      try {
        SQLContext.clearSqlListener()
        val sqlContext = new SQLContext(sc)
        import sqlContext.implicits._
        // Run 100 successful executions and 100 failed executions.
        // Each execution only has one job and one stage.
        for (i <- 0 until 100) {
          val df = Seq(
            (1, 1),
            (2, 2)
          ).toDF()
          df.collect()
          try {
            df.foreach(_ => throw new RuntimeException("Oops"))
          } catch {
            case e: SparkException => // This is expected for a failed job
          }
        }
        sc.listenerBus.waitUntilEmpty(10000)
        assert(sqlContext.listener.getCompletedExecutions.size <= 50)
        assert(sqlContext.listener.getFailedExecutions.size <= 50)
        // 50 for successful executions and 50 for failed executions
        assert(sqlContext.listener.executionIdToData.size <= 100)
        assert(sqlContext.listener.jobIdToExecutionId.size <= 100)
        assert(sqlContext.listener.stageIdToStageMetrics.size <= 100)
      } finally {
        sc.stop()
      }
    }
  }
}
