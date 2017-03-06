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

package org.apache.spark.sql.catalyst.optimizer

import org.apache.spark.sql.catalyst.analysis.EliminateSubqueryAliases
import org.apache.spark.sql.catalyst.dsl.expressions._
import org.apache.spark.sql.catalyst.dsl.plans._
import org.apache.spark.sql.catalyst.plans.PlanTest
import org.apache.spark.sql.catalyst.plans.logical._
import org.apache.spark.sql.catalyst.rules._

class SetOperationSuite extends PlanTest {
  object Optimize extends RuleExecutor[LogicalPlan] {
    val batches =
      Batch("Subqueries", Once, EliminateSubqueryAliases) :: Batch(
      "Union Pushdown",
      Once,
      CombineUnions,
      SetOperationPushDown,
      PruneFilters) :: Nil
  }

  val testRelation = LocalRelation('a.int, 'b.int, 'c.int)
  val testRelation2 = LocalRelation('d.int, 'e.int, 'f.int)
  val testRelation3 = LocalRelation('g.int, 'h.int, 'i.int)
  val testUnion = Union(testRelation :: testRelation2 :: testRelation3 :: Nil)
  val testExcept = Except(testRelation, testRelation2)

  test("union: combine unions into one unions") {
    val unionQuery1 = Union(Union(testRelation, testRelation2), testRelation)
    val unionQuery2 = Union(testRelation, Union(testRelation2, testRelation))
    val unionOptimized1 = Optimize.execute(unionQuery1.analyze)
    val unionOptimized2 = Optimize.execute(unionQuery2.analyze)

    comparePlans(unionOptimized1, unionOptimized2)

    val combinedUnions = Union(unionOptimized1 :: unionOptimized2 :: Nil)
    val combinedUnionsOptimized = Optimize.execute(combinedUnions.analyze)
    val unionQuery3 = Union(unionQuery1, unionQuery2)
    val unionOptimized3 = Optimize.execute(unionQuery3.analyze)
    comparePlans(combinedUnionsOptimized, unionOptimized3)
  }

  test("except: filter to each side") {
    val exceptQuery = testExcept.where('c >= 5)
    val exceptOptimized = Optimize.execute(exceptQuery.analyze)
    val exceptCorrectAnswer =
      Except(testRelation.where('c >= 5), testRelation2.where('f >= 5)).analyze

    comparePlans(exceptOptimized, exceptCorrectAnswer)
  }

  test("union: filter to each side") {
    val unionQuery = testUnion.where('a === 1)
    val unionOptimized = Optimize.execute(unionQuery.analyze)
    val unionCorrectAnswer = Union(
      testRelation.where('a === 1) :: testRelation2
        .where('d === 1) :: testRelation3.where('g === 1) :: Nil).analyze

    comparePlans(unionOptimized, unionCorrectAnswer)
  }

  test("union: project to each side") {
    val unionQuery = testUnion.select('a)
    val unionOptimized = Optimize.execute(unionQuery.analyze)
    val unionCorrectAnswer = Union(
      testRelation.select('a) :: testRelation2
        .select('d) :: testRelation3.select('g) :: Nil).analyze
    comparePlans(unionOptimized, unionCorrectAnswer)
  }

  test(
    "SPARK-10539: Project should not be pushed down through Intersect or Except") {
    val exceptQuery = testExcept.select('a, 'b, 'c)
    val exceptOptimized = Optimize.execute(exceptQuery.analyze)
    comparePlans(exceptOptimized, exceptQuery.analyze)
  }
}
