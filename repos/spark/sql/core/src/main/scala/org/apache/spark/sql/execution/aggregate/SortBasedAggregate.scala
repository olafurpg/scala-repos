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

package org.apache.spark.sql.execution.aggregate

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.catalyst.errors._
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.catalyst.expressions.aggregate._
import org.apache.spark.sql.catalyst.plans.physical.{AllTuples, ClusteredDistribution, Distribution, UnspecifiedDistribution}
import org.apache.spark.sql.execution.{SparkPlan, UnaryNode}
import org.apache.spark.sql.execution.metric.SQLMetrics

case class SortBasedAggregate(
    requiredChildDistributionExpressions: Option[Seq[Expression]],
    groupingExpressions: Seq[NamedExpression],
    aggregateExpressions: Seq[AggregateExpression],
    aggregateAttributes: Seq[Attribute],
    initialInputBufferOffset: Int,
    resultExpressions: Seq[NamedExpression],
    child: SparkPlan)
    extends UnaryNode

  private[this] val aggregateBufferAttributes =
    aggregateExpressions.flatMap(_.aggregateFunction.aggBufferAttributes)

  override def producedAttributes: AttributeSet =
    AttributeSet(aggregateAttributes) ++ AttributeSet(resultExpressions
          .diff(groupingExpressions)
          .map(_.toAttribute)) ++ AttributeSet(aggregateBufferAttributes)

  override private[sql] lazy val metrics = Map(
      "numOutputRows" -> SQLMetrics.createLongMetric(sparkContext,
                                                     "number of output rows"))

  override def output: Seq[Attribute] = resultExpressions.map(_.toAttribute)

  override def requiredChildDistribution: List[Distribution] =
    requiredChildDistributionExpressions match
      case Some(exprs) if exprs.length == 0 => AllTuples :: Nil
      case Some(exprs) if exprs.length > 0 =>
        ClusteredDistribution(exprs) :: Nil
      case None => UnspecifiedDistribution :: Nil

  override def requiredChildOrdering: Seq[Seq[SortOrder]] =
    groupingExpressions.map(SortOrder(_, Ascending)) :: Nil

  override def outputOrdering: Seq[SortOrder] =
    groupingExpressions.map(SortOrder(_, Ascending))

  protected override def doExecute(): RDD[InternalRow] =
    attachTree(this, "execute")
      val numOutputRows = longMetric("numOutputRows")
      child.execute().mapPartitionsInternal  iter =>
        // Because the constructor of an aggregation iterator will read at least the first row,
        // we need to get the value of iter.hasNext first.
        val hasInput = iter.hasNext
        if (!hasInput && groupingExpressions.nonEmpty)
          // This is a grouped aggregate and the input iterator is empty,
          // so return an empty iterator.
          Iterator[UnsafeRow]()
        else
          val outputIter = new SortBasedAggregationIterator(
              groupingExpressions,
              child.output,
              iter,
              aggregateExpressions,
              aggregateAttributes,
              initialInputBufferOffset,
              resultExpressions,
              (expressions, inputSchema) =>
                newMutableProjection(
                    expressions, inputSchema, subexpressionEliminationEnabled),
              numOutputRows)
          if (!hasInput && groupingExpressions.isEmpty)
            // There is no input and there is no grouping expressions.
            // We need to output a single row as the output.
            numOutputRows += 1
            Iterator[UnsafeRow](
                outputIter.outputForEmptyGroupingKeyWithoutInput())
          else
            outputIter

  override def simpleString: String =
    val allAggregateExpressions = aggregateExpressions

    val keyString = groupingExpressions.mkString("[", ",", "]")
    val functionString = allAggregateExpressions.mkString("[", ",", "]")
    val outputString = output.mkString("[", ",", "]")
    s"SortBasedAggregate(key=$keyString, functions=$functionString, output=$outputString)"
