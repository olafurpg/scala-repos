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

package org.apache.spark.sql.catalyst.analysis

import org.apache.spark.sql.AnalysisException
import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.catalyst.expressions.aggregate.AggregateExpression
import org.apache.spark.sql.catalyst.plans.UsingJoin
import org.apache.spark.sql.catalyst.plans.logical._
import org.apache.spark.sql.types._

/**
  * Throws user facing errors when passed invalid queries that fail to analyze.
  */
trait CheckAnalysis {

  /**
    * Override to provide additional checks for correct analysis.
    * These rules will be evaluated after our built-in check rules.
    */
  val extendedCheckRules: Seq[LogicalPlan => Unit] = Nil

  protected def failAnalysis(msg: String): Nothing = {
    throw new AnalysisException(msg)
  }

  protected def containsMultipleGenerators(exprs: Seq[Expression]): Boolean = {
    exprs
      .flatMap(_.collect {
        case e: Generator => e
      })
      .length > 1
  }

  def checkAnalysis(plan: LogicalPlan): Unit = {
    // We transform up and order the rules so as to catch the first possible failure instead
    // of the result of cascading resolution failures.
    plan.foreachUp {
      case p if p.analyzed => // Skip already analyzed sub-plans

      case u: UnresolvedRelation =>
        u.failAnalysis(s"Table not found: ${u.tableIdentifier}")

      case operator: LogicalPlan =>
        operator transformExpressionsUp {
          case a: Attribute if !a.resolved =>
            val from = operator.inputSet.map(_.name).mkString(", ")
            a.failAnalysis(
                s"cannot resolve '${a.sql}' given input columns: [$from]")

          case e: Expression if e.checkInputDataTypes().isFailure =>
            e.checkInputDataTypes() match {
              case TypeCheckResult.TypeCheckFailure(message) =>
                e.failAnalysis(
                    s"cannot resolve '${e.sql}' due to data type mismatch: $message")
            }

          case c: Cast if !c.resolved =>
            failAnalysis(
                s"invalid cast from ${c.child.dataType.simpleString} to ${c.dataType.simpleString}")

          case g: Grouping =>
            failAnalysis(
                s"grouping() can only be used with GroupingSets/Cube/Rollup")
          case g: GroupingID =>
            failAnalysis(
                s"grouping_id() can only be used with GroupingSets/Cube/Rollup")

          case w @ WindowExpression(AggregateExpression(_, _, true), _) =>
            failAnalysis(s"Distinct window functions are not supported: $w")

          case w @ WindowExpression(
              _: OffsetWindowFunction,
              WindowSpecDefinition(_,
                                   order,
                                   SpecifiedWindowFrame(
                                   frame, FrameBoundary(l), FrameBoundary(h))))
              if order.isEmpty || frame != RowFrame || l != h =>
            failAnalysis(
                "An offset window function can only be evaluated in an ordered " +
                s"row-based window frame with a single offset: $w")

          case w @ WindowExpression(e, s) =>
            // Only allow window functions with an aggregate expression or an offset window
            // function.
            e match {
              case _: AggregateExpression | _: OffsetWindowFunction |
                  _: AggregateWindowFunction =>
              case _ =>
                failAnalysis(
                    s"Expression '$e' not supported within a window function.")
            }
            // Make sure the window specification is valid.
            s.validate match {
              case Some(m) =>
                failAnalysis(
                    s"Window specification $s is not valid because $m")
              case None => w
            }
        }

        operator match {
          case f: Filter if f.condition.dataType != BooleanType =>
            failAnalysis(s"filter expression '${f.condition.sql}' " +
                s"of type ${f.condition.dataType.simpleString} is not a boolean.")

          case j @ Join(_, _, UsingJoin(_, cols), _) =>
            val from = operator.inputSet.map(_.name).mkString(", ")
            failAnalysis(s"using columns [${cols.mkString(",")}] " +
                s"can not be resolved given input columns: [$from] ")

          case j @ Join(_, _, _, Some(condition))
              if condition.dataType != BooleanType =>
            failAnalysis(s"join condition '${condition.sql}' " +
                s"of type ${condition.dataType.simpleString} is not a boolean.")

          case j @ Join(_, _, _, Some(condition)) =>
            def checkValidJoinConditionExprs(expr: Expression): Unit =
              expr match {
                case p: Predicate =>
                  p.asInstanceOf[Expression]
                    .children
                    .foreach(checkValidJoinConditionExprs)
                case e if e.dataType.isInstanceOf[BinaryType] =>
                  failAnalysis(
                      s"binary type expression ${e.sql} cannot be used " +
                      "in join conditions")
                case e if e.dataType.isInstanceOf[MapType] =>
                  failAnalysis(
                      s"map type expression ${e.sql} cannot be used " +
                      "in join conditions")
                case _ => // OK
              }

            checkValidJoinConditionExprs(condition)

          case Aggregate(groupingExprs, aggregateExprs, child) =>
            def checkValidAggregateExpression(expr: Expression): Unit =
              expr match {
                case aggExpr: AggregateExpression =>
                  aggExpr.aggregateFunction.children.foreach { child =>
                    child.foreach {
                      case agg: AggregateExpression =>
                        failAnalysis(
                            s"It is not allowed to use an aggregate function in the argument of " +
                            s"another aggregate function. Please use the inner aggregate function " +
                            s"in a sub-query.")
                      case other => // OK
                    }

                    if (!child.deterministic) {
                      failAnalysis(
                          s"nondeterministic expression ${expr.sql} should not " +
                          s"appear in the arguments of an aggregate function.")
                    }
                  }
                case e: Attribute
                    if !groupingExprs.exists(_.semanticEquals(e)) =>
                  failAnalysis(
                      s"expression '${e.sql}' is neither present in the group by, " +
                      s"nor is it an aggregate function. " +
                      "Add to group by or wrap in first() (or first_value) if you don't care " +
                      "which value you get.")
                case e if groupingExprs.exists(_.semanticEquals(e)) => // OK
                case e if e.references.isEmpty => // OK
                case e => e.children.foreach(checkValidAggregateExpression)
              }

            def checkValidGroupingExprs(expr: Expression): Unit = {
              // Check if the data type of expr is orderable.
              if (!RowOrdering.isOrderable(expr.dataType)) {
                failAnalysis(
                    s"expression ${expr.sql} cannot be used as a grouping expression " +
                    s"because its data type ${expr.dataType.simpleString} is not a orderable " +
                    s"data type.")
              }

              if (!expr.deterministic) {
                // This is just a sanity check, our analysis rule PullOutNondeterministic should
                // already pull out those nondeterministic expressions and evaluate them in
                // a Project node.
                failAnalysis(
                    s"nondeterministic expression ${expr.sql} should not " +
                    s"appear in grouping expression.")
              }
            }

            aggregateExprs.foreach(checkValidAggregateExpression)
            groupingExprs.foreach(checkValidGroupingExprs)

          case Sort(orders, _, _) =>
            orders.foreach { order =>
              if (!RowOrdering.isOrderable(order.dataType)) {
                failAnalysis(
                    s"sorting is not supported for columns of type ${order.dataType.simpleString}")
              }
            }

          case s @ SetOperation(left, right)
              if left.output.length != right.output.length =>
            failAnalysis(
                s"${s.nodeName} can only be performed on tables with the same number of columns, " +
                s"but the left table has ${left.output.length} columns and the right has " +
                s"${right.output.length}")

          case s: Union
              if s.children.exists(
                  _.output.length != s.children.head.output.length) =>
            val firstError = s.children
              .find(_.output.length != s.children.head.output.length)
              .get
            failAnalysis(s"""
                |Unions can only be performed on tables with the same number of columns,
                | but one table has '${firstError.output.length}' columns and another table has
                | '${s.children.head.output.length}' columns""".stripMargin)

          case _ => // Fallbacks to the following checks
        }

        operator match {
          case o if o.children.nonEmpty && o.missingInput.nonEmpty =>
            val missingAttributes = o.missingInput.mkString(",")
            val input = o.inputSet.mkString(",")

            failAnalysis(
                s"resolved attribute(s) $missingAttributes missing from $input " +
                s"in operator ${operator.simpleString}")

          case p @ Project(exprs, _) if containsMultipleGenerators(exprs) =>
            failAnalysis(
                s"""Only a single table generating function is allowed in a SELECT clause, found:
                 | ${exprs.map(_.sql).mkString(",")}""".stripMargin)

          case j: Join if !j.duplicateResolved =>
            val conflictingAttributes =
              j.left.outputSet.intersect(j.right.outputSet)
            failAnalysis(s"""
                 |Failure when resolving conflicting references in Join:
                 |$plan
                 |Conflicting attributes: ${conflictingAttributes.mkString(",")}
                 |""".stripMargin)

          case i: Intersect if !i.duplicateResolved =>
            val conflictingAttributes =
              i.left.outputSet.intersect(i.right.outputSet)
            failAnalysis(s"""
                 |Failure when resolving conflicting references in Intersect:
                 |$plan
                 |Conflicting attributes: ${conflictingAttributes.mkString(",")}
                 |""".stripMargin)

          case o if !o.resolved =>
            failAnalysis(s"unresolved operator ${operator.simpleString}")

          case o
              if o.expressions.exists(!_.deterministic) &&
              !o.isInstanceOf[Project] && !o.isInstanceOf[Filter] &&
              !o.isInstanceOf[Aggregate] && !o.isInstanceOf[Window] =>
            // The rule above is used to check Aggregate operator.
            failAnalysis(s"""nondeterministic expressions are only allowed in
                 |Project, Filter, Aggregate or Window, found:
                 | ${o.expressions.map(_.sql).mkString(",")}
                 |in operator ${operator.simpleString}
               """.stripMargin)

          case _ => // Analysis successful!
        }
    }
    extendedCheckRules.foreach(_ (plan))

    plan.foreach(_.setAnalyzed())
  }
}
