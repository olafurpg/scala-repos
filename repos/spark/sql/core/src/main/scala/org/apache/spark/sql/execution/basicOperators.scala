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

package org.apache.spark.sql.execution

import org.apache.spark.rdd.{PartitionwiseSampledRDD, RDD}
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.catalyst.expressions.codegen.{
  CodegenContext, ExprCode, ExpressionCanonicalizer
}
import org.apache.spark.sql.catalyst.plans.physical._
import org.apache.spark.sql.execution.metric.{LongSQLMetricValue, SQLMetrics}
import org.apache.spark.sql.types.LongType
import org.apache.spark.util.random.PoissonSampler

case class Project(projectList: Seq[NamedExpression], child: SparkPlan)
    extends UnaryNode
    with CodegenSupport {

  override def output: Seq[Attribute] = projectList.map(_.toAttribute)

  override def upstreams(): Seq[RDD[InternalRow]] = {
    child.asInstanceOf[CodegenSupport].upstreams()
  }

  protected override def doProduce(ctx: CodegenContext): String = {
    child.asInstanceOf[CodegenSupport].produce(ctx, this)
  }

  override def usedInputs: AttributeSet = {
    // only the attributes those are used at least twice should be evaluated before this plan,
    // otherwise we could defer the evaluation until output attribute is actually used.
    val usedExprIds = projectList.flatMap(_.collect {
      case a: Attribute => a.exprId
    })
    val usedMoreThanOnce =
      usedExprIds.groupBy(id => id).filter(_._2.size > 1).keySet
    references.filter(a => usedMoreThanOnce.contains(a.exprId))
  }

  override def doConsume(ctx: CodegenContext,
                         input: Seq[ExprCode],
                         row: String): String = {
    val exprs = projectList.map(
        x =>
          ExpressionCanonicalizer.execute(
              BindReferences.bindReference(x, child.output)))
    ctx.currentVars = input
    val resultVars = exprs.map(_.gen(ctx))
    // Evaluation of non-deterministic expressions can't be deferred.
    val nonDeterministicAttrs =
      projectList.filterNot(_.deterministic).map(_.toAttribute)
    s"""
       |${evaluateRequiredVariables(output,
                                    resultVars,
                                    AttributeSet(nonDeterministicAttrs))}
       |${consume(ctx, resultVars)}
     """.stripMargin
  }

  protected override def doExecute(): RDD[InternalRow] = {
    child.execute().mapPartitionsInternal { iter =>
      val project = UnsafeProjection
        .create(projectList, child.output, subexpressionEliminationEnabled)
      iter.map(project)
    }
  }

  override def outputOrdering: Seq[SortOrder] = child.outputOrdering
}

case class Filter(condition: Expression, child: SparkPlan)
    extends UnaryNode
    with CodegenSupport
    with PredicateHelper {

  // Split out all the IsNotNulls from condition.
  private val (notNullPreds, otherPreds) =
    splitConjunctivePredicates(condition).partition {
      case IsNotNull(a) if child.output.contains(a) => true
      case _ => false
    }

  // The columns that will filtered out by `IsNotNull` could be considered as not nullable.
  private val notNullAttributes = notNullPreds.flatMap(_.references)

  override def output: Seq[Attribute] = {
    child.output.map { a =>
      if (a.nullable && notNullAttributes.contains(a)) {
        a.withNullability(false)
      } else {
        a
      }
    }
  }

  private[sql] override lazy val metrics = Map(
      "numOutputRows" -> SQLMetrics.createLongMetric(sparkContext,
                                                     "number of output rows"))

  override def upstreams(): Seq[RDD[InternalRow]] = {
    child.asInstanceOf[CodegenSupport].upstreams()
  }

  protected override def doProduce(ctx: CodegenContext): String = {
    child.asInstanceOf[CodegenSupport].produce(ctx, this)
  }

  override def doConsume(ctx: CodegenContext,
                         input: Seq[ExprCode],
                         row: String): String = {
    val numOutput = metricTerm(ctx, "numOutputRows")

    // filter out the nulls
    val filterOutNull = notNullAttributes.map { a =>
      val idx = child.output.indexOf(a)
      s"if (${input(idx).isNull}) continue;"
    }.mkString("\n")

    ctx.currentVars = input
    val predicates = otherPreds.map { e =>
      val bound = ExpressionCanonicalizer.execute(
          BindReferences.bindReference(e, output))
      val ev = bound.gen(ctx)
      val nullCheck = if (bound.nullable) {
        s"${ev.isNull} || "
      } else {
        s""
      }
      s"""
         |${ev.code}
         |if (${nullCheck}!${ev.value}) continue;
       """.stripMargin
    }.mkString("\n")

    // Reset the isNull to false for the not-null columns, then the followed operators could
    // generate better code (remove dead branches).
    val resultVars = input.zipWithIndex.map {
      case (ev, i) =>
        if (notNullAttributes.contains(child.output(i))) {
          ev.isNull = "false"
        }
        ev
    }
    s"""
       |$filterOutNull
       |$predicates
       |$numOutput.add(1);
       |${consume(ctx, resultVars)}
     """.stripMargin
  }

  protected override def doExecute(): RDD[InternalRow] = {
    val numOutputRows = longMetric("numOutputRows")
    child.execute().mapPartitionsInternal { iter =>
      val predicate = newPredicate(condition, child.output)
      iter.filter { row =>
        val r = predicate(row)
        if (r) numOutputRows += 1
        r
      }
    }
  }

  override def outputOrdering: Seq[SortOrder] = child.outputOrdering
}

/**
  * Sample the dataset.
  *
  * @param lowerBound Lower-bound of the sampling probability (usually 0.0)
  * @param upperBound Upper-bound of the sampling probability. The expected fraction sampled
  *                   will be ub - lb.
  * @param withReplacement Whether to sample with replacement.
  * @param seed the random seed
  * @param child the SparkPlan
  */
case class Sample(lowerBound: Double,
                  upperBound: Double,
                  withReplacement: Boolean,
                  seed: Long,
                  child: SparkPlan)
    extends UnaryNode {
  override def output: Seq[Attribute] = child.output

  protected override def doExecute(): RDD[InternalRow] = {
    if (withReplacement) {
      // Disable gap sampling since the gap sampling method buffers two rows internally,
      // requiring us to copy the row, which is more expensive than the random number generator.
      new PartitionwiseSampledRDD[InternalRow, InternalRow](
          child.execute(),
          new PoissonSampler[InternalRow](upperBound - lowerBound,
                                          useGapSamplingIfPossible = false),
          preservesPartitioning = true,
          seed)
    } else {
      child.execute().randomSampleWithRange(lowerBound, upperBound, seed)
    }
  }
}

case class Range(start: Long,
                 step: Long,
                 numSlices: Int,
                 numElements: BigInt,
                 output: Seq[Attribute])
    extends LeafNode
    with CodegenSupport {

  private[sql] override lazy val metrics = Map(
      "numOutputRows" -> SQLMetrics.createLongMetric(sparkContext,
                                                     "number of output rows"))

  // output attributes should not affect the results
  override lazy val cleanArgs: Seq[Any] =
    Seq(start, step, numSlices, numElements)

  override def upstreams(): Seq[RDD[InternalRow]] = {
    sqlContext.sparkContext
      .parallelize(0 until numSlices, numSlices)
      .map(i => InternalRow(i)) :: Nil
  }

  protected override def doProduce(ctx: CodegenContext): String = {
    val numOutput = metricTerm(ctx, "numOutputRows")

    val initTerm = ctx.freshName("initRange")
    ctx.addMutableState("boolean", initTerm, s"$initTerm = false;")
    val partitionEnd = ctx.freshName("partitionEnd")
    ctx.addMutableState("long", partitionEnd, s"$partitionEnd = 0L;")
    val number = ctx.freshName("number")
    ctx.addMutableState("long", number, s"$number = 0L;")
    val overflow = ctx.freshName("overflow")
    ctx.addMutableState("boolean", overflow, s"$overflow = false;")

    val value = ctx.freshName("value")
    val ev = ExprCode("", "false", value)
    val BigInt = classOf[java.math.BigInteger].getName
    val checkEnd = if (step > 0) {
      s"$number < $partitionEnd"
    } else {
      s"$number > $partitionEnd"
    }

    ctx.addNewFunction("initRange",
                       s"""
        | private void initRange(int idx) {
        |   $BigInt index = $BigInt.valueOf(idx);
        |   $BigInt numSlice = $BigInt.valueOf(${numSlices}L);
        |   $BigInt numElement = $BigInt.valueOf(${numElements.toLong}L);
        |   $BigInt step = $BigInt.valueOf(${step}L);
        |   $BigInt start = $BigInt.valueOf(${start}L);
        |
        |   $BigInt st = index.multiply(numElement).divide(numSlice).multiply(step).add(start);
        |   if (st.compareTo($BigInt.valueOf(Long.MAX_VALUE)) > 0) {
        |     $number = Long.MAX_VALUE;
        |   } else if (st.compareTo($BigInt.valueOf(Long.MIN_VALUE)) < 0) {
        |     $number = Long.MIN_VALUE;
        |   } else {
        |     $number = st.longValue();
        |   }
        |
        |   $BigInt end = index.add($BigInt.ONE).multiply(numElement).divide(numSlice)
        |     .multiply(step).add(start);
        |   if (end.compareTo($BigInt.valueOf(Long.MAX_VALUE)) > 0) {
        |     $partitionEnd = Long.MAX_VALUE;
        |   } else if (end.compareTo($BigInt.valueOf(Long.MIN_VALUE)) < 0) {
        |     $partitionEnd = Long.MIN_VALUE;
        |   } else {
        |     $partitionEnd = end.longValue();
        |   }
        |
        |   $numOutput.add(($partitionEnd - $number) / ${step}L);
        | }
       """.stripMargin)

    val input = ctx.freshName("input")
    // Right now, Range is only used when there is one upstream.
    ctx.addMutableState("scala.collection.Iterator",
                        input,
                        s"$input = inputs[0];")
    s"""
      | // initialize Range
      | if (!$initTerm) {
      |   $initTerm = true;
      |   if ($input.hasNext()) {
      |     initRange(((InternalRow) $input.next()).getInt(0));
      |   } else {
      |     return;
      |   }
      | }
      |
      | while (!$overflow && $checkEnd && !shouldStop()) {
      |  long $value = $number;
      |  $number += ${step}L;
      |  if ($number < $value ^ ${step}L < 0) {
      |    $overflow = true;
      |  }
      |  ${consume(ctx, Seq(ev))}
      | }
     """.stripMargin
  }

  protected override def doExecute(): RDD[InternalRow] = {
    val numOutputRows = longMetric("numOutputRows")
    sqlContext.sparkContext
      .parallelize(0 until numSlices, numSlices)
      .mapPartitionsWithIndex((i, _) => {
        val partitionStart = (i * numElements) / numSlices * step + start
        val partitionEnd = (((i + 1) * numElements) / numSlices) * step + start
        def getSafeMargin(bi: BigInt): Long =
          if (bi.isValidLong) {
            bi.toLong
          } else if (bi > 0) {
            Long.MaxValue
          } else {
            Long.MinValue
          }
        val safePartitionStart = getSafeMargin(partitionStart)
        val safePartitionEnd = getSafeMargin(partitionEnd)
        val rowSize =
          UnsafeRow.calculateBitSetWidthInBytes(1) + LongType.defaultSize
        val unsafeRow = UnsafeRow.createFromByteArray(rowSize, 1)

        new Iterator[InternalRow] {
          private[this] var number: Long = safePartitionStart
          private[this] var overflow: Boolean = false

          override def hasNext =
            if (!overflow) {
              if (step > 0) {
                number < safePartitionEnd
              } else {
                number > safePartitionEnd
              }
            } else false

          override def next() = {
            val ret = number
            number += step
            if (number < ret ^ step < 0) {
              // we have Long.MaxValue + Long.MaxValue < Long.MaxValue
              // and Long.MinValue + Long.MinValue > Long.MinValue, so iff the step causes a step
              // back, we are pretty sure that we have an overflow.
              overflow = true
            }

            numOutputRows += 1
            unsafeRow.setLong(0, ret)
            unsafeRow
          }
        }
      })
  }
}

/**
  * Union two plans, without a distinct. This is UNION ALL in SQL.
  */
case class Union(children: Seq[SparkPlan]) extends SparkPlan {
  override def output: Seq[Attribute] =
    children
      .map(_.output)
      .transpose
      .map(attrs => attrs.head.withNullability(attrs.exists(_.nullable)))

  protected override def doExecute(): RDD[InternalRow] =
    sparkContext.union(children.map(_.execute()))
}

/**
  * Return a new RDD that has exactly `numPartitions` partitions.
  * Similar to coalesce defined on an [[RDD]], this operation results in a narrow dependency, e.g.
  * if you go from 1000 partitions to 100 partitions, there will not be a shuffle, instead each of
  * the 100 new partitions will claim 10 of the current partitions.
  */
case class Coalesce(numPartitions: Int, child: SparkPlan) extends UnaryNode {
  override def output: Seq[Attribute] = child.output

  override def outputPartitioning: Partitioning = {
    if (numPartitions == 1) SinglePartition
    else UnknownPartitioning(numPartitions)
  }

  protected override def doExecute(): RDD[InternalRow] = {
    child.execute().coalesce(numPartitions, shuffle = false)
  }
}

/**
  * Returns a table with the elements from left that are not in right using
  * the built-in spark subtract function.
  */
case class Except(left: SparkPlan, right: SparkPlan) extends BinaryNode {
  override def output: Seq[Attribute] = left.output

  protected override def doExecute(): RDD[InternalRow] = {
    left.execute().map(_.copy()).subtract(right.execute().map(_.copy()))
  }
}

/**
  * A plan node that does nothing but lie about the output of its child.  Used to spice a
  * (hopefully structurally equivalent) tree from a different optimization sequence into an already
  * resolved tree.
  */
case class OutputFaker(output: Seq[Attribute], child: SparkPlan)
    extends SparkPlan {
  def children: Seq[SparkPlan] = child :: Nil

  protected override def doExecute(): RDD[InternalRow] = child.execute()
}

/**
  * A plan as subquery.
  *
  * This is used to generate tree string for SparkScalarSubquery.
  */
case class Subquery(name: String, child: SparkPlan) extends UnaryNode {
  override def output: Seq[Attribute] = child.output
  override def outputPartitioning: Partitioning = child.outputPartitioning
  override def outputOrdering: Seq[SortOrder] = child.outputOrdering

  protected override def doExecute(): RDD[InternalRow] = {
    throw new UnsupportedOperationException
  }
}
