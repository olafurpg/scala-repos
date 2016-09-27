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

package org.apache.spark.sql.execution.joins

import org.apache.spark.TaskContext
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.catalyst.expressions.codegen.{
  CodegenContext,
  ExprCode,
  GenerateUnsafeProjection
}
import org.apache.spark.sql.catalyst.plans._
import org.apache.spark.sql.catalyst.plans.physical.{
  BroadcastDistribution,
  Distribution,
  Partitioning,
  UnspecifiedDistribution
}
import org.apache.spark.sql.execution.{BinaryNode, CodegenSupport, SparkPlan}
import org.apache.spark.sql.execution.metric.SQLMetrics
import org.apache.spark.util.collection.CompactBuffer

/**
  * Performs an inner hash join of two child relations.  When the output RDD of this operator is
  * being constructed, a Spark job is asynchronously started to calculate the values for the
  * broadcasted relation.  This data is then placed in a Spark broadcast variable.  The streamed
  * relation is not shuffled.
  */
case class BroadcastHashJoin(leftKeys: Seq[Expression],
                             rightKeys: Seq[Expression],
                             joinType: JoinType,
                             buildSide: BuildSide,
                             condition: Option[Expression],
                             left: SparkPlan,
                             right: SparkPlan)
    extends BinaryNode
    with HashJoin
    with CodegenSupport {

  override private[sql] lazy val metrics = Map(
    "numOutputRows" -> SQLMetrics.createLongMetric(sparkContext,
                                                   "number of output rows"))

  override def outputPartitioning: Partitioning =
    streamedPlan.outputPartitioning

  override def requiredChildDistribution: Seq[Distribution] = {
    val mode = HashedRelationBroadcastMode(canJoinKeyFitWithinLong,
                                           rewriteKeyExpr(buildKeys),
                                           buildPlan.output)
    buildSide match {
      case BuildLeft =>
        BroadcastDistribution(mode) :: UnspecifiedDistribution :: Nil
      case BuildRight =>
        UnspecifiedDistribution :: BroadcastDistribution(mode) :: Nil
    }
  }

  protected override def doExecute(): RDD[InternalRow] = {
    val numOutputRows = longMetric("numOutputRows")

    val broadcastRelation = buildPlan.executeBroadcast[HashedRelation]()
    streamedPlan.execute().mapPartitions { streamedIter =>
      val joinedRow = new JoinedRow()
      val hashTable = broadcastRelation.value
      TaskContext
        .get()
        .taskMetrics()
        .incPeakExecutionMemory(hashTable.getMemorySize)
      val keyGenerator = streamSideKeyGenerator
      val resultProj = createResultProjection

      joinType match {
        case Inner =>
          hashJoin(streamedIter, hashTable, numOutputRows)

        case LeftOuter =>
          streamedIter.flatMap { currentRow =>
            val rowKey = keyGenerator(currentRow)
            joinedRow.withLeft(currentRow)
            leftOuterIterator(rowKey,
                              joinedRow,
                              hashTable.get(rowKey),
                              resultProj,
                              numOutputRows)
          }

        case RightOuter =>
          streamedIter.flatMap { currentRow =>
            val rowKey = keyGenerator(currentRow)
            joinedRow.withRight(currentRow)
            rightOuterIterator(rowKey,
                               hashTable.get(rowKey),
                               joinedRow,
                               resultProj,
                               numOutputRows)
          }

        case LeftSemi =>
          hashSemiJoin(streamedIter, hashTable, numOutputRows)

        case x =>
          throw new IllegalArgumentException(
            s"BroadcastHashJoin should not take $x as the JoinType")
      }
    }
  }

  override def upstreams(): Seq[RDD[InternalRow]] = {
    streamedPlan.asInstanceOf[CodegenSupport].upstreams()
  }

  override def doProduce(ctx: CodegenContext): String = {
    streamedPlan.asInstanceOf[CodegenSupport].produce(ctx, this)
  }

  override def doConsume(ctx: CodegenContext,
                         input: Seq[ExprCode],
                         row: String): String = {
    joinType match {
      case Inner => codegenInner(ctx, input)
      case LeftOuter | RightOuter => codegenOuter(ctx, input)
      case LeftSemi => codegenSemi(ctx, input)
      case x =>
        throw new IllegalArgumentException(
          s"BroadcastHashJoin should not take $x as the JoinType")
    }
  }

  /**
    * Returns a tuple of Broadcast of HashedRelation and the variable name for it.
    */
  private def prepareBroadcast(
      ctx: CodegenContext): (Broadcast[HashedRelation], String) = {
    // create a name for HashedRelation
    val broadcastRelation = buildPlan.executeBroadcast[HashedRelation]()
    val broadcast = ctx.addReferenceObj("broadcast", broadcastRelation)
    val relationTerm = ctx.freshName("relation")
    val clsName = broadcastRelation.value.getClass.getName
    ctx.addMutableState(clsName,
                        relationTerm,
                        s"""
         | $relationTerm = ($clsName) $broadcast.value();
         | incPeakExecutionMemory($relationTerm.getMemorySize());
       """.stripMargin)
    (broadcastRelation, relationTerm)
  }

  /**
    * Returns the code for generating join key for stream side, and expression of whether the key
    * has any null in it or not.
    */
  private def genStreamSideJoinKey(
      ctx: CodegenContext,
      input: Seq[ExprCode]): (ExprCode, String) = {
    ctx.currentVars = input
    if (canJoinKeyFitWithinLong) {
      // generate the join key as Long
      val expr = rewriteKeyExpr(streamedKeys).head
      val ev = BindReferences.bindReference(expr, streamedPlan.output).gen(ctx)
      (ev, ev.isNull)
    } else {
      // generate the join key as UnsafeRow
      val keyExpr =
        streamedKeys.map(BindReferences.bindReference(_, streamedPlan.output))
      val ev = GenerateUnsafeProjection.createCode(ctx, keyExpr)
      (ev, s"${ev.value}.anyNull()")
    }
  }

  /**
    * Generates the code for variable of build side.
    */
  private def genBuildSideVars(ctx: CodegenContext,
                               matched: String): Seq[ExprCode] = {
    ctx.currentVars = null
    ctx.INPUT_ROW = matched
    buildPlan.output.zipWithIndex.map {
      case (a, i) =>
        val ev = BoundReference(i, a.dataType, a.nullable).gen(ctx)
        if (joinType == Inner) {
          ev
        } else {
          // the variables are needed even there is no matched rows
          val isNull = ctx.freshName("isNull")
          val value = ctx.freshName("value")
          val code =
            s"""
          |boolean $isNull = true;
          |${ctx.javaType(a.dataType)} $value = ${ctx.defaultValue(a.dataType)};
          |if ($matched != null) {
          |  ${ev.code}
          |  $isNull = ${ev.isNull};
          |  $value = ${ev.value};
          |}
         """.stripMargin
          ExprCode(code, isNull, value)
        }
    }
  }

  /**
    * Generates the code for Inner join.
    */
  private def codegenInner(ctx: CodegenContext, input: Seq[ExprCode]): String = {
    val (broadcastRelation, relationTerm) = prepareBroadcast(ctx)
    val (keyEv, anyNull) = genStreamSideJoinKey(ctx, input)
    val matched = ctx.freshName("matched")
    val buildVars = genBuildSideVars(ctx, matched)
    val numOutput = metricTerm(ctx, "numOutputRows")

    val checkCondition =
      if (condition.isDefined) {
        val expr = condition.get
        // evaluate the variables from build side that used by condition
        val eval = evaluateRequiredVariables(buildPlan.output,
                                             buildVars,
                                             expr.references)
        // filter the output via condition
        ctx.currentVars = input ++ buildVars
        val ev = BindReferences
          .bindReference(expr, streamedPlan.output ++ buildPlan.output)
          .gen(ctx)
        s"""
         |$eval
         |${ev.code}
         |if (${ev.isNull} || !${ev.value}) continue;
       """.stripMargin
      } else {
        ""
      }

    val resultVars = buildSide match {
      case BuildLeft => buildVars ++ input
      case BuildRight => input ++ buildVars
    }
    if (broadcastRelation.value.isInstanceOf[UniqueHashedRelation]) {
      s"""
         |// generate join key for stream side
         |${keyEv.code}
         |// find matches from HashedRelation
         |UnsafeRow $matched = $anyNull ? null: (UnsafeRow)$relationTerm.getValue(${keyEv.value});
         |if ($matched == null) continue;
         |$checkCondition
         |$numOutput.add(1);
         |${consume(ctx, resultVars)}
       """.stripMargin
    } else {
      ctx.copyResult = true
      val matches = ctx.freshName("matches")
      val bufferType = classOf[CompactBuffer[UnsafeRow]].getName
      val i = ctx.freshName("i")
      val size = ctx.freshName("size")
      s"""
         |// generate join key for stream side
         |${keyEv.code}
         |// find matches from HashRelation
         |$bufferType $matches = $anyNull ? null : ($bufferType)$relationTerm.get(${keyEv.value});
         |if ($matches == null) continue;
         |int $size = $matches.size();
         |for (int $i = 0; $i < $size; $i++) {
         |  UnsafeRow $matched = (UnsafeRow) $matches.apply($i);
         |  $checkCondition
         |  $numOutput.add(1);
         |  ${consume(ctx, resultVars)}
         |}
       """.stripMargin
    }
  }

  /**
    * Generates the code for left or right outer join.
    */
  private def codegenOuter(ctx: CodegenContext, input: Seq[ExprCode]): String = {
    val (broadcastRelation, relationTerm) = prepareBroadcast(ctx)
    val (keyEv, anyNull) = genStreamSideJoinKey(ctx, input)
    val matched = ctx.freshName("matched")
    val buildVars = genBuildSideVars(ctx, matched)
    val numOutput = metricTerm(ctx, "numOutputRows")

    // filter the output via condition
    val conditionPassed = ctx.freshName("conditionPassed")
    val checkCondition =
      if (condition.isDefined) {
        val expr = condition.get
        // evaluate the variables from build side that used by condition
        val eval = evaluateRequiredVariables(buildPlan.output,
                                             buildVars,
                                             expr.references)
        ctx.currentVars = input ++ buildVars
        val ev = BindReferences
          .bindReference(expr, streamedPlan.output ++ buildPlan.output)
          .gen(ctx)
        s"""
         |boolean $conditionPassed = true;
         |${eval.trim}
         |${ev.code}
         |if ($matched != null) {
         |  $conditionPassed = !${ev.isNull} && ${ev.value};
         |}
       """.stripMargin
      } else {
        s"final boolean $conditionPassed = true;"
      }

    val resultVars = buildSide match {
      case BuildLeft => buildVars ++ input
      case BuildRight => input ++ buildVars
    }
    if (broadcastRelation.value.isInstanceOf[UniqueHashedRelation]) {
      s"""
         |// generate join key for stream side
         |${keyEv.code}
         |// find matches from HashedRelation
         |UnsafeRow $matched = $anyNull ? null: (UnsafeRow)$relationTerm.getValue(${keyEv.value});
         |${checkCondition.trim}
         |if (!$conditionPassed) {
         |  $matched = null;
         |  // reset the variables those are already evaluated.
         |  ${buildVars
           .filter(_.code == "")
           .map(v => s"${v.isNull} = true;")
           .mkString("\n")}
         |}
         |$numOutput.add(1);
         |${consume(ctx, resultVars)}
       """.stripMargin
    } else {
      ctx.copyResult = true
      val matches = ctx.freshName("matches")
      val bufferType = classOf[CompactBuffer[UnsafeRow]].getName
      val i = ctx.freshName("i")
      val size = ctx.freshName("size")
      val found = ctx.freshName("found")
      s"""
         |// generate join key for stream side
         |${keyEv.code}
         |// find matches from HashRelation
         |$bufferType $matches = $anyNull ? null : ($bufferType)$relationTerm.get(${keyEv.value});
         |int $size = $matches != null ? $matches.size() : 0;
         |boolean $found = false;
         |// the last iteration of this loop is to emit an empty row if there is no matched rows.
         |for (int $i = 0; $i <= $size; $i++) {
         |  UnsafeRow $matched = $i < $size ? (UnsafeRow) $matches.apply($i) : null;
         |  ${checkCondition.trim}
         |  if (!$conditionPassed || ($i == $size && $found)) continue;
         |  $found = true;
         |  $numOutput.add(1);
         |  ${consume(ctx, resultVars)}
         |}
       """.stripMargin
    }
  }

  /**
    * Generates the code for left semi join.
    */
  private def codegenSemi(ctx: CodegenContext, input: Seq[ExprCode]): String = {
    val (broadcastRelation, relationTerm) = prepareBroadcast(ctx)
    val (keyEv, anyNull) = genStreamSideJoinKey(ctx, input)
    val matched = ctx.freshName("matched")
    val buildVars = genBuildSideVars(ctx, matched)
    val numOutput = metricTerm(ctx, "numOutputRows")

    val checkCondition =
      if (condition.isDefined) {
        val expr = condition.get
        // evaluate the variables from build side that used by condition
        val eval = evaluateRequiredVariables(buildPlan.output,
                                             buildVars,
                                             expr.references)
        // filter the output via condition
        ctx.currentVars = input ++ buildVars
        val ev = BindReferences
          .bindReference(expr, streamedPlan.output ++ buildPlan.output)
          .gen(ctx)
        s"""
         |$eval
         |${ev.code}
         |if (${ev.isNull} || !${ev.value}) continue;
       """.stripMargin
      } else {
        ""
      }

    if (broadcastRelation.value.isInstanceOf[UniqueHashedRelation]) {
      s"""
         |// generate join key for stream side
         |${keyEv.code}
         |// find matches from HashedRelation
         |UnsafeRow $matched = $anyNull ? null: (UnsafeRow)$relationTerm.getValue(${keyEv.value});
         |if ($matched == null) continue;
         |$checkCondition
         |$numOutput.add(1);
         |${consume(ctx, input)}
       """.stripMargin
    } else {
      val matches = ctx.freshName("matches")
      val bufferType = classOf[CompactBuffer[UnsafeRow]].getName
      val i = ctx.freshName("i")
      val size = ctx.freshName("size")
      val found = ctx.freshName("found")
      s"""
         |// generate join key for stream side
         |${keyEv.code}
         |// find matches from HashRelation
         |$bufferType $matches = $anyNull ? null : ($bufferType)$relationTerm.get(${keyEv.value});
         |if ($matches == null) continue;
         |int $size = $matches.size();
         |boolean $found = false;
         |for (int $i = 0; $i < $size; $i++) {
         |  UnsafeRow $matched = (UnsafeRow) $matches.apply($i);
         |  $checkCondition
         |  $found = true;
         |  break;
         |}
         |if (!$found) continue;
         |$numOutput.add(1);
         |${consume(ctx, input)}
       """.stripMargin
    }
  }
}
