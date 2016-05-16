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

package org.apache.spark.sql.catalyst.expressions

import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.analysis.TypeCheckResult
import org.apache.spark.sql.catalyst.expressions.codegen._
import org.apache.spark.sql.catalyst.util.TypeUtils
import org.apache.spark.sql.types._

case class If(
    predicate: Expression, trueValue: Expression, falseValue: Expression)
    extends Expression {

  override def children: Seq[Expression] =
    predicate :: trueValue :: falseValue :: Nil
  override def nullable: Boolean = trueValue.nullable || falseValue.nullable

  override def checkInputDataTypes(): TypeCheckResult = {
    if (predicate.dataType != BooleanType) {
      TypeCheckResult.TypeCheckFailure(
          s"type of predicate expression in If should be boolean, not ${predicate.dataType}")
    } else if (trueValue.dataType.asNullable != falseValue.dataType.asNullable) {
      TypeCheckResult.TypeCheckFailure(s"differing types in '$sql' " +
          s"(${trueValue.dataType.simpleString} and ${falseValue.dataType.simpleString}).")
    } else {
      TypeCheckResult.TypeCheckSuccess
    }
  }

  override def dataType: DataType = trueValue.dataType

  override def eval(input: InternalRow): Any = {
    if (java.lang.Boolean.TRUE.equals(predicate.eval(input))) {
      trueValue.eval(input)
    } else {
      falseValue.eval(input)
    }
  }

  override def genCode(ctx: CodegenContext, ev: ExprCode): String = {
    val condEval = predicate.gen(ctx)
    val trueEval = trueValue.gen(ctx)
    val falseEval = falseValue.gen(ctx)

    s"""
      ${condEval.code}
      boolean ${ev.isNull} = false;
      ${ctx.javaType(dataType)} ${ev.value} = ${ctx.defaultValue(dataType)};
      if (!${condEval.isNull} && ${condEval.value}) {
        ${trueEval.code}
        ${ev.isNull} = ${trueEval.isNull};
        ${ev.value} = ${trueEval.value};
      } else {
        ${falseEval.code}
        ${ev.isNull} = ${falseEval.isNull};
        ${ev.value} = ${falseEval.value};
      }
    """
  }

  override def toString: String =
    s"if ($predicate) $trueValue else $falseValue"

  override def sql: String =
    s"(IF(${predicate.sql}, ${trueValue.sql}, ${falseValue.sql}))"
}

/**
  * Case statements of the form "CASE WHEN a THEN b [WHEN c THEN d]* [ELSE e] END".
  * When a = true, returns b; when c = true, returns d; else returns e.
  *
  * @param branches seq of (branch condition, branch value)
  * @param elseValue optional value for the else branch
  */
case class CaseWhen(branches: Seq[(Expression, Expression)],
                    elseValue: Option[Expression] = None)
    extends Expression
    with CodegenFallback {

  override def children: Seq[Expression] =
    branches.flatMap(b => b._1 :: b._2 :: Nil) ++ elseValue

  // both then and else expressions should be considered.
  def valueTypes: Seq[DataType] =
    branches.map(_._2.dataType) ++ elseValue.map(_.dataType)

  def valueTypesEqual: Boolean =
    valueTypes.size <= 1 || valueTypes.sliding(2, 1).forall {
      case Seq(dt1, dt2) => dt1.sameType(dt2)
    }

  override def dataType: DataType = branches.head._2.dataType

  override def nullable: Boolean = {
    // Result is nullable if any of the branch is nullable, or if the else value is nullable
    branches.exists(_._2.nullable) || elseValue.map(_.nullable).getOrElse(true)
  }

  override def checkInputDataTypes(): TypeCheckResult = {
    // Make sure all branch conditions are boolean types.
    if (valueTypesEqual) {
      if (branches.forall(_._1.dataType == BooleanType)) {
        TypeCheckResult.TypeCheckSuccess
      } else {
        val index = branches.indexWhere(_._1.dataType != BooleanType)
        TypeCheckResult.TypeCheckFailure(
            s"WHEN expressions in CaseWhen should all be boolean type, " +
            s"but the ${index + 1}th when expression's type is ${branches(index)._1}")
      }
    } else {
      TypeCheckResult.TypeCheckFailure(
          "THEN and ELSE expressions should all be same type or coercible to a common type")
    }
  }

  override def eval(input: InternalRow): Any = {
    var i = 0
    while (i < branches.size) {
      if (java.lang.Boolean.TRUE.equals(branches(i)._1.eval(input))) {
        return branches(i)._2.eval(input)
      }
      i += 1
    }
    if (elseValue.isDefined) {
      return elseValue.get.eval(input)
    } else {
      return null
    }
  }

  def shouldCodegen: Boolean = {
    branches.length < CaseWhen.MAX_NUM_CASES_FOR_CODEGEN
  }

  override def genCode(ctx: CodegenContext, ev: ExprCode): String = {
    if (!shouldCodegen) {
      // Fallback to interpreted mode if there are too many branches, as it may reach the
      // 64K limit (limit on bytecode size for a single function).
      return super [CodegenFallback].genCode(ctx, ev)
    }
    // Generate code that looks like:
    //
    // condA = ...
    // if (condA) {
    //   valueA
    // } else {
    //   condB = ...
    //   if (condB) {
    //     valueB
    //   } else {
    //     condC = ...
    //     if (condC) {
    //       valueC
    //     } else {
    //       elseValue
    //     }
    //   }
    // }
    val cases = branches.map {
      case (condExpr, valueExpr) =>
        val cond = condExpr.gen(ctx)
        val res = valueExpr.gen(ctx)
        s"""
        ${cond.code}
        if (!${cond.isNull} && ${cond.value}) {
          ${res.code}
          ${ev.isNull} = ${res.isNull};
          ${ev.value} = ${res.value};
        }
      """
    }

    var generatedCode = cases.mkString("", "\nelse {\n", "\nelse {\n")

    elseValue.foreach { elseExpr =>
      val res = elseExpr.gen(ctx)
      generatedCode += s"""
          ${res.code}
          ${ev.isNull} = ${res.isNull};
          ${ev.value} = ${res.value};
        """
    }

    generatedCode += "}\n" * cases.size

    s"""
      boolean ${ev.isNull} = true;
      ${ctx.javaType(dataType)} ${ev.value} = ${ctx.defaultValue(dataType)};
      $generatedCode
    """
  }

  override def toString: String = {
    val cases = branches.map { case (c, v) => s" WHEN $c THEN $v" }.mkString
    val elseCase = elseValue.map(" ELSE " + _).getOrElse("")
    "CASE" + cases + elseCase + " END"
  }

  override def sql: String = {
    val cases = branches.map { case (c, v) => s" WHEN ${c.sql} THEN ${v.sql}" }.mkString
    val elseCase = elseValue.map(" ELSE " + _.sql).getOrElse("")
    "CASE" + cases + elseCase + " END"
  }
}

/** Factory methods for CaseWhen. */
object CaseWhen {

  // The maximum number of switches supported with codegen.
  val MAX_NUM_CASES_FOR_CODEGEN = 20

  def apply(branches: Seq[(Expression, Expression)],
            elseValue: Expression): CaseWhen = {
    CaseWhen(branches, Option(elseValue))
  }

  /**
    * A factory method to faciliate the creation of this expression when used in parsers.
    * @param branches Expressions at even position are the branch conditions, and expressions at odd
    *                 position are branch values.
    */
  def createFromParser(branches: Seq[Expression]): CaseWhen = {
    val cases = branches
      .grouped(2)
      .flatMap {
        case cond :: value :: Nil => Some((cond, value))
        case value :: Nil => None
      }
      .toArray
      .toSeq // force materialization to make the seq serializable
    val elseValue = if (branches.size % 2 == 1) Some(branches.last) else None
    CaseWhen(cases, elseValue)
  }
}

/**
  * Case statements of the form "CASE a WHEN b THEN c [WHEN d THEN e]* [ELSE f] END".
  * When a = b, returns c; when a = d, returns e; else returns f.
  */
object CaseKeyWhen {
  def apply(key: Expression, branches: Seq[Expression]): CaseWhen = {
    val cases = branches
      .grouped(2)
      .flatMap {
        case cond :: value :: Nil => Some((EqualTo(key, cond), value))
        case value :: Nil => None
      }
      .toArray
      .toSeq // force materialization to make the seq serializable
    val elseValue = if (branches.size % 2 == 1) Some(branches.last) else None
    CaseWhen(cases, elseValue)
  }
}

/**
  * A function that returns the least value of all parameters, skipping null values.
  * It takes at least 2 parameters, and returns null iff all parameters are null.
  */
case class Least(children: Seq[Expression]) extends Expression {

  override def nullable: Boolean = children.forall(_.nullable)
  override def foldable: Boolean = children.forall(_.foldable)

  private lazy val ordering = TypeUtils.getInterpretedOrdering(dataType)

  override def checkInputDataTypes(): TypeCheckResult = {
    if (children.length <= 1) {
      TypeCheckResult.TypeCheckFailure(s"LEAST requires at least 2 arguments")
    } else if (children.map(_.dataType).distinct.count(_ != NullType) > 1) {
      TypeCheckResult.TypeCheckFailure(
          s"The expressions should all have the same type," +
          s" got LEAST (${children.map(_.dataType)}).")
    } else {
      TypeUtils.checkForOrderingExpr(dataType, "function " + prettyName)
    }
  }

  override def dataType: DataType = children.head.dataType

  override def eval(input: InternalRow): Any = {
    children.foldLeft[Any](null)(
        (r, c) => {
      val evalc = c.eval(input)
      if (evalc != null) {
        if (r == null || ordering.lt(evalc, r)) evalc else r
      } else {
        r
      }
    })
  }

  override def genCode(ctx: CodegenContext, ev: ExprCode): String = {
    val evalChildren = children.map(_.gen(ctx))
    val first = evalChildren(0)
    val rest = evalChildren.drop(1)
    def updateEval(eval: ExprCode): String = {
      s"""
        ${eval.code}
        if (!${eval.isNull} && (${ev.isNull} ||
          ${ctx.genGreater(dataType, ev.value, eval.value)})) {
          ${ev.isNull} = false;
          ${ev.value} = ${eval.value};
        }
      """
    }
    s"""
      ${first.code}
      boolean ${ev.isNull} = ${first.isNull};
      ${ctx.javaType(dataType)} ${ev.value} = ${first.value};
      ${rest.map(updateEval).mkString("\n")}
    """
  }
}

/**
  * A function that returns the greatest value of all parameters, skipping null values.
  * It takes at least 2 parameters, and returns null iff all parameters are null.
  */
case class Greatest(children: Seq[Expression]) extends Expression {

  override def nullable: Boolean = children.forall(_.nullable)
  override def foldable: Boolean = children.forall(_.foldable)

  private lazy val ordering = TypeUtils.getInterpretedOrdering(dataType)

  override def checkInputDataTypes(): TypeCheckResult = {
    if (children.length <= 1) {
      TypeCheckResult.TypeCheckFailure(
          s"GREATEST requires at least 2 arguments")
    } else if (children.map(_.dataType).distinct.count(_ != NullType) > 1) {
      TypeCheckResult.TypeCheckFailure(
          s"The expressions should all have the same type," +
          s" got GREATEST (${children.map(_.dataType)}).")
    } else {
      TypeUtils.checkForOrderingExpr(dataType, "function " + prettyName)
    }
  }

  override def dataType: DataType = children.head.dataType

  override def eval(input: InternalRow): Any = {
    children.foldLeft[Any](null)(
        (r, c) => {
      val evalc = c.eval(input)
      if (evalc != null) {
        if (r == null || ordering.gt(evalc, r)) evalc else r
      } else {
        r
      }
    })
  }

  override def genCode(ctx: CodegenContext, ev: ExprCode): String = {
    val evalChildren = children.map(_.gen(ctx))
    val first = evalChildren(0)
    val rest = evalChildren.drop(1)
    def updateEval(eval: ExprCode): String = {
      s"""
        ${eval.code}
        if (!${eval.isNull} && (${ev.isNull} ||
          ${ctx.genGreater(dataType, eval.value, ev.value)})) {
          ${ev.isNull} = false;
          ${ev.value} = ${eval.value};
        }
      """
    }
    s"""
      ${first.code}
      boolean ${ev.isNull} = ${first.isNull};
      ${ctx.javaType(dataType)} ${ev.value} = ${first.value};
      ${rest.map(updateEval).mkString("\n")}
    """
  }
}
