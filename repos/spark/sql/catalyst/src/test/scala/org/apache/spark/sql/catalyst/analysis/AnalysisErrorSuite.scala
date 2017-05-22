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

import scala.beans.{BeanInfo, BeanProperty}

import org.apache.spark.sql.AnalysisException
import org.apache.spark.sql.catalyst.dsl.expressions._
import org.apache.spark.sql.catalyst.dsl.plans._
import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.catalyst.expressions.aggregate.{AggregateExpression, Complete, Count}
import org.apache.spark.sql.catalyst.plans.logical._
import org.apache.spark.sql.catalyst.plans.Inner
import org.apache.spark.sql.catalyst.util.{ArrayBasedMapData, GenericArrayData, MapData}
import org.apache.spark.sql.types._

@BeanInfo
private[sql] case class GroupableData(@BeanProperty data: Int)

private[sql] class GroupableUDT extends UserDefinedType[GroupableData]

  override def sqlType: DataType = IntegerType

  override def serialize(groupableData: GroupableData): Int =
    groupableData.data

  override def deserialize(datum: Any): GroupableData =
    datum match
      case data: Int => GroupableData(data)

  override def userClass: Class[GroupableData] = classOf[GroupableData]

  private[spark] override def asNullable: GroupableUDT = this

@BeanInfo
private[sql] case class UngroupableData(@BeanProperty data: Map[Int, Int])

private[sql] class UngroupableUDT extends UserDefinedType[UngroupableData]

  override def sqlType: DataType = MapType(IntegerType, IntegerType)

  override def serialize(ungroupableData: UngroupableData): MapData =
    val keyArray = new GenericArrayData(ungroupableData.data.keys.toSeq)
    val valueArray = new GenericArrayData(ungroupableData.data.values.toSeq)
    new ArrayBasedMapData(keyArray, valueArray)

  override def deserialize(datum: Any): UngroupableData =
    datum match
      case data: MapData =>
        val keyArray = data.keyArray().array
        val valueArray = data.valueArray().array
        assert(keyArray.length == valueArray.length)
        val mapData =
          keyArray.zip(valueArray).toMap.asInstanceOf[Map[Int, Int]]
        UngroupableData(mapData)

  override def userClass: Class[UngroupableData] = classOf[UngroupableData]

  private[spark] override def asNullable: UngroupableUDT = this

case class TestFunction(
    children: Seq[Expression], inputTypes: Seq[AbstractDataType])
    extends Expression with ImplicitCastInputTypes with Unevaluable
  override def nullable: Boolean = true
  override def dataType: DataType = StringType

case class UnresolvedTestPlan() extends LeafNode
  override lazy val resolved = false
  override def output: Seq[Attribute] = Nil

class AnalysisErrorSuite extends AnalysisTest
  import TestRelations._

  def errorTest(name: String,
                plan: LogicalPlan,
                errorMessages: Seq[String],
                caseSensitive: Boolean = true): Unit =
    test(name)
      assertAnalysisError(plan, errorMessages, caseSensitive)

  val dateLit = Literal.create(null, DateType)

  errorTest("scalar subquery with 2 columns",
            testRelation.select(
                (ScalarSubquery(testRelation.select('a, dateLit.as('b))) +
                    Literal(1)).as('a)),
            "Scalar subquery must return only one column, but got 2" :: Nil)

  errorTest("scalar subquery with no column",
            testRelation.select(ScalarSubquery(LocalRelation()).as('a)),
            "Scalar subquery must return only one column, but got 0" :: Nil)

  errorTest(
      "single invalid type, single arg",
      testRelation.select(
          TestFunction(dateLit :: Nil, IntegerType :: Nil).as('a)),
      "cannot resolve" :: "testfunction(CAST(NULL AS DATE))" :: "argument 1" :: "requires int type" :: "'CAST(NULL AS DATE)' is of date type" :: Nil)

  errorTest(
      "single invalid type, second arg",
      testRelation.select(TestFunction(dateLit :: dateLit :: Nil,
                                       DateType :: IntegerType :: Nil).as('a)),
      "cannot resolve" :: "testfunction(CAST(NULL AS DATE), CAST(NULL AS DATE))" :: "argument 2" :: "requires int type" :: "'CAST(NULL AS DATE)' is of date type" :: Nil)

  errorTest(
      "multiple invalid type",
      testRelation.select(
          TestFunction(dateLit :: dateLit :: Nil,
                       IntegerType :: IntegerType :: Nil).as('a)),
      "cannot resolve" :: "testfunction(CAST(NULL AS DATE), CAST(NULL AS DATE))" :: "argument 1" :: "argument 2" :: "requires int type" :: "'CAST(NULL AS DATE)' is of date type" :: Nil)

  errorTest("invalid window function",
            testRelation2.select(
                WindowExpression(
                    Literal(0),
                    WindowSpecDefinition(UnresolvedAttribute("a") :: Nil,
                                         SortOrder(UnresolvedAttribute("b"),
                                                   Ascending) :: Nil,
                                         UnspecifiedFrame)).as('window)),
            "not supported within a window function" :: Nil)

  errorTest("distinct window function",
            testRelation2.select(
                WindowExpression(
                    AggregateExpression(Count(UnresolvedAttribute("b")),
                                        Complete,
                                        isDistinct = true),
                    WindowSpecDefinition(UnresolvedAttribute("a") :: Nil,
                                         SortOrder(UnresolvedAttribute("b"),
                                                   Ascending) :: Nil,
                                         UnspecifiedFrame)).as('window)),
            "Distinct window functions are not supported" :: Nil)

  errorTest("offset window function",
            testRelation2.select(
                WindowExpression(
                    new Lead(UnresolvedAttribute("b")),
                    WindowSpecDefinition(
                        UnresolvedAttribute("a") :: Nil,
                        SortOrder(UnresolvedAttribute("b"), Ascending) :: Nil,
                        SpecifiedWindowFrame(RangeFrame,
                                             ValueFollowing(1),
                                             ValueFollowing(2)))).as('window)),
            "window frame" :: "must match the required frame" :: Nil)

  errorTest("too many generators",
            listRelation.select(Explode('list).as('a), Explode('list).as('b)),
            "only one generator" :: "explode" :: Nil)

  errorTest("unresolved attributes",
            testRelation.select('abcd),
            "cannot resolve" :: "abcd" :: Nil)

  errorTest("unresolved attributes with a generated name",
            testRelation2
              .groupBy('a)(max('b))
              .where(sum('b) > 0)
              .orderBy('havingCondition.asc),
            "cannot resolve" :: "havingCondition" :: Nil)

  errorTest(
      "bad casts",
      testRelation.select(Literal(1).cast(BinaryType).as('badCast)),
      "cannot cast" :: Literal(1).dataType.simpleString :: BinaryType.simpleString :: Nil)

  errorTest("sorting by unsupported column types",
            mapRelation.orderBy('map.asc),
            "sort" :: "type" :: "map<int,int>" :: Nil)

  errorTest(
      "sorting by attributes are not from grouping expressions",
      testRelation2
        .groupBy('a, 'c)('a, 'c, count('a).as("a3"))
        .orderBy('b.asc),
      "cannot resolve" :: "'`b`'" :: "given input columns" :: "[a, c, a3]" :: Nil)

  errorTest(
      "non-boolean filters",
      testRelation.where(Literal(1)),
      "filter" :: "'1'" :: "not a boolean" :: Literal(1).dataType.simpleString :: Nil)

  errorTest(
      "non-boolean join conditions",
      testRelation.join(testRelation, condition = Some(Literal(1))),
      "condition" :: "'1'" :: "not a boolean" :: Literal(1).dataType.simpleString :: Nil)

  errorTest(
      "missing group by",
      testRelation2.groupBy('a)('b),
      "'`b`'" :: "group by" :: Nil
  )

  errorTest("ambiguous field",
            nestedRelation.select($"top.duplicateField"),
            "Ambiguous reference to fields" :: "duplicateField" :: Nil,
            caseSensitive = false)

  errorTest(
      "ambiguous field due to case insensitivity",
      nestedRelation.select($"top.differentCase"),
      "Ambiguous reference to fields" :: "differentCase" :: "differentcase" :: Nil,
      caseSensitive = false)

  errorTest("missing field",
            nestedRelation2.select($"top.c"),
            "No such struct field" :: "aField" :: "bField" :: "cField" :: Nil,
            caseSensitive = false)

  errorTest("catch all unresolved plan",
            UnresolvedTestPlan(),
            "unresolved" :: Nil)

  errorTest(
      "union with unequal number of columns",
      testRelation.unionAll(testRelation2),
      "union" :: "number of columns" :: testRelation2.output.length.toString :: testRelation.output.length.toString :: Nil)

  errorTest(
      "intersect with unequal number of columns",
      testRelation.intersect(testRelation2),
      "intersect" :: "number of columns" :: testRelation2.output.length.toString :: testRelation.output.length.toString :: Nil)

  errorTest(
      "except with unequal number of columns",
      testRelation.except(testRelation2),
      "except" :: "number of columns" :: testRelation2.output.length.toString :: testRelation.output.length.toString :: Nil)

  errorTest("SPARK-9955: correct error message for aggregate",
            // When parse SQL string, we will wrap aggregate expressions with UnresolvedAlias.
            testRelation2
              .where('bad_column > 1)
              .groupBy('a)(UnresolvedAlias(max('b))),
            "cannot resolve '`bad_column`'" :: Nil)

  test("SPARK-6452 regression test")
    // CheckAnalysis should throw AnalysisException when Aggregate contains missing attribute(s)
    // Since we manually construct the logical plan at here and Sum only accept
    // LongType, DoubleType, and DecimalType. We use LongType as the type of a.
    val plan =
      Aggregate(
          Nil,
          Alias(sum(AttributeReference("a", LongType)(exprId = ExprId(1))),
                "b")() :: Nil,
          LocalRelation(AttributeReference("a", LongType)(exprId = ExprId(2))))

    assert(plan.resolved)

    assertAnalysisError(
        plan, "resolved attribute(s) a#1L missing from a#2L" :: Nil)

  test("error test for self-join")
    val join = Join(testRelation, testRelation, Inner, None)
    val error = intercept[AnalysisException]
      SimpleAnalyzer.checkAnalysis(join)
    assert(error.message.contains(
            "Failure when resolving conflicting references in Join"))
    assert(error.message.contains("Conflicting attributes"))

  test("check grouping expression data types")
    def checkDataType(dataType: DataType, shouldSuccess: Boolean): Unit =
      val plan = Aggregate(
          AttributeReference("a", dataType)(exprId = ExprId(2)) :: Nil,
          Alias(sum(AttributeReference("b", IntegerType)(exprId = ExprId(1))),
                "c")() :: Nil,
          LocalRelation(
              AttributeReference("a", dataType)(exprId = ExprId(2)),
              AttributeReference("b", IntegerType)(exprId = ExprId(1))))

      shouldSuccess match
        case true =>
          assertAnalysisSuccess(plan, true)
        case false =>
          assertAnalysisError(
              plan,
              "expression `a` cannot be used as a grouping expression" :: Nil)

    val supportedDataTypes =
      Seq(StringType,
          BinaryType,
          NullType,
          BooleanType,
          ByteType,
          ShortType,
          IntegerType,
          LongType,
          FloatType,
          DoubleType,
          DecimalType(25, 5),
          DecimalType(6, 5),
          DateType,
          TimestampType,
          ArrayType(IntegerType),
          new StructType()
            .add("f1", FloatType, nullable = true)
            .add("f2", StringType, nullable = true),
          new StructType()
            .add("f1", FloatType, nullable = true)
            .add("f2",
                 ArrayType(BooleanType, containsNull = true),
                 nullable = true),
          new GroupableUDT())
    supportedDataTypes.foreach  dataType =>
      checkDataType(dataType, shouldSuccess = true)

    val unsupportedDataTypes =
      Seq(MapType(StringType, LongType),
          new StructType()
            .add("f1", FloatType, nullable = true)
            .add("f2", MapType(StringType, LongType), nullable = true),
          new UngroupableUDT())
    unsupportedDataTypes.foreach  dataType =>
      checkDataType(dataType, shouldSuccess = false)

  test("we should fail analysis when we find nested aggregate functions")
    val plan = Aggregate(
        AttributeReference("a", IntegerType)(exprId = ExprId(2)) :: Nil,
        Alias(
            sum(sum(AttributeReference("b", IntegerType)(exprId = ExprId(1)))),
            "c")() :: Nil,
        LocalRelation(
            AttributeReference("a", IntegerType)(exprId = ExprId(2)),
            AttributeReference("b", IntegerType)(exprId = ExprId(1))))

    assertAnalysisError(
        plan,
        "It is not allowed to use an aggregate function in the argument of " +
        "another aggregate function." :: Nil)

  test("Join can't work on binary and map types")
    val plan = Join(
        LocalRelation(
            AttributeReference("a", BinaryType)(exprId = ExprId(2)),
            AttributeReference("b", IntegerType)(exprId = ExprId(1))),
        LocalRelation(
            AttributeReference("c", BinaryType)(exprId = ExprId(4)),
            AttributeReference("d", IntegerType)(exprId = ExprId(3))),
        Inner,
        Some(EqualTo(AttributeReference("a", BinaryType)(exprId = ExprId(2)),
                     AttributeReference("c", BinaryType)(exprId = ExprId(4)))))

    assertAnalysisError(
        plan,
        "binary type expression `a` cannot be used in join conditions" :: Nil)

    val plan2 = Join(
        LocalRelation(
            AttributeReference("a", MapType(IntegerType, StringType))(
                exprId = ExprId(2)),
            AttributeReference("b", IntegerType)(exprId = ExprId(1))),
        LocalRelation(
            AttributeReference("c", MapType(IntegerType, StringType))(
                exprId = ExprId(4)),
            AttributeReference("d", IntegerType)(exprId = ExprId(3))),
        Inner,
        Some(EqualTo(AttributeReference("a", MapType(IntegerType, StringType))(
                         exprId = ExprId(2)),
                     AttributeReference("c", MapType(IntegerType, StringType))(
                         exprId = ExprId(4)))))

    assertAnalysisError(
        plan2,
        "map type expression `a` cannot be used in join conditions" :: Nil)
