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

import java.util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import org.apache.spark.{SparkEnv, TaskContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.expressions._
import org.apache.spark.sql.catalyst.expressions.aggregate._
import org.apache.spark.sql.catalyst.plans.physical._
import org.apache.spark.sql.types.IntegerType
import org.apache.spark.util.collection.unsafe.sort.{
  UnsafeExternalSorter,
  UnsafeSorterIterator
}

/**
  * This class calculates and outputs (windowed) aggregates over the rows in a single (sorted)
  * partition. The aggregates are calculated for each row in the group. Special processing
  * instructions, frames, are used to calculate these aggregates. Frames are processed in the order
  * specified in the window specification (the ORDER BY ... clause). There are four different frame
  * types:
  * - Entire partition: The frame is the entire partition, i.e.
  *   UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING. For this case, window function will take all
  *   rows as inputs and be evaluated once.
  * - Growing frame: We only add new rows into the frame, i.e. UNBOUNDED PRECEDING AND ....
  *   Every time we move to a new row to process, we add some rows to the frame. We do not remove
  *   rows from this frame.
  * - Shrinking frame: We only remove rows from the frame, i.e. ... AND UNBOUNDED FOLLOWING.
  *   Every time we move to a new row to process, we remove some rows from the frame. We do not add
  *   rows to this frame.
  * - Moving frame: Every time we move to a new row to process, we remove some rows from the frame
  *   and we add some rows to the frame. Examples are:
  *     1 PRECEDING AND CURRENT ROW and 1 FOLLOWING AND 2 FOLLOWING.
  * - Offset frame: The frame consist of one row, which is an offset number of rows away from the
  *   current row. Only [[OffsetWindowFunction]]s can be processed in an offset frame.
  *
  * Different frame boundaries can be used in Growing, Shrinking and Moving frames. A frame
  * boundary can be either Row or Range based:
  * - Row Based: A row based boundary is based on the position of the row within the partition.
  *   An offset indicates the number of rows above or below the current row, the frame for the
  *   current row starts or ends. For instance, given a row based sliding frame with a lower bound
  *   offset of -1 and a upper bound offset of +2. The frame for row with index 5 would range from
  *   index 4 to index 6.
  * - Range based: A range based boundary is based on the actual value of the ORDER BY
  *   expression(s). An offset is used to alter the value of the ORDER BY expression, for
  *   instance if the current order by expression has a value of 10 and the lower bound offset
  *   is -3, the resulting lower bound for the current row will be 10 - 3 = 7. This however puts a
  *   number of constraints on the ORDER BY expressions: there can be only one expression and this
  *   expression must have a numerical data type. An exception can be made when the offset is 0,
  *   because no value modification is needed, in this case multiple and non-numeric ORDER BY
  *   expression are allowed.
  *
  * This is quite an expensive operator because every row for a single group must be in the same
  * partition and partitions must be sorted according to the grouping and sort order. The operator
  * requires the planner to take care of the partitioning and sorting.
  *
  * The operator is semi-blocking. The window functions and aggregates are calculated one group at
  * a time, the result will only be made available after the processing for the entire group has
  * finished. The operator is able to process different frame configurations at the same time. This
  * is done by delegating the actual frame processing (i.e. calculation of the window functions) to
  * specialized classes, see [[WindowFunctionFrame]], which take care of their own frame type:
  * Entire Partition, Sliding, Growing & Shrinking. Boundary evaluation is also delegated to a pair
  * of specialized classes: [[RowBoundOrdering]] & [[RangeBoundOrdering]].
  */
case class Window(windowExpression: Seq[NamedExpression],
                  partitionSpec: Seq[Expression],
                  orderSpec: Seq[SortOrder],
                  child: SparkPlan)
    extends UnaryNode {

  override def output: Seq[Attribute] =
    child.output ++ windowExpression.map(_.toAttribute)

  override def requiredChildDistribution: Seq[Distribution] = {
    if (partitionSpec.isEmpty) {
      // Only show warning when the number of bytes is larger than 100 MB?
      logWarning(
        "No Partition Defined for Window operation! Moving all data to a single " +
          "partition, this can cause serious performance degradation.")
      AllTuples :: Nil
    } else ClusteredDistribution(partitionSpec) :: Nil
  }

  override def requiredChildOrdering: Seq[Seq[SortOrder]] =
    Seq(partitionSpec.map(SortOrder(_, Ascending)) ++ orderSpec)

  override def outputOrdering: Seq[SortOrder] = child.outputOrdering

  /**
    * Create a bound ordering object for a given frame type and offset. A bound ordering object is
    * used to determine which input row lies within the frame boundaries of an output row.
    *
    * This method uses Code Generation. It can only be used on the executor side.
    *
    * @param frameType to evaluate. This can either be Row or Range based.
    * @param offset with respect to the row.
    * @return a bound ordering object.
    */
  private[this] def createBoundOrdering(frameType: FrameType,
                                        offset: Int): BoundOrdering = {
    frameType match {
      case RangeFrame =>
        val (exprs, current, bound) =
          if (offset == 0) {
            // Use the entire order expression when the offset is 0.
            val exprs = orderSpec.map(_.child)
            val projection = newMutableProjection(exprs, child.output)
            (orderSpec, projection(), projection())
          } else if (orderSpec.size == 1) {
            // Use only the first order expression when the offset is non-null.
            val sortExpr = orderSpec.head
            val expr = sortExpr.child
            // Create the projection which returns the current 'value'.
            val current = newMutableProjection(expr :: Nil, child.output)()
            // Flip the sign of the offset when processing the order is descending
            val boundOffset = sortExpr.direction match {
              case Descending => -offset
              case Ascending => offset
            }
            // Create the projection which returns the current 'value' modified by adding the offset.
            val boundExpr = Add(
              expr,
              Cast(Literal.create(boundOffset, IntegerType), expr.dataType))
            val bound = newMutableProjection(boundExpr :: Nil, child.output)()
            (sortExpr :: Nil, current, bound)
          } else {
            sys.error(
              "Non-Zero range offsets are not supported for windows " +
                "with multiple order expressions.")
          }
        // Construct the ordering. This is used to compare the result of current value projection
        // to the result of bound value projection. This is done manually because we want to use
        // Code Generation (if it is enabled).
        val sortExprs = exprs.zipWithIndex.map {
          case (e, i) =>
            SortOrder(BoundReference(i, e.dataType, e.nullable), e.direction)
        }
        val ordering = newOrdering(sortExprs, Nil)
        RangeBoundOrdering(ordering, current, bound)
      case RowFrame => RowBoundOrdering(offset)
    }
  }

  /**
    * Collection containing an entry for each window frame to process. Each entry contains a frames'
    * WindowExpressions and factory function for the WindowFrameFunction.
    */
  private[this] lazy val windowFrameExpressionFactoryPairs = {
    type FrameKey = (String, FrameType, Option[Int], Option[Int])
    type ExpressionBuffer = mutable.Buffer[Expression]
    val framedFunctions =
      mutable.Map.empty[FrameKey, (ExpressionBuffer, ExpressionBuffer)]

    // Add a function and its function to the map for a given frame.
    def collect(tpe: String,
                fr: SpecifiedWindowFrame,
                e: Expression,
                fn: Expression): Unit = {
      val key = (tpe,
                 fr.frameType,
                 FrameBoundary(fr.frameStart),
                 FrameBoundary(fr.frameEnd))
      val (es, fns) = framedFunctions.getOrElseUpdate(
        key,
        (ArrayBuffer.empty[Expression], ArrayBuffer.empty[Expression]))
      es.append(e)
      fns.append(fn)
    }

    // Collect all valid window functions and group them by their frame.
    windowExpression.foreach { x =>
      x.foreach {
        case e @ WindowExpression(function, spec) =>
          val frame =
            spec.frameSpecification.asInstanceOf[SpecifiedWindowFrame]
          function match {
            case AggregateExpression(f, _, _) =>
              collect("AGGREGATE", frame, e, f)
            case f: AggregateWindowFunction =>
              collect("AGGREGATE", frame, e, f)
            case f: OffsetWindowFunction => collect("OFFSET", frame, e, f)
            case f => sys.error(s"Unsupported window function: $f")
          }
        case _ =>
      }
    }

    // Map the groups to a (unbound) expression and frame factory pair.
    var numExpressions = 0
    framedFunctions.toSeq.map {
      case (key, (expressions, functionSeq)) =>
        val ordinal = numExpressions
        val functions = functionSeq.toArray

        // Construct an aggregate processor if we need one.
        def processor =
          AggregateProcessor(
            functions,
            ordinal,
            child.output,
            (expressions, schema) =>
              newMutableProjection(expressions,
                                   schema,
                                   subexpressionEliminationEnabled))

        // Create the factory
        val factory = key match {
          // Offset Frame
          case ("OFFSET", RowFrame, Some(offset), Some(h)) if offset == h =>
            target: MutableRow =>
              new OffsetWindowFunctionFrame(
                target,
                ordinal,
                functions,
                child.output,
                (expressions, schema) =>
                  newMutableProjection(expressions,
                                       schema,
                                       subexpressionEliminationEnabled),
                offset)

          // Growing Frame.
          case ("AGGREGATE", frameType, None, Some(high)) =>
            target: MutableRow =>
              {
                new UnboundedPrecedingWindowFunctionFrame(
                  target,
                  processor,
                  createBoundOrdering(frameType, high))
              }

          // Shrinking Frame.
          case ("AGGREGATE", frameType, Some(low), None) =>
            target: MutableRow =>
              {
                new UnboundedFollowingWindowFunctionFrame(
                  target,
                  processor,
                  createBoundOrdering(frameType, low))
              }

          // Moving Frame.
          case ("AGGREGATE", frameType, Some(low), Some(high)) =>
            target: MutableRow =>
              {
                new SlidingWindowFunctionFrame(
                  target,
                  processor,
                  createBoundOrdering(frameType, low),
                  createBoundOrdering(frameType, high))
              }

          // Entire Partition Frame.
          case ("AGGREGATE", frameType, None, None) =>
            target: MutableRow =>
              {
                new UnboundedWindowFunctionFrame(target, processor)
              }
        }

        // Keep track of the number of expressions. This is a side-effect in a map...
        numExpressions += expressions.size

        // Create the Frame Expression - Factory pair.
        (expressions, factory)
    }
  }

  /**
    * Create the resulting projection.
    *
    * This method uses Code Generation. It can only be used on the executor side.
    *
    * @param expressions unbound ordered function expressions.
    * @return the final resulting projection.
    */
  private[this] def createResultProjection(
      expressions: Seq[Expression]): UnsafeProjection = {
    val references = expressions.zipWithIndex.map {
      case (e, i) =>
        // Results of window expressions will be on the right side of child's output
        BoundReference(child.output.size + i, e.dataType, e.nullable)
    }
    val unboundToRefMap = expressions.zip(references).toMap
    val patchedWindowExpression =
      windowExpression.map(_.transform(unboundToRefMap))
    UnsafeProjection
      .create(child.output ++ patchedWindowExpression, child.output)
  }

  protected override def doExecute(): RDD[InternalRow] = {
    // Unwrap the expressions and factories from the map.
    val expressions = windowFrameExpressionFactoryPairs.flatMap(_._1)
    val factories = windowFrameExpressionFactoryPairs.map(_._2).toArray

    // Start processing.
    child.execute().mapPartitions { stream =>
      new Iterator[InternalRow] {

        // Get all relevant projections.
        val result = createResultProjection(expressions)
        val grouping = UnsafeProjection.create(partitionSpec, child.output)

        // Manage the stream and the grouping.
        var nextRow: UnsafeRow = null
        var nextGroup: UnsafeRow = null
        var nextRowAvailable: Boolean = false
        private[this] def fetchNextRow() {
          nextRowAvailable = stream.hasNext
          if (nextRowAvailable) {
            nextRow = stream.next().asInstanceOf[UnsafeRow]
            nextGroup = grouping(nextRow)
          } else {
            nextRow = null
            nextGroup = null
          }
        }
        fetchNextRow()

        // Manage the current partition.
        val rows = ArrayBuffer.empty[UnsafeRow]
        val inputFields = child.output.length
        var sorter: UnsafeExternalSorter = null
        var rowBuffer: RowBuffer = null
        val windowFunctionResult =
          new SpecificMutableRow(expressions.map(_.dataType))
        val frames = factories.map(_(windowFunctionResult))
        val numFrames = frames.length
        private[this] def fetchNextPartition() {
          // Collect all the rows in the current partition.
          // Before we start to fetch new input rows, make a copy of nextGroup.
          val currentGroup = nextGroup.copy()

          // clear last partition
          if (sorter != null) {
            // the last sorter of this task will be cleaned up via task completion listener
            sorter.cleanupResources()
            sorter = null
          } else {
            rows.clear()
          }

          while (nextRowAvailable && nextGroup == currentGroup) {
            if (sorter == null) {
              rows += nextRow.copy()

              if (rows.length >= 4096) {
                // We will not sort the rows, so prefixComparator and recordComparator are null.
                sorter = UnsafeExternalSorter.create(
                  TaskContext.get().taskMemoryManager(),
                  SparkEnv.get.blockManager,
                  TaskContext.get(),
                  null,
                  null,
                  1024,
                  SparkEnv.get.memoryManager.pageSizeBytes)
                rows.foreach { r =>
                  sorter.insertRecord(r.getBaseObject,
                                      r.getBaseOffset,
                                      r.getSizeInBytes,
                                      0)
                }
                rows.clear()
              }
            } else {
              sorter.insertRecord(nextRow.getBaseObject,
                                  nextRow.getBaseOffset,
                                  nextRow.getSizeInBytes,
                                  0)
            }
            fetchNextRow()
          }
          if (sorter != null) {
            rowBuffer = new ExternalRowBuffer(sorter, inputFields)
          } else {
            rowBuffer = new ArrayRowBuffer(rows)
          }

          // Setup the frames.
          var i = 0
          while (i < numFrames) {
            frames(i).prepare(rowBuffer.copy())
            i += 1
          }

          // Setup iteration
          rowIndex = 0
          rowsSize = rowBuffer.size()
        }

        // Iteration
        var rowIndex = 0
        var rowsSize = 0L

        override final def hasNext: Boolean =
          rowIndex < rowsSize || nextRowAvailable

        val join = new JoinedRow
        override final def next(): InternalRow = {
          // Load the next partition if we need to.
          if (rowIndex >= rowsSize && nextRowAvailable) {
            fetchNextPartition()
          }

          if (rowIndex < rowsSize) {
            // Get the results for the window frames.
            var i = 0
            val current = rowBuffer.next()
            while (i < numFrames) {
              frames(i).write(rowIndex, current)
              i += 1
            }

            // 'Merge' the input row with the window function result
            join(current, windowFunctionResult)
            rowIndex += 1

            // Return the projection.
            result(join)
          } else throw new NoSuchElementException
        }
      }
    }
  }
}

/**
  * Function for comparing boundary values.
  */
private[execution] abstract class BoundOrdering {
  def compare(inputRow: InternalRow,
              inputIndex: Int,
              outputRow: InternalRow,
              outputIndex: Int): Int
}

/**
  * Compare the input index to the bound of the output index.
  */
private[execution] final case class RowBoundOrdering(offset: Int)
    extends BoundOrdering {
  override def compare(inputRow: InternalRow,
                       inputIndex: Int,
                       outputRow: InternalRow,
                       outputIndex: Int): Int =
    inputIndex - (outputIndex + offset)
}

/**
  * Compare the value of the input index to the value bound of the output index.
  */
private[execution] final case class RangeBoundOrdering(
    ordering: Ordering[InternalRow],
    current: Projection,
    bound: Projection)
    extends BoundOrdering {
  override def compare(inputRow: InternalRow,
                       inputIndex: Int,
                       outputRow: InternalRow,
                       outputIndex: Int): Int =
    ordering.compare(current(inputRow), bound(outputRow))
}

/**
  * The interface of row buffer for a partition
  */
private[execution] abstract class RowBuffer {

  /** Number of rows. */
  def size(): Int

  /** Return next row in the buffer, null if no more left. */
  def next(): InternalRow

  /** Skip the next `n` rows. */
  def skip(n: Int): Unit

  /** Return a new RowBuffer that has the same rows. */
  def copy(): RowBuffer
}

/**
  * A row buffer based on ArrayBuffer (the number of rows is limited)
  */
private[execution] class ArrayRowBuffer(buffer: ArrayBuffer[UnsafeRow])
    extends RowBuffer {

  private[this] var cursor: Int = -1

  /** Number of rows. */
  def size(): Int = buffer.length

  /** Return next row in the buffer, null if no more left. */
  def next(): InternalRow = {
    cursor += 1
    if (cursor < buffer.length) {
      buffer(cursor)
    } else {
      null
    }
  }

  /** Skip the next `n` rows. */
  def skip(n: Int): Unit = {
    cursor += n
  }

  /** Return a new RowBuffer that has the same rows. */
  def copy(): RowBuffer = {
    new ArrayRowBuffer(buffer)
  }
}

/**
  * An external buffer of rows based on UnsafeExternalSorter
  */
private[execution] class ExternalRowBuffer(sorter: UnsafeExternalSorter,
                                           numFields: Int)
    extends RowBuffer {

  private[this] val iter: UnsafeSorterIterator = sorter.getIterator

  private[this] val currentRow = new UnsafeRow(numFields)

  /** Number of rows. */
  def size(): Int = iter.getNumRecords()

  /** Return next row in the buffer, null if no more left. */
  def next(): InternalRow = {
    if (iter.hasNext) {
      iter.loadNext()
      currentRow
        .pointTo(iter.getBaseObject, iter.getBaseOffset, iter.getRecordLength)
      currentRow
    } else {
      null
    }
  }

  /** Skip the next `n` rows. */
  def skip(n: Int): Unit = {
    var i = 0
    while (i < n && iter.hasNext) {
      iter.loadNext()
      i += 1
    }
  }

  /** Return a new RowBuffer that has the same rows. */
  def copy(): RowBuffer = {
    new ExternalRowBuffer(sorter, numFields)
  }
}

/**
  * A window function calculates the results of a number of window functions for a window frame.
  * Before use a frame must be prepared by passing it all the rows in the current partition. After
  * preparation the update method can be called to fill the output rows.
  */
private[execution] abstract class WindowFunctionFrame {

  /**
    * Prepare the frame for calculating the results for a partition.
    *
    * @param rows to calculate the frame results for.
    */
  def prepare(rows: RowBuffer): Unit

  /**
    * Write the current results to the target row.
    */
  def write(index: Int, current: InternalRow): Unit
}

/**
  * The offset window frame calculates frames containing LEAD/LAG statements.
  *
  * @param target to write results to.
  * @param expressions to shift a number of rows.
  * @param inputSchema required for creating a projection.
  * @param newMutableProjection function used to create the projection.
  * @param offset by which rows get moved within a partition.
  */
private[execution] final class OffsetWindowFunctionFrame(
    target: MutableRow,
    ordinal: Int,
    expressions: Array[Expression],
    inputSchema: Seq[Attribute],
    newMutableProjection: (Seq[Expression], Seq[Attribute]) => () => MutableProjection,
    offset: Int)
    extends WindowFunctionFrame {

  /** Rows of the partition currently being processed. */
  private[this] var input: RowBuffer = null

  /** Index of the input row currently used for output. */
  private[this] var inputIndex = 0

  /** Row used when there is no valid input. */
  private[this] val emptyRow = new GenericInternalRow(inputSchema.size)

  /** Row used to combine the offset and the current row. */
  private[this] val join = new JoinedRow

  /** Create the projection. */
  private[this] val projection = {
    // Collect the expressions and bind them.
    val inputAttrs = inputSchema.map(_.withNullability(true))
    val numInputAttributes = inputAttrs.size
    val boundExpressions =
      Seq.fill(ordinal)(NoOp) ++ expressions.toSeq.map {
        case e: OffsetWindowFunction =>
          val input = BindReferences.bindReference(e.input, inputAttrs)
          if (e.default == null ||
              e.default.foldable && e.default.eval() == null) {
            // Without default value.
            input
          } else {
            // With default value.
            val default =
              BindReferences.bindReference(e.default, inputAttrs).transform {
                // Shift the input reference to its default version.
                case BoundReference(o, dataType, nullable) =>
                  BoundReference(o + numInputAttributes, dataType, nullable)
              }
            org.apache.spark.sql.catalyst.expressions
              .Coalesce(input :: default :: Nil)
          }
        case e =>
          BindReferences.bindReference(e, inputAttrs)
      }

    // Create the projection.
    newMutableProjection(boundExpressions, Nil)().target(target)
  }

  override def prepare(rows: RowBuffer): Unit = {
    input = rows
    // drain the first few rows if offset is larger than zero
    inputIndex = 0
    while (inputIndex < offset) {
      input.next()
      inputIndex += 1
    }
    inputIndex = offset
  }

  override def write(index: Int, current: InternalRow): Unit = {
    if (inputIndex >= 0 && inputIndex < input.size) {
      val r = input.next()
      join(r, current)
    } else {
      join(emptyRow, current)
    }
    projection(join)
    inputIndex += 1
  }
}

/**
  * The sliding window frame calculates frames with the following SQL form:
  * ... BETWEEN 1 PRECEDING AND 1 FOLLOWING
  *
  * @param target to write results to.
  * @param processor to calculate the row values with.
  * @param lbound comparator used to identify the lower bound of an output row.
  * @param ubound comparator used to identify the upper bound of an output row.
  */
private[execution] final class SlidingWindowFunctionFrame(
    target: MutableRow,
    processor: AggregateProcessor,
    lbound: BoundOrdering,
    ubound: BoundOrdering)
    extends WindowFunctionFrame {

  /** Rows of the partition currently being processed. */
  private[this] var input: RowBuffer = null

  /** The next row from `input`. */
  private[this] var nextRow: InternalRow = null

  /** The rows within current sliding window. */
  private[this] val buffer = new util.ArrayDeque[InternalRow]()

  /** Index of the first input row with a value greater than the upper bound of the current
    * output row. */
  private[this] var inputHighIndex = 0

  /** Index of the first input row with a value equal to or greater than the lower bound of the
    * current output row. */
  private[this] var inputLowIndex = 0

  /** Prepare the frame for calculating a new partition. Reset all variables. */
  override def prepare(rows: RowBuffer): Unit = {
    input = rows
    nextRow = rows.next()
    inputHighIndex = 0
    inputLowIndex = 0
    buffer.clear()
  }

  /** Write the frame columns for the current row to the given target row. */
  override def write(index: Int, current: InternalRow): Unit = {
    var bufferUpdated = index == 0

    // Add all rows to the buffer for which the input row value is equal to or less than
    // the output row upper bound.
    while (nextRow != null &&
           ubound.compare(nextRow, inputHighIndex, current, index) <= 0) {
      buffer.add(nextRow.copy())
      nextRow = input.next()
      inputHighIndex += 1
      bufferUpdated = true
    }

    // Drop all rows from the buffer for which the input row value is smaller than
    // the output row lower bound.
    while (!buffer.isEmpty &&
           lbound.compare(buffer.peek(), inputLowIndex, current, index) < 0) {
      buffer.remove()
      inputLowIndex += 1
      bufferUpdated = true
    }

    // Only recalculate and update when the buffer changes.
    if (bufferUpdated) {
      processor.initialize(input.size)
      val iter = buffer.iterator()
      while (iter.hasNext) {
        processor.update(iter.next())
      }
      processor.evaluate(target)
    }
  }
}

/**
  * The unbounded window frame calculates frames with the following SQL forms:
  * ... (No Frame Definition)
  * ... BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
  *
  * Its results are  the same for each and every row in the partition. This class can be seen as a
  * special case of a sliding window, but is optimized for the unbound case.
  *
  * @param target to write results to.
  * @param processor to calculate the row values with.
  */
private[execution] final class UnboundedWindowFunctionFrame(
    target: MutableRow,
    processor: AggregateProcessor)
    extends WindowFunctionFrame {

  /** Prepare the frame for calculating a new partition. Process all rows eagerly. */
  override def prepare(rows: RowBuffer): Unit = {
    val size = rows.size()
    processor.initialize(size)
    var i = 0
    while (i < size) {
      processor.update(rows.next())
      i += 1
    }
  }

  /** Write the frame columns for the current row to the given target row. */
  override def write(index: Int, current: InternalRow): Unit = {
    // Unfortunately we cannot assume that evaluation is deterministic. So we need to re-evaluate
    // for each row.
    processor.evaluate(target)
  }
}

/**
  * The UnboundPreceding window frame calculates frames with the following SQL form:
  * ... BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
  *
  * There is only an upper bound. Very common use cases are for instance running sums or counts
  * (row_number). Technically this is a special case of a sliding window. However a sliding window
  * has to maintain a buffer, and it must do a full evaluation everytime the buffer changes. This
  * is not the case when there is no lower bound, given the additive nature of most aggregates
  * streaming updates and partial evaluation suffice and no buffering is needed.
  *
  * @param target to write results to.
  * @param processor to calculate the row values with.
  * @param ubound comparator used to identify the upper bound of an output row.
  */
private[execution] final class UnboundedPrecedingWindowFunctionFrame(
    target: MutableRow,
    processor: AggregateProcessor,
    ubound: BoundOrdering)
    extends WindowFunctionFrame {

  /** Rows of the partition currently being processed. */
  private[this] var input: RowBuffer = null

  /** The next row from `input`. */
  private[this] var nextRow: InternalRow = null

  /** Index of the first input row with a value greater than the upper bound of the current
    * output row. */
  private[this] var inputIndex = 0

  /** Prepare the frame for calculating a new partition. */
  override def prepare(rows: RowBuffer): Unit = {
    input = rows
    nextRow = rows.next()
    inputIndex = 0
    processor.initialize(input.size)
  }

  /** Write the frame columns for the current row to the given target row. */
  override def write(index: Int, current: InternalRow): Unit = {
    var bufferUpdated = index == 0

    // Add all rows to the aggregates for which the input row value is equal to or less than
    // the output row upper bound.
    while (nextRow != null &&
           ubound.compare(nextRow, inputIndex, current, index) <= 0) {
      processor.update(nextRow)
      nextRow = input.next()
      inputIndex += 1
      bufferUpdated = true
    }

    // Only recalculate and update when the buffer changes.
    if (bufferUpdated) {
      processor.evaluate(target)
    }
  }
}

/**
  * The UnboundFollowing window frame calculates frames with the following SQL form:
  * ... BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING
  *
  * There is only an upper bound. This is a slightly modified version of the sliding window. The
  * sliding window operator has to check if both upper and the lower bound change when a new row
  * gets processed, where as the unbounded following only has to check the lower bound.
  *
  * This is a very expensive operator to use, O(n * (n - 1) /2), because we need to maintain a
  * buffer and must do full recalculation after each row. Reverse iteration would be possible, if
  * the communitativity of the used window functions can be guaranteed.
  *
  * @param target to write results to.
  * @param processor to calculate the row values with.
  * @param lbound comparator used to identify the lower bound of an output row.
  */
private[execution] final class UnboundedFollowingWindowFunctionFrame(
    target: MutableRow,
    processor: AggregateProcessor,
    lbound: BoundOrdering)
    extends WindowFunctionFrame {

  /** Rows of the partition currently being processed. */
  private[this] var input: RowBuffer = null

  /** Index of the first input row with a value equal to or greater than the lower bound of the
    * current output row. */
  private[this] var inputIndex = 0

  /** Prepare the frame for calculating a new partition. */
  override def prepare(rows: RowBuffer): Unit = {
    input = rows
    inputIndex = 0
  }

  /** Write the frame columns for the current row to the given target row. */
  override def write(index: Int, current: InternalRow): Unit = {
    var bufferUpdated = index == 0

    // Duplicate the input to have a new iterator
    val tmp = input.copy()

    // Drop all rows from the buffer for which the input row value is smaller than
    // the output row lower bound.
    tmp.skip(inputIndex)
    var nextRow = tmp.next()
    while (nextRow != null &&
           lbound.compare(nextRow, inputIndex, current, index) < 0) {
      nextRow = tmp.next()
      inputIndex += 1
      bufferUpdated = true
    }

    // Only recalculate and update when the buffer changes.
    if (bufferUpdated) {
      processor.initialize(input.size)
      while (nextRow != null) {
        processor.update(nextRow)
        nextRow = tmp.next()
      }
      processor.evaluate(target)
    }
  }
}

/**
  * This class prepares and manages the processing of a number of [[AggregateFunction]]s within a
  * single frame. The [[WindowFunctionFrame]] takes care of processing the frame in the correct way,
  * this reduces the processing of a [[AggregateWindowFunction]] to processing the underlying
  * [[AggregateFunction]]. All [[AggregateFunction]]s are processed in [[Complete]] mode.
  *
  * [[SizeBasedWindowFunction]]s are initialized in a slightly different way. These functions
  * require the size of the partition processed, this value is exposed to them when the processor is
  * constructed.
  *
  * Processing of distinct aggregates is currently not supported.
  *
  * The implementation is split into an object which takes care of construction, and a the actual
  * processor class.
  */
private[execution] object AggregateProcessor {
  def apply(
      functions: Array[Expression],
      ordinal: Int,
      inputAttributes: Seq[Attribute],
      newMutableProjection: (Seq[Expression], Seq[Attribute]) => () => MutableProjection)
    : AggregateProcessor = {
    val aggBufferAttributes = mutable.Buffer.empty[AttributeReference]
    val initialValues = mutable.Buffer.empty[Expression]
    val updateExpressions = mutable.Buffer.empty[Expression]
    val evaluateExpressions = mutable.Buffer.fill[Expression](ordinal)(NoOp)
    val imperatives = mutable.Buffer.empty[ImperativeAggregate]

    // Check if there are any SizeBasedWindowFunctions. If there are, we add the partition size to
    // the aggregation buffer. Note that the ordinal of the partition size value will always be 0.
    val trackPartitionSize =
      functions.exists(_.isInstanceOf[SizeBasedWindowFunction])
    if (trackPartitionSize) {
      aggBufferAttributes += SizeBasedWindowFunction.n
      initialValues += NoOp
      updateExpressions += NoOp
    }

    // Add an AggregateFunction to the AggregateProcessor.
    functions.foreach {
      case agg: DeclarativeAggregate =>
        aggBufferAttributes ++= agg.aggBufferAttributes
        initialValues ++= agg.initialValues
        updateExpressions ++= agg.updateExpressions
        evaluateExpressions += agg.evaluateExpression
      case agg: ImperativeAggregate =>
        val offset = aggBufferAttributes.size
        val imperative = BindReferences.bindReference(
          agg
            .withNewInputAggBufferOffset(offset)
            .withNewMutableAggBufferOffset(offset),
          inputAttributes)
        imperatives += imperative
        aggBufferAttributes ++= imperative.aggBufferAttributes
        val noOps = Seq.fill(imperative.aggBufferAttributes.size)(NoOp)
        initialValues ++= noOps
        updateExpressions ++= noOps
        evaluateExpressions += imperative
      case other =>
        sys.error(s"Unsupported Aggregate Function: $other")
    }

    // Create the projections.
    val initialProjection =
      newMutableProjection(initialValues, Seq(SizeBasedWindowFunction.n))()
    val updateProjection =
      newMutableProjection(updateExpressions,
                           aggBufferAttributes ++ inputAttributes)()
    val evaluateProjection =
      newMutableProjection(evaluateExpressions, aggBufferAttributes)()

    // Create the processor
    new AggregateProcessor(aggBufferAttributes.toArray,
                           initialProjection,
                           updateProjection,
                           evaluateProjection,
                           imperatives.toArray,
                           trackPartitionSize)
  }
}

/**
  * This class manages the processing of a number of aggregate functions. See the documentation of
  * the object for more information.
  */
private[execution] final class AggregateProcessor(
    private[this] val bufferSchema: Array[AttributeReference],
    private[this] val initialProjection: MutableProjection,
    private[this] val updateProjection: MutableProjection,
    private[this] val evaluateProjection: MutableProjection,
    private[this] val imperatives: Array[ImperativeAggregate],
    private[this] val trackPartitionSize: Boolean) {

  private[this] val join = new JoinedRow
  private[this] val numImperatives = imperatives.length
  private[this] val buffer = new SpecificMutableRow(
    bufferSchema.toSeq.map(_.dataType))
  initialProjection.target(buffer)
  updateProjection.target(buffer)

  /** Create the initial state. */
  def initialize(size: Int): Unit = {
    // Some initialization expressions are dependent on the partition size so we have to
    // initialize the size before initializing all other fields, and we have to pass the buffer to
    // the initialization projection.
    if (trackPartitionSize) {
      buffer.setInt(0, size)
    }
    initialProjection(buffer)
    var i = 0
    while (i < numImperatives) {
      imperatives(i).initialize(buffer)
      i += 1
    }
  }

  /** Update the buffer. */
  def update(input: InternalRow): Unit = {
    updateProjection(join(buffer, input))
    var i = 0
    while (i < numImperatives) {
      imperatives(i).update(buffer, input)
      i += 1
    }
  }

  /** Evaluate buffer. */
  def evaluate(target: MutableRow): Unit =
    evaluateProjection.target(target)(buffer)
}
