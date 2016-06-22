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

package org.apache.spark.sql.execution.python

import scala.collection.JavaConverters._

import net.razorvine.pickle.{Pickler, Unpickler}

import org.apache.spark.TaskContext
import org.apache.spark.api.python.PythonRunner
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.expressions.{Attribute, GenericMutableRow, JoinedRow, UnsafeProjection}
import org.apache.spark.sql.execution.SparkPlan
import org.apache.spark.sql.types.{StructField, StructType}

/**
  * A physical plan that evaluates a [[PythonUDF]], one partition of tuples at a time.
  *
  * Python evaluation works by sending the necessary (projected) input data via a socket to an
  * external Python process, and combine the result from the Python process with the original row.
  *
  * For each row we send to Python, we also put it in a queue. For each output row from Python,
  * we drain the queue to find the original input row. Note that if the Python process is way too
  * slow, this could lead to the queue growing unbounded and eventually run out of memory.
  */
case class BatchPythonEvaluation(udf: PythonUDF,
                                 output: Seq[Attribute],
                                 child: SparkPlan)
    extends SparkPlan {

  def children: Seq[SparkPlan] = child :: Nil

  protected override def doExecute(): RDD[InternalRow] = {
    val inputRDD = child.execute().map(_.copy())
    val bufferSize = inputRDD.conf.getInt("spark.buffer.size", 65536)
    val reuseWorker = inputRDD.conf
      .getBoolean("spark.python.worker.reuse", defaultValue = true)

    inputRDD.mapPartitions { iter =>
      EvaluatePython.registerPicklers() // register pickler for Row

      // The queue used to buffer input rows so we can drain it to
      // combine input with output from Python.
      val queue = new java.util.concurrent.ConcurrentLinkedQueue[InternalRow]()

      val pickle = new Pickler
      val currentRow = newMutableProjection(udf.children, child.output)()
      val fields = udf.children.map(_.dataType)
      val schema =
        new StructType(fields.map(t => new StructField("", t, true)).toArray)

      // Input iterator to Python: input rows are grouped so we send them in batches to Python.
      // For each row, add it to the queue.
      val inputIterator = iter.grouped(100).map { inputRows =>
        val toBePickled = inputRows.map { row =>
          queue.add(row)
          EvaluatePython.toJava(currentRow(row), schema)
        }.toArray
        pickle.dumps(toBePickled)
      }

      val context = TaskContext.get()

      // Output iterator for results from Python.
      val outputIterator = new PythonRunner(
          udf.func,
          bufferSize,
          reuseWorker
      ).compute(inputIterator, context.partitionId(), context)

      val unpickle = new Unpickler
      val row = new GenericMutableRow(1)
      val joined = new JoinedRow
      val resultProj = UnsafeProjection.create(output, output)

      outputIterator.flatMap { pickedResult =>
        val unpickledBatch = unpickle.loads(pickedResult)
        unpickledBatch.asInstanceOf[java.util.ArrayList[Any]].asScala
      }.map { result =>
        row(0) = EvaluatePython.fromJava(result, udf.dataType)
        resultProj(joined(queue.poll(), row))
      }
    }
  }
}
