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

package org.apache.spark.streaming.api.java

import java.{lang => jl}
import java.util.{List => JList}

import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.reflect.ClassTag

import org.apache.spark.api.java.{JavaPairRDD, JavaRDD, JavaRDDLike}
import org.apache.spark.api.java.JavaPairRDD._
import org.apache.spark.api.java.JavaSparkContext.fakeClassTag
import org.apache.spark.api.java.function.{
  Function => JFunction,
  Function2 => JFunction2,
  Function3 => JFunction3,
  VoidFunction => JVoidFunction,
  VoidFunction2 => JVoidFunction2,
  _
}
import org.apache.spark.rdd.RDD
import org.apache.spark.streaming._
import org.apache.spark.streaming.api.java.JavaDStream._
import org.apache.spark.streaming.dstream.DStream

/**
  * As a workaround for https://issues.scala-lang.org/browse/SI-8905, implementations
  * of JavaDStreamLike should extend this dummy abstract class instead of directly inheriting
  * from the trait. See SPARK-3266 for additional details.
  */
private[streaming] abstract class AbstractJavaDStreamLike[
    T, This <: JavaDStreamLike[T, This, R], R <: JavaRDDLike[T, R]]
    extends JavaDStreamLike[T, This, R]

trait JavaDStreamLike[
    T, This <: JavaDStreamLike[T, This, R], R <: JavaRDDLike[T, R]]
    extends Serializable {
  implicit val classTag: ClassTag[T]

  def dstream: DStream[T]

  def wrapRDD(in: RDD[T]): R

  // This is just unfortunate we made a mistake in naming -- should be scalaLongToJavaLong.
  // Don't fix this for now as it would break binary compatibility.
  implicit def scalaIntToJavaLong(in: DStream[Long]): JavaDStream[jl.Long] = {
    in.map(jl.Long.valueOf)
  }

  /**
    * Print the first ten elements of each RDD generated in this DStream. This is an output
    * operator, so this DStream will be registered as an output stream and there materialized.
    */
  def print(): Unit = {
    print(10)
  }

  /**
    * Print the first num elements of each RDD generated in this DStream. This is an output
    * operator, so this DStream will be registered as an output stream and there materialized.
    */
  def print(num: Int): Unit = {
    dstream.print(num)
  }

  /**
    * Return a new DStream in which each RDD has a single element generated by counting each RDD
    * of this DStream.
    */
  def count(): JavaDStream[jl.Long] = dstream.count()

  /**
    * Return a new DStream in which each RDD contains the counts of each distinct value in
    * each RDD of this DStream.  Hash partitioning is used to generate the RDDs with
    * Spark's default number of partitions.
    */
  def countByValue(): JavaPairDStream[T, jl.Long] = {
    JavaPairDStream.scalaToJavaLong(dstream.countByValue())
  }

  /**
    * Return a new DStream in which each RDD contains the counts of each distinct value in
    * each RDD of this DStream. Hash partitioning is used to generate the RDDs with `numPartitions`
    * partitions.
    * @param numPartitions  number of partitions of each RDD in the new DStream.
    */
  def countByValue(numPartitions: Int): JavaPairDStream[T, jl.Long] = {
    JavaPairDStream.scalaToJavaLong(dstream.countByValue(numPartitions))
  }

  /**
    * Return a new DStream in which each RDD has a single element generated by counting the number
    * of elements in a window over this DStream. windowDuration and slideDuration are as defined in
    * the window() operation. This is equivalent to window(windowDuration, slideDuration).count()
    */
  def countByWindow(windowDuration: Duration,
                    slideDuration: Duration): JavaDStream[jl.Long] = {
    dstream.countByWindow(windowDuration, slideDuration)
  }

  /**
    * Return a new DStream in which each RDD contains the count of distinct elements in
    * RDDs in a sliding window over this DStream. Hash partitioning is used to generate the RDDs
    * with Spark's default number of partitions.
    * @param windowDuration width of the window; must be a multiple of this DStream's
    *                       batching interval
    * @param slideDuration  sliding interval of the window (i.e., the interval after which
    *                       the new DStream will generate RDDs); must be a multiple of this
    *                       DStream's batching interval
    */
  def countByValueAndWindow(
      windowDuration: Duration,
      slideDuration: Duration): JavaPairDStream[T, jl.Long] = {
    JavaPairDStream.scalaToJavaLong(
        dstream.countByValueAndWindow(windowDuration, slideDuration))
  }

  /**
    * Return a new DStream in which each RDD contains the count of distinct elements in
    * RDDs in a sliding window over this DStream. Hash partitioning is used to generate the RDDs
    * with `numPartitions` partitions.
    * @param windowDuration width of the window; must be a multiple of this DStream's
    *                       batching interval
    * @param slideDuration  sliding interval of the window (i.e., the interval after which
    *                       the new DStream will generate RDDs); must be a multiple of this
    *                       DStream's batching interval
    * @param numPartitions  number of partitions of each RDD in the new DStream.
    */
  def countByValueAndWindow(
      windowDuration: Duration,
      slideDuration: Duration,
      numPartitions: Int): JavaPairDStream[T, jl.Long] = {
    JavaPairDStream.scalaToJavaLong(
        dstream
          .countByValueAndWindow(windowDuration, slideDuration, numPartitions))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying glom() to each RDD of
    * this DStream. Applying glom() to an RDD coalesces all elements within each partition into
    * an array.
    */
  def glom(): JavaDStream[JList[T]] =
    new JavaDStream(dstream.glom().map(_.toSeq.asJava))

  /** Return the [[org.apache.spark.streaming.StreamingContext]] associated with this DStream */
  def context(): StreamingContext = dstream.context

  /** Return a new DStream by applying a function to all elements of this DStream. */
  def map[R](f: JFunction[T, R]): JavaDStream[R] = {
    new JavaDStream(dstream.map(f)(fakeClassTag))(fakeClassTag)
  }

  /** Return a new DStream by applying a function to all elements of this DStream. */
  def mapToPair[K2, V2](f: PairFunction[T, K2, V2]): JavaPairDStream[K2, V2] = {
    def cm: ClassTag[(K2, V2)] = fakeClassTag
    new JavaPairDStream(dstream.map[(K2, V2)](f)(cm))(fakeClassTag[K2],
                                                      fakeClassTag[V2])
  }

  /**
    * Return a new DStream by applying a function to all elements of this DStream,
    * and then flattening the results
    */
  def flatMap[U](f: FlatMapFunction[T, U]): JavaDStream[U] = {
    def fn: (T) => Iterator[U] = (x: T) => f.call(x).asScala
    new JavaDStream(dstream.flatMap(fn)(fakeClassTag[U]))(fakeClassTag[U])
  }

  /**
    * Return a new DStream by applying a function to all elements of this DStream,
    * and then flattening the results
    */
  def flatMapToPair[K2, V2](
      f: PairFlatMapFunction[T, K2, V2]): JavaPairDStream[K2, V2] = {
    def fn: (T) => Iterator[(K2, V2)] = (x: T) => f.call(x).asScala
    def cm: ClassTag[(K2, V2)] = fakeClassTag
    new JavaPairDStream(dstream.flatMap(fn)(cm))(fakeClassTag[K2],
                                                 fakeClassTag[V2])
  }

  /**
    * Return a new DStream in which each RDD is generated by applying mapPartitions() to each RDDs
    * of this DStream. Applying mapPartitions() to an RDD applies a function to each partition
    * of the RDD.
    */
  def mapPartitions[U](
      f: FlatMapFunction[java.util.Iterator[T], U]): JavaDStream[U] = {
    def fn: (Iterator[T]) => Iterator[U] = { (x: Iterator[T]) =>
      f.call(x.asJava).asScala
    }
    new JavaDStream(dstream.mapPartitions(fn)(fakeClassTag[U]))(
        fakeClassTag[U])
  }

  /**
    * Return a new DStream in which each RDD is generated by applying mapPartitions() to each RDDs
    * of this DStream. Applying mapPartitions() to an RDD applies a function to each partition
    * of the RDD.
    */
  def mapPartitionsToPair[K2, V2](
      f: PairFlatMapFunction[java.util.Iterator[T], K2, V2])
    : JavaPairDStream[K2, V2] = {
    def fn: (Iterator[T]) => Iterator[(K2, V2)] = { (x: Iterator[T]) =>
      f.call(x.asJava).asScala
    }
    new JavaPairDStream(dstream.mapPartitions(fn))(fakeClassTag[K2],
                                                   fakeClassTag[V2])
  }

  /**
    * Return a new DStream in which each RDD has a single element generated by reducing each RDD
    * of this DStream.
    */
  def reduce(f: JFunction2[T, T, T]): JavaDStream[T] = dstream.reduce(f)

  /**
    * Return a new DStream in which each RDD has a single element generated by reducing all
    * elements in a sliding window over this DStream.
    * @param reduceFunc associative and commutative reduce function
    * @param windowDuration width of the window; must be a multiple of this DStream's
    *                       batching interval
    * @param slideDuration  sliding interval of the window (i.e., the interval after which
    *                       the new DStream will generate RDDs); must be a multiple of this
    *                       DStream's batching interval
    */
  def reduceByWindow(
      reduceFunc: JFunction2[T, T, T],
      windowDuration: Duration,
      slideDuration: Duration
  ): JavaDStream[T] = {
    dstream.reduceByWindow(reduceFunc, windowDuration, slideDuration)
  }

  /**
    * Return a new DStream in which each RDD has a single element generated by reducing all
    * elements in a sliding window over this DStream. However, the reduction is done incrementally
    * using the old window's reduced value :
    *  1. reduce the new values that entered the window (e.g., adding new counts)
    *  2. "inverse reduce" the old values that left the window (e.g., subtracting old counts)
    *  This is more efficient than reduceByWindow without "inverse reduce" function.
    *  However, it is applicable to only "invertible reduce functions".
    * @param reduceFunc associative and commutative reduce function
    * @param invReduceFunc inverse reduce function
    * @param windowDuration width of the window; must be a multiple of this DStream's
    *                       batching interval
    * @param slideDuration  sliding interval of the window (i.e., the interval after which
    *                       the new DStream will generate RDDs); must be a multiple of this
    *                       DStream's batching interval
    */
  def reduceByWindow(
      reduceFunc: JFunction2[T, T, T],
      invReduceFunc: JFunction2[T, T, T],
      windowDuration: Duration,
      slideDuration: Duration
  ): JavaDStream[T] = {
    dstream
      .reduceByWindow(reduceFunc, invReduceFunc, windowDuration, slideDuration)
  }

  /**
    * Return all the RDDs between 'fromDuration' to 'toDuration' (both included)
    */
  def slice(fromTime: Time, toTime: Time): JList[R] = {
    dstream.slice(fromTime, toTime).map(wrapRDD).asJava
  }

  /**
    * Apply a function to each RDD in this DStream. This is an output operator, so
    * 'this' DStream will be registered as an output stream and therefore materialized.
    */
  def foreachRDD(foreachFunc: JVoidFunction[R]) {
    dstream.foreachRDD(rdd => foreachFunc.call(wrapRDD(rdd)))
  }

  /**
    * Apply a function to each RDD in this DStream. This is an output operator, so
    * 'this' DStream will be registered as an output stream and therefore materialized.
    */
  def foreachRDD(foreachFunc: JVoidFunction2[R, Time]) {
    dstream.foreachRDD((rdd, time) => foreachFunc.call(wrapRDD(rdd), time))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream.
    */
  def transform[U](transformFunc: JFunction[R, JavaRDD[U]]): JavaDStream[U] = {
    implicit val cm: ClassTag[U] = fakeClassTag

    def scalaTransform(in: RDD[T]): RDD[U] =
      transformFunc.call(wrapRDD(in)).rdd
    dstream.transform(scalaTransform(_))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream.
    */
  def transform[U](
      transformFunc: JFunction2[R, Time, JavaRDD[U]]): JavaDStream[U] = {
    implicit val cm: ClassTag[U] = fakeClassTag

    def scalaTransform(in: RDD[T], time: Time): RDD[U] =
      transformFunc.call(wrapRDD(in), time).rdd
    dstream.transform(scalaTransform(_, _))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream.
    */
  def transformToPair[K2, V2](transformFunc: JFunction[R, JavaPairRDD[K2, V2]])
    : JavaPairDStream[K2, V2] = {
    implicit val cmk: ClassTag[K2] = fakeClassTag
    implicit val cmv: ClassTag[V2] = fakeClassTag

    def scalaTransform(in: RDD[T]): RDD[(K2, V2)] =
      transformFunc.call(wrapRDD(in)).rdd
    dstream.transform(scalaTransform(_))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream.
    */
  def transformToPair[K2, V2](
      transformFunc: JFunction2[R, Time, JavaPairRDD[K2, V2]])
    : JavaPairDStream[K2, V2] = {
    implicit val cmk: ClassTag[K2] = fakeClassTag
    implicit val cmv: ClassTag[V2] = fakeClassTag

    def scalaTransform(in: RDD[T], time: Time): RDD[(K2, V2)] =
      transformFunc.call(wrapRDD(in), time).rdd
    dstream.transform(scalaTransform(_, _))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream and 'other' DStream.
    */
  def transformWith[U, W](
      other: JavaDStream[U],
      transformFunc: JFunction3[R, JavaRDD[U], Time, JavaRDD[W]]
  ): JavaDStream[W] = {
    implicit val cmu: ClassTag[U] = fakeClassTag
    implicit val cmv: ClassTag[W] = fakeClassTag

    def scalaTransform(inThis: RDD[T], inThat: RDD[U], time: Time): RDD[W] =
      transformFunc.call(wrapRDD(inThis), other.wrapRDD(inThat), time).rdd
    dstream.transformWith[U, W](other.dstream, scalaTransform(_, _, _))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream and 'other' DStream.
    */
  def transformWithToPair[U, K2, V2](
      other: JavaDStream[U],
      transformFunc: JFunction3[R, JavaRDD[U], Time, JavaPairRDD[K2, V2]]
  ): JavaPairDStream[K2, V2] = {
    implicit val cmu: ClassTag[U] = fakeClassTag
    implicit val cmk2: ClassTag[K2] = fakeClassTag
    implicit val cmv2: ClassTag[V2] = fakeClassTag
    def scalaTransform(inThis: RDD[T],
                       inThat: RDD[U],
                       time: Time): RDD[(K2, V2)] =
      transformFunc.call(wrapRDD(inThis), other.wrapRDD(inThat), time).rdd
    dstream.transformWith[U, (K2, V2)](other.dstream, scalaTransform(_, _, _))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream and 'other' DStream.
    */
  def transformWith[K2, V2, W](
      other: JavaPairDStream[K2, V2],
      transformFunc: JFunction3[R, JavaPairRDD[K2, V2], Time, JavaRDD[W]]
  ): JavaDStream[W] = {
    implicit val cmk2: ClassTag[K2] = fakeClassTag
    implicit val cmv2: ClassTag[V2] = fakeClassTag
    implicit val cmw: ClassTag[W] = fakeClassTag

    def scalaTransform(inThis: RDD[T],
                       inThat: RDD[(K2, V2)],
                       time: Time): RDD[W] = {
      transformFunc.call(wrapRDD(inThis), other.wrapRDD(inThat), time).rdd
    }
    dstream.transformWith[(K2, V2), W](other.dstream, scalaTransform(_, _, _))
  }

  /**
    * Return a new DStream in which each RDD is generated by applying a function
    * on each RDD of 'this' DStream and 'other' DStream.
    */
  def transformWithToPair[K2, V2, K3, V3](
      other: JavaPairDStream[K2, V2],
      transformFunc: JFunction3[R,
                                JavaPairRDD[K2, V2],
                                Time,
                                JavaPairRDD[K3, V3]]
  ): JavaPairDStream[K3, V3] = {
    implicit val cmk2: ClassTag[K2] = fakeClassTag
    implicit val cmv2: ClassTag[V2] = fakeClassTag
    implicit val cmk3: ClassTag[K3] = fakeClassTag
    implicit val cmv3: ClassTag[V3] = fakeClassTag
    def scalaTransform(inThis: RDD[T],
                       inThat: RDD[(K2, V2)],
                       time: Time): RDD[(K3, V3)] =
      transformFunc.call(wrapRDD(inThis), other.wrapRDD(inThat), time).rdd
    dstream.transformWith[(K2, V2), (K3, V3)](other.dstream,
                                              scalaTransform(_, _, _))
  }

  /**
    * Enable periodic checkpointing of RDDs of this DStream.
    * @param interval Time interval after which generated RDD will be checkpointed
    */
  def checkpoint(interval: Duration): DStream[T] = {
    dstream.checkpoint(interval)
  }
}
