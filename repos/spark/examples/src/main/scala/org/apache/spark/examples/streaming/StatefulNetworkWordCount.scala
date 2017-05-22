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

// scalastyle:off println
package org.apache.spark.examples.streaming

import org.apache.spark.SparkConf
import org.apache.spark.streaming._

/**
  * Counts words cumulatively in UTF8 encoded, '\n' delimited text received from the network every
  * second starting with initial value of word count.
  * Usage: StatefulNetworkWordCount <hostname> <port>
  *   <hostname> and <port> describe the TCP server that Spark Streaming would connect to receive
  *   data.
  *
  * To run this on your local machine, you need to first run a Netcat server
  *    `$ nc -lk 9999`
  * and then run the example
  *    `$ bin/run-example
  *      org.apache.spark.examples.streaming.StatefulNetworkWordCount localhost 9999`
  */
object StatefulNetworkWordCount
  def main(args: Array[String])
    if (args.length < 2)
      System.err.println("Usage: StatefulNetworkWordCount <hostname> <port>")
      System.exit(1)

    StreamingExamples.setStreamingLogLevels()

    val sparkConf = new SparkConf().setAppName("StatefulNetworkWordCount")
    // Create the context with a 1 second batch size
    val ssc = new StreamingContext(sparkConf, Seconds(1))
    ssc.checkpoint(".")

    // Initial state RDD for mapWithState operation
    val initialRDD =
      ssc.sparkContext.parallelize(List(("hello", 1), ("world", 1)))

    // Create a ReceiverInputDStream on target ip:port and count the
    // words in input stream of \n delimited test (eg. generated by 'nc')
    val lines = ssc.socketTextStream(args(0), args(1).toInt)
    val words = lines.flatMap(_.split(" "))
    val wordDstream = words.map(x => (x, 1))

    // Update the cumulative count using mapWithState
    // This will give a DStream made of state (which is the cumulative count of the words)
    val mappingFunc = (word: String, one: Option[Int], state: State[Int]) =>
        val sum = one.getOrElse(0) + state.getOption.getOrElse(0)
        val output = (word, sum)
        state.update(sum)
        output

    val stateDstream = wordDstream.mapWithState(
        StateSpec.function(mappingFunc).initialState(initialRDD))
    stateDstream.print()
    ssc.start()
    ssc.awaitTermination()
// scalastyle:on println
