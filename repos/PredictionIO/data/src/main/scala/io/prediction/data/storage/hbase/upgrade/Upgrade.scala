/** Copyright 2015 TappingStone, Inc.
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
package io.prediction.data.storage.hbase.upgrade

import io.prediction.annotation.Experimental

import io.prediction.data.storage.Storage
import io.prediction.data.storage.hbase.HBLEvents
import io.prediction.data.storage.hbase.HBEventsUtil

import scala.collection.JavaConversions._

/** :: Experimental :: */
@Experimental
object Upgrade {

  def main(args: Array[String]) {
    val fromAppId = args(0).toInt
    val toAppId = args(1).toInt
    val batchSize = args.lift(2).map(_.toInt).getOrElse(100)
    val fromNamespace = args.lift(3).getOrElse("predictionio_eventdata")

    upgrade(fromAppId, toAppId, batchSize, fromNamespace)
  }

  /* For upgrade from 0.8.0 or 0.8.1 to 0.8.2 only */
  def upgrade(fromAppId: Int,
              toAppId: Int,
              batchSize: Int,
              fromNamespace: String) {

    val events = Storage.getLEvents().asInstanceOf[HBLEvents]

    // Assume already run "pio app new <newapp>" (new app already created)
    // TODO: check if new table empty and warn user if not
    val newTable = events.getTable(toAppId)

    val newTableName = newTable.getName().getNameAsString()
    println(
        s"Copying data from ${fromNamespace}:events for app ID ${fromAppId}" +
          s" to new HBase table ${newTableName}...")

    HB_0_8_0
      .getByAppId(events.client.connection, fromNamespace, fromAppId)
      .grouped(batchSize)
      .foreach { eventGroup =>
        val puts = eventGroup.map { e =>
          val (put, rowkey) = HBEventsUtil.eventToPut(e, toAppId)
          put
        }
        newTable.put(puts.toList)
      }

    newTable.flushCommits()
    newTable.close()
    println("Done.")
  }
}
