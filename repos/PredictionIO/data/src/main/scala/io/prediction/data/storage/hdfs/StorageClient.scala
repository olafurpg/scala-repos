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
package io.prediction.data.storage.hdfs

import grizzled.slf4j.Logging
import io.prediction.data.storage.BaseStorageClient
import io.prediction.data.storage.StorageClientConfig
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.FileSystem
import org.apache.hadoop.fs.Path

class StorageClient(val config: StorageClientConfig)
    extends BaseStorageClient
    with Logging {
  override val prefix = "HDFS"
  val conf = new Configuration
  val fs = FileSystem.get(conf)
  fs.setWorkingDirectory(new Path(
          config.properties.getOrElse("PATH", config.properties("HOSTS"))))
  val client = fs
}
