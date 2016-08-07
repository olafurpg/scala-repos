/**
  * Licensed to the Apache Software Foundation (ASF) under one or more
  * contributor license agreements.  See the NOTICE file distributed with
  * this work for additional information regarding copyright ownership.
  * The ASF licenses this file to You under the Apache License, Version 2.0
  * (the "License"); you may not use this file except in compliance with
  * the License.  You may obtain a copy of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
package kafka.server

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean

import kafka.api.{FetchResponsePartitionData, PartitionFetchInfo}
import kafka.cluster.Broker
import kafka.common.TopicAndPartition
import kafka.message.{ByteBufferMessageSet, Message}
import kafka.utils.{MockScheduler, MockTime, TestUtils, ZkUtils}
import org.I0Itec.zkclient.ZkClient
import org.apache.kafka.common.metrics.Metrics
import org.apache.kafka.common.protocol.Errors
import org.apache.kafka.common.requests.LeaderAndIsrRequest
import org.apache.kafka.common.requests.LeaderAndIsrRequest.PartitionState
import org.apache.kafka.common.requests.ProduceResponse.PartitionResponse
import org.apache.kafka.common.utils.{MockTime => JMockTime}
import org.apache.kafka.common.{BrokerEndPoint, TopicPartition}
import org.easymock.EasyMock
import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.collection.JavaConverters._
import scala.collection.Map

class ReplicaManagerTest {

  val topic = "test-topic"

  @Test
  def testHighWaterMarkDirectoryMapping() {
    val props = TestUtils.createBrokerConfig(1, TestUtils.MockZkConnect)
    val config = KafkaConfig.fromProps(props)
    val zkClient = EasyMock.createMock(classOf[ZkClient])
    val zkUtils = ZkUtils(zkClient, isZkSecurityEnabled = false)
    val mockLogMgr =
      TestUtils.createLogManager(config.logDirs.map(new File(_)).toArray)
    val time = new MockTime()
    val jTime = new JMockTime
    val metrics = new Metrics
    val rm = new ReplicaManager(config,
                                metrics,
                                time,
                                jTime,
                                zkUtils,
                                new MockScheduler(time),
                                mockLogMgr,
                                new AtomicBoolean(false))
    try {
      val partition = rm.getOrCreatePartition(topic, 1)
      partition.getOrCreateReplica(1)
      rm.checkpointHighWatermarks()
    } finally {
      // shutdown the replica manager upon test completion
      rm.shutdown(false)
      metrics.close()
    }
  }

  @Test
  def testHighwaterMarkRelativeDirectoryMapping() {
    val props = TestUtils.createBrokerConfig(1, TestUtils.MockZkConnect)
    props.put("log.dir", TestUtils.tempRelativeDir("data").getAbsolutePath)
    val config = KafkaConfig.fromProps(props)
    val zkClient = EasyMock.createMock(classOf[ZkClient])
    val zkUtils = ZkUtils(zkClient, false)
    val mockLogMgr =
      TestUtils.createLogManager(config.logDirs.map(new File(_)).toArray)
    val time = new MockTime()
    val jTime = new JMockTime
    val metrics = new Metrics
    val rm = new ReplicaManager(config,
                                metrics,
                                time,
                                jTime,
                                zkUtils,
                                new MockScheduler(time),
                                mockLogMgr,
                                new AtomicBoolean(false))
    try {
      val partition = rm.getOrCreatePartition(topic, 1)
      partition.getOrCreateReplica(1)
      rm.checkpointHighWatermarks()
    } finally {
      // shutdown the replica manager upon test completion
      rm.shutdown(checkpointHW = false)
      metrics.close()
    }
  }

  @Test
  def testIllegalRequiredAcks() {
    val props = TestUtils.createBrokerConfig(1, TestUtils.MockZkConnect)
    val config = KafkaConfig.fromProps(props)
    val zkClient = EasyMock.createMock(classOf[ZkClient])
    val zkUtils = ZkUtils(zkClient, isZkSecurityEnabled = false)
    val mockLogMgr =
      TestUtils.createLogManager(config.logDirs.map(new File(_)).toArray)
    val time = new MockTime()
    val jTime = new JMockTime
    val metrics = new Metrics
    val rm = new ReplicaManager(config,
                                metrics,
                                time,
                                jTime,
                                zkUtils,
                                new MockScheduler(time),
                                mockLogMgr,
                                new AtomicBoolean(false),
                                Option(this.getClass.getName))
    try {
      def callback(responseStatus: Map[TopicPartition, PartitionResponse]) = {
        assert(
            responseStatus.values.head.errorCode == Errors.INVALID_REQUIRED_ACKS.code)
      }
      rm.appendMessages(
          timeout = 0,
          requiredAcks = 3,
          internalTopicsAllowed = false,
          messagesPerPartition = Map(
              new TopicPartition("test1", 0) -> new ByteBufferMessageSet(
                  new Message("first message".getBytes))),
          responseCallback = callback)
    } finally {
      rm.shutdown(checkpointHW = false)
      metrics.close()
    }

    TestUtils.verifyNonDaemonThreadsStatus(this.getClass.getName)
  }

  @Test
  def testClearPurgatoryOnBecomingFollower() {
    val props = TestUtils.createBrokerConfig(0, TestUtils.MockZkConnect)
    props.put("log.dir", TestUtils.tempRelativeDir("data").getAbsolutePath)
    val config = KafkaConfig.fromProps(props)
    val zkClient = EasyMock.createMock(classOf[ZkClient])
    val zkUtils = ZkUtils(zkClient, isZkSecurityEnabled = false)
    val mockLogMgr =
      TestUtils.createLogManager(config.logDirs.map(new File(_)).toArray)
    val time = new MockTime()
    val jTime = new JMockTime
    val metrics = new Metrics
    val rm = new ReplicaManager(config,
                                metrics,
                                time,
                                jTime,
                                zkUtils,
                                new MockScheduler(time),
                                mockLogMgr,
                                new AtomicBoolean(false))

    try {
      var produceCallbackFired = false
      def produceCallback(
          responseStatus: Map[TopicPartition, PartitionResponse]) = {
        assertEquals("Should give NotLeaderForPartitionException",
                     Errors.NOT_LEADER_FOR_PARTITION.code,
                     responseStatus.values.head.errorCode)
        produceCallbackFired = true
      }

      var fetchCallbackFired = false
      def fetchCallback(responseStatus: Map[TopicAndPartition,
                                            FetchResponsePartitionData]) = {
        assertEquals("Should give NotLeaderForPartitionException",
                     Errors.NOT_LEADER_FOR_PARTITION.code,
                     responseStatus.values.head.error)
        fetchCallbackFired = true
      }

      val aliveBrokers =
        Seq(new Broker(0, "host0", 0), new Broker(1, "host1", 1))
      val metadataCache = EasyMock.createMock(classOf[MetadataCache])
      EasyMock
        .expect(metadataCache.getAliveBrokers)
        .andReturn(aliveBrokers)
        .anyTimes()
      EasyMock.replay(metadataCache)

      val brokerList: java.util.List[Integer] = Seq[Integer](0, 1).asJava
      val brokerSet: java.util.Set[Integer] = Set[Integer](0, 1).asJava

      val partition = rm.getOrCreatePartition(topic, 0)
      partition.getOrCreateReplica(0)
      // Make this replica the leader.
      val leaderAndIsrRequest1 = new LeaderAndIsrRequest(
          0,
          0,
          collection.immutable
            .Map(
                new TopicPartition(topic, 0) -> new PartitionState(0,
                                                                   0,
                                                                   0,
                                                                   brokerList,
                                                                   0,
                                                                   brokerSet))
            .asJava,
          Set(new BrokerEndPoint(0, "host1", 0),
              new BrokerEndPoint(1, "host2", 1)).asJava)
      rm.becomeLeaderOrFollower(0,
                                leaderAndIsrRequest1,
                                metadataCache,
                                (_, _) => {})
      rm.getLeaderReplicaIfLocal(topic, 0)

      // Append a message.
      rm.appendMessages(
          timeout = 1000,
          requiredAcks = -1,
          internalTopicsAllowed = false,
          messagesPerPartition = Map(
              new TopicPartition(topic, 0) -> new ByteBufferMessageSet(
                  new Message("first message".getBytes))),
          responseCallback = produceCallback)

      // Fetch some messages
      rm.fetchMessages(
          timeout = 1000,
          replicaId = -1,
          fetchMinBytes = 100000,
          fetchInfo = collection.immutable.Map(
              new TopicAndPartition(topic, 0) -> new PartitionFetchInfo(
                  0,
                  100000)),
          responseCallback = fetchCallback)

      // Make this replica the follower
      val leaderAndIsrRequest2 = new LeaderAndIsrRequest(
          0,
          0,
          collection.immutable
            .Map(
                new TopicPartition(topic, 0) -> new PartitionState(0,
                                                                   1,
                                                                   1,
                                                                   brokerList,
                                                                   0,
                                                                   brokerSet))
            .asJava,
          Set(new BrokerEndPoint(0, "host1", 0),
              new BrokerEndPoint(1, "host2", 1)).asJava)
      rm.becomeLeaderOrFollower(1,
                                leaderAndIsrRequest2,
                                metadataCache,
                                (_, _) => {})

      assertTrue(produceCallbackFired)
      assertTrue(fetchCallbackFired)
    } finally {
      rm.shutdown(checkpointHW = false)
      metrics.close()
    }
  }
}
