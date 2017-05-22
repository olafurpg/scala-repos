package com.twitter.conversions.common

import com.twitter.common.zookeeper.ZooKeeperClient
import com.twitter.conversions.common.quantity.COMMON_FOREVER
import com.twitter.util.{Duration, FuturePool}
import com.twitter.zk.{CommonConnector, ZkClient}
import scala.language.implicitConversions

/** Adapters for common's ZooKeeperClient (and, later, serversets, etc) */
object zookeeper
  class CommonZkClientAdapter(zkc: ZooKeeperClient)
    def toConnector(timeout: Duration = COMMON_FOREVER)(
        implicit pool: FuturePool): CommonConnector =
      new CommonConnector(zkc, timeout)

    def toZkClient(timeout: Duration = COMMON_FOREVER)(
        implicit pool: FuturePool): ZkClient =
      ZkClient(toConnector(timeout))

  /** Implicit conversion of ZooKeeperClient to CommonZkClient */
  implicit def commonZkClient(zkc: ZooKeeperClient): CommonZkClientAdapter =
    new CommonZkClientAdapter(zkc)
