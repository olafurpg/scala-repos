package com.twitter.hashing

trait Distributor[A]
  def entryForHash(hash: Long): (Long, A)
  def nodeForHash(hash: Long): A
  def nodeCount: Int
  def nodes: Seq[A]

class SingletonDistributor[A](node: A) extends Distributor[A]
  def entryForHash(hash: Long) = (hash, node)
  def nodeForHash(hash: Long) = node
  def nodeCount = 1
  def nodes = Seq(node)
