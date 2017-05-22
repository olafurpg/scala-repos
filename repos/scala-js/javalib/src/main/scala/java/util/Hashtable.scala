package java.util

import java.{util => ju}

import scala.collection.mutable
import scala.collection.JavaConversions._

class Hashtable[K, V] private (inner: mutable.HashMap[Box[Any], V])
    extends ju.Dictionary[K, V] with ju.Map[K, V] with Cloneable
    with Serializable

  def this() =
    this(mutable.HashMap.empty[Box[Any], V])

  def this(initialCapacity: Int) = this()

  def this(initialCapacity: Int, loadFactor: Float) = this()

  def this(t: ju.Map[_ <: K, _ <: V]) =
    this()
    putAll(t)

  def size(): Int =
    inner.size

  def isEmpty: Boolean =
    inner.isEmpty

  def keys(): ju.Enumeration[K] =
    inner.keysIterator.map(_.inner.asInstanceOf[K])

  def elements(): ju.Enumeration[V] =
    inner.valuesIterator

  def contains(value: Any): Boolean =
    containsValue(value)

  def containsValue(value: Any): Boolean =
    inner.containsValue(value)

  def containsKey(key: Any): Boolean =
    inner.contains(Box(key))

  def get(key: Any): V =
    if (key == null) throw new NullPointerException
    inner.getOrElse(Box(key), null.asInstanceOf[V])

  // Not implemented
  // protected def rehash(): Unit

  def put(key: K, value: V): V =
    inner
      .put(Box(key.asInstanceOf[AnyRef]), value)
      .getOrElse(null.asInstanceOf[V])

  def remove(key: Any): V =
    if (key == null) throw new NullPointerException
    inner.remove(Box(key)).getOrElse(null.asInstanceOf[V])

  def putAll(m: ju.Map[_ <: K, _ <: V]): Unit =
    m.iterator.foreach(kv => inner.put(Box(kv._1.asInstanceOf[AnyRef]), kv._2))

  def clear(): Unit =
    inner.clear()

  override def clone(): AnyRef =
    new ju.Hashtable[K, V](this)

  override def toString(): String =
    inner.iterator
      .map(kv => kv._1.inner + "=" + kv._2)
      .mkString("{", ", ", "}")

  def keySet(): ju.Set[K] =
    inner.keySet.map(_.inner.asInstanceOf[K])

  def entrySet(): ju.Set[ju.Map.Entry[K, V]] =
    class UnboxedEntry(
        private[UnboxedEntry] val boxedEntry: ju.Map.Entry[Box[Any], V])
        extends ju.Map.Entry[K, V]
      def getKey(): K = boxedEntry.getKey.inner.asInstanceOf[K]
      def getValue(): V = boxedEntry.getValue
      def setValue(value: V): V = boxedEntry.setValue(value)
      override def equals(o: Any): Boolean = o match
        case o: UnboxedEntry => boxedEntry.equals(o.boxedEntry)
        case _ => false
      override def hashCode(): Int = boxedEntry.hashCode()
    setAsJavaSet(inner.entrySet().map(new UnboxedEntry(_)))

  def values(): ju.Collection[V] =
    inner.values
