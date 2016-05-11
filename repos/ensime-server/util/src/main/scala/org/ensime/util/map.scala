// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import Predef.{ any2stringadd => _, _ => _ }

package object map {
  implicit class RichMap[K, V](val map: Map[K, V]) extends AnyVal {
    /**
     * Map.mapValues is notoriously inconsistent and returns a View
     * rather than a solid implementation, this is what you thought it
     * did.
     */
    def mapValuesEagerly[W](f: V => W): Map[K, W] = map.map {
      case (k, v) => (k, f(v))
    }
  }

  // I'm sure CanBuildFrom could make this general to all value containers
  implicit class RichMultiMapSet[K, V](val map: Map[K, Set[V]]) extends AnyVal {
    /**
     * Treating `map` as a multimap, merge with another similarly
     * structured object removing duplicate values.
     */
    def merge(other: Map[K, Set[V]]): Map[K, Set[V]] = {
      import collection.mutable
      val builder = new mutable.HashMap[K, mutable.Set[V]] with mutable.MultiMap[K, V]
      builder ++= map.mapValuesEagerly { v => v.to[mutable.Set] }

      for {
        (k, vs) <- other
        v <- vs
      } builder.addBinding(k, v)
      builder.map {
        case (k, vs) => (k, vs.toSet)
      }(collection.breakOut)
    }

  }

}
