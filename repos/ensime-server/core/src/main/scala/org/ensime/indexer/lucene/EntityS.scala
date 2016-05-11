// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.lucene

abstract class EntityS[T <: Entity](clazz: Class[T]) extends Serializer(clazz) {
  def id(t: T) = t.id
}
