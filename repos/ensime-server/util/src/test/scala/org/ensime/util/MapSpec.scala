// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import Predef.{ any2stringadd => _, _ => _ }

import org.scalatest._

class MapSpec extends FlatSpec with Matchers {
  import map._

  val as = Map(
    1 -> Set('a, 'b, 'c),
    2 -> Set('b)
  )

  val bs = Map(
    1 -> Set('c, 'd),
    2 -> Set('a),
    3 -> Set('e)
  )

  val merged = Map(
    1 -> Set('a, 'b, 'c, 'd),
    2 -> Set('a, 'b),
    3 -> Set('e)
  )

  "map._" should "map values eagerly" in {
    var count = 0
    val mapped = merged.mapValuesEagerly { syms =>
      count += 1
      syms.head
    }

    count shouldBe 3 // not lazy

    mapped shouldBe Map(1 -> 'a, 2 -> 'a, 3 -> 'e)
  }

  it should "merge multimap sets" in {
    (as merge Map.empty) shouldBe as
    (Map.empty[Int, Set[Symbol]] merge as) shouldBe as

    (as merge bs) shouldBe merged
  }

}
