package pin

import bin.A
import bin.B
import bin.C
import bin.D
import tin._
import bon.G

class FixImport extends G {
  val x = new /*ref*/ E
}
/*
package pin
import bin.{bon => _, _}
import tin._
import bon.G
class FixImport extends G {
  val x = new E
}
 */
