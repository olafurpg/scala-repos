/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package scalaguide.tests.specs2

import play.api.test._

import org.specs2.execute.{Result, AsResult}

class WithDbDataSpec extends PlaySpecification {

  // #scalafunctionaltest-withdbdata
  abstract class WithDbData extends WithApplication {
    override def around[T: AsResult](t: => T): Result = super.around {
      setupData()
      t
    }

    def setupData() {
      // setup data
    }
  }

  "Computer model" should {

    "be retrieved by id" in new WithDbData {
      // your test code
    }
    "be retrieved by email" in new WithDbData {
      // your test code
    }
  }
  // #scalafunctionaltest-withdbdata
}
