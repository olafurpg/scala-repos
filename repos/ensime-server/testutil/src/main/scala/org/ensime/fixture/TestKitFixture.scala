// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.fixture

import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import java.util.concurrent.TimeUnit
import org.scalatest._
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.testkit._

/**
 * Normally a TestKit will reuse the same actor system for all tests
 * in a suite, but sometimes isolation of the system is needed on a
 * per-test basis, this fixture adds support for that.
 *
 * Instead of extending TestKit, use withTestKit and import
 * the parameter for all implicits.
 *
 * Inspired by https://gist.github.com/derekwyatt/3138807
 */
trait TestKitFixture {
  require(
    !this.isInstanceOf[TestKit],
    "IsolatedActorSystems are incompatible with TestKit. Instead, 'import sys._'"
  )

  implicit protected val akkaTimeout: Timeout = ConfigFactory.load().getDuration("akka.test.default-timeout", TimeUnit.MILLISECONDS) milliseconds

  def withTestKit(testCode: TestKitFix => Any): Any
}

class TestKitFix extends TestKit(ActorSystem()) with ImplicitSender

trait IsolatedTestKitFixture extends TestKitFixture {
  override def withTestKit(testCode: TestKitFix => Any): Any = {
    val sys = new TestKitFix
    try {
      testCode(sys)
    } finally {
      sys.system.shutdown()
      sys.system.awaitTermination()
    }
  }
}

// this seems redundant, because it mimics "extends TestKit" behaviour,
// but it allows for easy swapping with the refreshing implementation
trait SharedTestKitFixture extends TestKitFixture with BeforeAndAfterAll {
  this: Suite =>

  var _testkit: TestKitFix = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _testkit = new TestKitFix
  }

  override def afterAll(): Unit = {
    super.afterAll()
    _testkit.system.shutdown()
    _testkit.system.awaitTermination()
  }

  override def withTestKit(testCode: TestKitFix => Any): Any = testCode(_testkit)

}
