/**
  * Copyright (C) 2009-2016 Lightbend Inc. <http://www.lightbend.com>
  */
package akka.persistence

import akka.actor.DeadLetter
import akka.persistence.PersistentActorBoundedStashingSpec._
import akka.persistence.journal.SteppingInmemJournal
import akka.testkit.TestEvent.Mute
import akka.testkit.EventFilter
import akka.testkit.ImplicitSender
import com.typesafe.config.Config
import org.scalatest.BeforeAndAfterEach

import scala.concurrent.duration._

object PersistentActorBoundedStashingSpec {
  final case class Cmd(data: Any)
  final case class Evt(data: Any)

  class ReplyToWithRejectConfigurator
      extends StashOverflowStrategyConfigurator {
    override def create(config: Config): StashOverflowStrategy =
      ReplyToStrategy("RejectToStash")
  }

  class StashOverflowStrategyFromConfigPersistentActor(name: String)
      extends NamedPersistentActor(name) {
    var events: List[Any] = Nil

    val updateState: Receive = {
      case Evt(data) ⇒ events = data :: events
    }

    val commonBehavior: Receive = {
      case GetState ⇒ sender() ! events.reverse
    }

    def receiveRecover = updateState

    override def receiveCommand: Receive = commonBehavior orElse {
      case Cmd(x: Any) ⇒ persist(Evt(x))(updateState)
    }
  }

  val capacity = 10

  val templateConfig = s"""
       |akka.actor.default-mailbox.stash-capacity=$capacity
       |akka.actor.guardian-supervisor-strategy="akka.actor.StoppingSupervisorStrategy"
       |akka.persistence.internal-stash-overflow-strategy = "%s"
       |""".stripMargin

  val throwConfig = String.format(
      templateConfig, "akka.persistence.ThrowExceptionConfigurator")
  val discardConfig =
    String.format(templateConfig, "akka.persistence.DiscardConfigurator")
  val replyToConfig = String.format(
      templateConfig,
      "akka.persistence.PersistentActorBoundedStashingSpec$ReplyToWithRejectConfigurator")
}

class SteppingInMemPersistentActorBoundedStashingSpec(strategyConfig: String)
    extends PersistenceSpec(
        SteppingInmemJournal
          .config("persistence-bounded-stash")
          .withFallback(PersistenceSpec.config(
                  "stepping-inmem",
                  "SteppingInMemPersistentActorBoundedStashingSpec",
                  extraConfig = Some(strategyConfig)))) with BeforeAndAfterEach
    with ImplicitSender {

  override def atStartup: Unit = {
    system.eventStream.publish(Mute(EventFilter.warning(
                pattern = ".*received dead letter from.*Cmd.*")))
  }

  override def beforeEach(): Unit =
    system.eventStream.subscribe(testActor, classOf[DeadLetter])

  override def afterEach(): Unit =
    system.eventStream.unsubscribe(testActor, classOf[DeadLetter])
}

class ThrowExceptionStrategyPersistentActorBoundedStashingSpec
    extends SteppingInMemPersistentActorBoundedStashingSpec(
        PersistentActorBoundedStashingSpec.throwConfig) {
  "Stashing with ThrowOverflowExceptionStrategy in a persistence actor " should {
    "throws stash overflow exception" in {
      val persistentActor =
        namedPersistentActor[StashOverflowStrategyFromConfigPersistentActor]
      awaitAssert(SteppingInmemJournal.getRef("persistence-bounded-stash"),
                  3.seconds)
      val journal = SteppingInmemJournal.getRef("persistence-bounded-stash")

      // initial read highest
      SteppingInmemJournal.step(journal)

      //barrier for stash
      persistentActor ! Cmd("a")

      //internal stash overflow
      1 to (2 * capacity) foreach (persistentActor ! Cmd(_))
      //after PA stopped, all stashed messages forward to deadletters
      1 to capacity foreach
      (i ⇒ expectMsg(DeadLetter(Cmd(i), testActor, persistentActor)))
      //non-stashed messages
      (capacity + 2) to (2 * capacity) foreach
      (i ⇒ expectMsg(DeadLetter(Cmd(i), testActor, persistentActor)))
    }
  }
}

class DiscardStrategyPersistentActorBoundedStashingSpec
    extends SteppingInMemPersistentActorBoundedStashingSpec(
        PersistentActorBoundedStashingSpec.discardConfig) {
  "Stashing with DiscardToDeadLetterStrategy in a persistence actor " should {
    "discard to deadletter" in {
      val persistentActor =
        namedPersistentActor[StashOverflowStrategyFromConfigPersistentActor]
      awaitAssert(SteppingInmemJournal.getRef("persistence-bounded-stash"),
                  3.seconds)
      val journal = SteppingInmemJournal.getRef("persistence-bounded-stash")

      //initial read highest
      SteppingInmemJournal.step(journal)

      //barrier for stash
      persistentActor ! Cmd("a")

      //internal stash overflow after 10
      1 to (2 * capacity) foreach (persistentActor ! Cmd(_))
      //so, 11 to 20 discard to deadletter
      (1 + capacity) to (2 * capacity) foreach
      (i ⇒ expectMsg(DeadLetter(Cmd(i), testActor, persistentActor)))
      //allow "a" and 1 to 10 write complete
      1 to (1 + capacity) foreach (i ⇒ SteppingInmemJournal.step(journal))

      persistentActor ! GetState

      expectMsg("a" :: (1 to capacity).toList ::: Nil)
    }
  }
}

class ReplyToStrategyPersistentActorBoundedStashingSpec
    extends SteppingInMemPersistentActorBoundedStashingSpec(
        PersistentActorBoundedStashingSpec.replyToConfig) {
  "Stashing with DiscardToDeadLetterStrategy in a persistence actor" should {
    "reply to request with custom message" in {
      val persistentActor =
        namedPersistentActor[StashOverflowStrategyFromConfigPersistentActor]
      awaitAssert(SteppingInmemJournal.getRef("persistence-bounded-stash"),
                  3.seconds)
      val journal = SteppingInmemJournal.getRef("persistence-bounded-stash")

      //initial read highest
      SteppingInmemJournal.step(journal)

      //barrier for stash
      persistentActor ! Cmd("a")

      //internal stash overflow after 10
      1 to (2 * capacity) foreach (persistentActor ! Cmd(_))
      //so, 11 to 20 reply to with "Reject" String
      (1 + capacity) to (2 * capacity) foreach (i ⇒ expectMsg("RejectToStash"))
      //allow "a" and 1 to 10 write complete
      1 to (1 + capacity) foreach (i ⇒ SteppingInmemJournal.step(journal))

      persistentActor ! GetState

      expectMsg("a" :: (1 to capacity).toList ::: Nil)
    }
  }
}
