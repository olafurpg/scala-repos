/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.it.http.parsing

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl.Source
import play.api.libs.streams.Accumulator

import scala.concurrent.Future

import play.api.libs.iteratee.ExecutionSpecification
import play.api.mvc.{BodyParser, Results, Result}
import play.api.test.{FakeRequest, PlaySpecification}

import org.specs2.ScalaCheck
import org.scalacheck.{Arbitrary, Gen}

object BodyParserSpec
    extends PlaySpecification
    with ExecutionSpecification
    with ScalaCheck {

  def run[A](bodyParser: BodyParser[A]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val system = ActorSystem()
    implicit val mat = ActorMaterializer()(system)
    try {
      await {
        Future {
          bodyParser(FakeRequest())
        }.flatMap {
          _.run(Source.empty)
        }
      }
    } finally {
      system.terminate()
    }
  }

  def constant[A](a: A): BodyParser[A] =
    BodyParser("constant") { request =>
      Accumulator.done(Right(a))
    }

  def simpleResult(s: Result): BodyParser[Any] =
    BodyParser("simple result") { request =>
      Accumulator.done(Left(s))
    }

  implicit val arbResult: Arbitrary[Result] = Arbitrary {
    Gen.oneOf(
        Results.Ok,
        Results.BadRequest,
        Results.NotFound,
        Results.InternalServerError
    )
  }

  /* map and mapM should satisfy the functor laws, namely,
   * preservation of identity and function composition.
   * flatMap and flatMapM should satisfy the monad laws, namely,
   * left and right identity, and associativity.
   * The *M versions are simply lifted to Future.
   * When the given function is right-biased, validate and
   * validateM are equivalent to map and mapM, respectively, so
   * should satisfy the functor laws. When left-biased, the result
   * will override the result of the body parser being validated.
   * All of the functions in question should pass a simple result
   * through.
   */

  "BodyParser.map" should {

    "satisfy functor law 1" in prop { (x: Int) =>
      mustExecute(1) { implicit ec =>
        // one execution from `map`
        run {
          constant(x).map(identity)
        } must beRight(x)
      }
    }

    "satisfy functor law 2" in prop { (x: Int) =>
      val inc = (i: Int) => i + 1
      val dbl = (i: Int) => i * 2
      mustExecute(3) { implicit ec =>
        // three executions from `map`
        run {
          constant(x).map(inc).map(dbl)
        } must_== run {
          constant(x).map(inc andThen dbl)
        }
      }
    }

    "pass through simple result" in prop { (s: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `map`
        run {
          simpleResult(s).map(identity)
        } must beLeft(s)
      }
    }
  }

  "BodyParser.mapM" should {

    "satisfy lifted functor law 1" in prop { (x: Int) =>
      mustExecute(1) { implicit ec =>
        // one execution from `mapM`
        run {
          constant(x).mapM(Future.successful)
        } must beRight(x)
      }
    }

    "satisfy lifted functor law 2" in prop { (x: Int) =>
      val inc = (i: Int) => Future.successful(i + 1)
      val dbl = (i: Int) => Future.successful(i * 2)
      mustExecute(3, 1) { (mapMEC, flatMapEC) =>
        val flatMapPEC = flatMapEC.prepare()
        /* three executions from `BodyParser.mapM`
         * and one from `Future.flatMapM`
         */
        run {
          constant(x).mapM(inc)(mapMEC).mapM(dbl)(mapMEC)
        } must_== run {
          constant(x).mapM { y =>
            inc(y).flatMap(dbl)(flatMapPEC)
          }(mapMEC)
        }
      }
    }

    "pass through simple result" in prop { (s: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `mapM`
        run {
          simpleResult(s).mapM(Future.successful)
        } must beLeft(s)
      }
    }
  }

  "BodyParser.validate" should {

    "satisfy right-biased functor law 1" in prop { (x: Int) =>
      val id = (i: Int) => Right(i)
      mustExecute(1) { implicit ec =>
        // one execution from `validate`
        run {
          constant(x).validate(id)
        } must beRight(x)
      }
    }

    "satisfy right-biased functor law 2" in prop { (x: Int) =>
      val inc = (i: Int) => Right(i + 1)
      val dbl = (i: Int) => Right(i * 2)
      mustExecute(3) { implicit ec =>
        // three executions from `validate`
        run {
          constant(x).validate(inc).validate(dbl)
        } must_== run {
          constant(x).validate { y =>
            inc(y).right.flatMap(dbl)
          }
        }
      }
    }

    "pass through simple result (case 1)" in prop { (s: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `validate`
        run {
          simpleResult(s).validate(Right.apply)
        } must beLeft(s)
      }
    }

    "pass through simple result (case 2)" in prop { (s1: Result, s2: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `validate`
        run {
          simpleResult(s1).validate { _ =>
            Left(s2)
          }
        } must beLeft(s1)
      }
    }

    "fail with simple result" in prop { (s: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `validate`
        run {
          constant(0).validate { _ =>
            Left(s)
          }
        } must beLeft(s)
      }
    }
  }

  "BodyParser.validateM" should {

    "satisfy right-biased, lifted functor law 1" in prop { (x: Int) =>
      val id = (i: Int) => Future.successful(Right(i))
      mustExecute(1) { implicit ec =>
        // one execution from `validateM`
        run {
          constant(x).validateM(id)
        } must beRight(x)
      }
    }

    "satisfy right-biased, lifted functor law 2" in prop { (x: Int) =>
      val inc = (i: Int) => Future.successful(Right(i + 1))
      val dbl = (i: Int) => Future.successful(Right(i * 2))
      mustExecute(3) { implicit ec =>
        // three executions from `validateM`
        run {
          constant(x).validateM(inc).validateM(dbl)
        } must_== run {
          constant(x).validateM { y =>
            Future.successful(Right((y + 1) * 2))
          }
        }
      }
    }

    "pass through simple result (case 1)" in prop { (s: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `validateM`
        run {
          simpleResult(s).validateM { x =>
            Future.successful(Right(x))
          }
        } must beLeft(s)
      }
    }

    "pass through simple result (case 2)" in prop { (s1: Result, s2: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `validateM`
        run {
          simpleResult(s1).validateM { _ =>
            Future.successful(Left(s2))
          }
        } must beLeft(s1)
      }
    }

    "fail with simple result" in prop { (s: Result) =>
      mustExecute(1) { implicit ec =>
        // one execution from `validateM`
        run {
          constant(0).validateM { _ =>
            Future.successful(Left(s))
          }
        } must beLeft(s)
      }
    }
  }
}
