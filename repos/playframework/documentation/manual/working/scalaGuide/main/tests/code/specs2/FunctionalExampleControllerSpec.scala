/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package scalaguide.tests.specs2

import scalaguide.tests.controllers

import play.api.test._

object FunctionalExampleControllerSpec extends PlaySpecification

  // #scalafunctionaltest-functionalexamplecontrollerspec
  "respond to the index Action" in
    val result = controllers.Application.index()(FakeRequest())

    status(result) must equalTo(OK)
    contentType(result) must beSome("text/plain")
    contentAsString(result) must contain("Hello Bob")
  // #scalafunctionaltest-functionalexamplecontrollerspec
