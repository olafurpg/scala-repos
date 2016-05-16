/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.test

import org.specs2.mutable.Specification
import play.api.http.{HttpProtocol, HttpVerbs, Status, HeaderNames}

/**
  * Play specs2 specification.
  *
  * This trait excludes some of the mixins provided in the default specs2 specification that clash with Play helpers
  * methods.  It also mixes in the Play test helpers and types for convenience.
  */
trait PlaySpecification
    extends Specification
    with PlayRunners
    with HeaderNames
    with Status
    with HttpProtocol
    with DefaultAwaitTimeout
    with ResultExtractors
    with Writeables
    with RouteInvokers
    with FutureAwaits
    with HttpVerbs {}
