/*
 *  ____    ____    _____    ____    ___     ____
 * |  _ \  |  _ \  | ____|  / ___|  / _/    / ___|        Precog (R)
 * | |_) | | |_) | |  _|   | |     | |  /| | |  _         Advanced Analytics Engine for NoSQL Data
 * |  __/  |  _ <  | |___  | |___  |/ _| | | |_| |        Copyright (C) 2010 - 2013 SlamData, Inc.
 * |_|     |_| \_\ |_____|  \____|   /__/   \____|        All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the
 * GNU Affero General Public License as published by the Free Software Foundation, either version
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
 * the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <http://www.gnu.org/licenses/>.
 *
 */
package com.precog
package ragnarok

import scalaz._

import akka.util.Duration
// Can no longer depend on Muspelheim test.

//import muspelheim.RawJsonColumnarTableStorageModule
//final class JsonPerfTestRunner[M[+_], T](val timer: Timer[T], _optimize: Boolean,
//_userUID: String)(implicit val M: Monad[M], val coM: Copointed[M])
//extends EvaluatingPerfTestRunner[M, T]
//with RawJsonColumnarTableStorageModule[M] {

//type YggConfig = PerfTestRunnerConfig
//object yggConfig extends EvaluatingPerfTestRunnerConfig {
//val userUID = _userUID
//val optimize = _optimize
//}

//def startup() = ()
//def shutdown() = ()
//}
