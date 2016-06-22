/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.spark.sql.catalyst.plans

import org.apache.spark.sql.catalyst.analysis.UnresolvedAttribute

object JoinType {
  def apply(typ: String): JoinType = typ.toLowerCase.replace("_", "") match {
    case "inner" => Inner
    case "outer" | "full" | "fullouter" => FullOuter
    case "leftouter" | "left" => LeftOuter
    case "rightouter" | "right" => RightOuter
    case "leftsemi" => LeftSemi
    case _ =>
      val supported = Seq("inner",
                          "outer",
                          "full",
                          "fullouter",
                          "leftouter",
                          "left",
                          "rightouter",
                          "right",
                          "leftsemi")

      throw new IllegalArgumentException(
          s"Unsupported join type '$typ'. " +
            "Supported join types include: " +
            supported.mkString("'", "', '", "'") + ".")
  }
}

sealed abstract class JoinType {
  def sql: String
}

case object Inner extends JoinType {
  override def sql: String = "INNER"
}

case object LeftOuter extends JoinType {
  override def sql: String = "LEFT OUTER"
}

case object RightOuter extends JoinType {
  override def sql: String = "RIGHT OUTER"
}

case object FullOuter extends JoinType {
  override def sql: String = "FULL OUTER"
}

case object LeftSemi extends JoinType {
  override def sql: String = "LEFT SEMI"
}

case class NaturalJoin(tpe: JoinType) extends JoinType {
  require(Seq(Inner, LeftOuter, RightOuter, FullOuter).contains(tpe),
          "Unsupported natural join type " + tpe)
  override def sql: String = "NATURAL " + tpe.sql
}

case class UsingJoin(tpe: JoinType, usingColumns: Seq[UnresolvedAttribute])
    extends JoinType {
  require(Seq(Inner, LeftOuter, LeftSemi, RightOuter, FullOuter).contains(tpe),
          "Unsupported using join type " + tpe)
  override def sql: String = "USING " + tpe.sql
}
