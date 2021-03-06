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
package com.precog.quirrel

import com.precog.util.{BitSet, BitSetUtil}

import scalaz.Tree

trait Tracer extends parser.AST with typer.Binder {
  import ast._
  import Stream.{empty => SNil}

  private def addNode(trace: Trace,
                      sigma: Sigma,
                      expr: Expr,
                      parentIdx: Option[Int]): Trace = {
    val copied =
      if (trace.nodes.contains((sigma, expr))) {
        trace
      } else {
        Trace.safeCopy(trace, (sigma, expr), BitSetUtil.create())
      }

    parentIdx match {
      case Some(idx) => {
          copied.indices(idx) set copied.nodes.indexOf((sigma, expr))
          copied
        }
      case None => copied
    }
  }

  override def buildTrace(sigma: Sigma)(expr: Expr): Trace = {

    def foldThrough(trace: Trace,
                    sigma: Sigma,
                    expr: Expr,
                    parentIdx: Option[Int],
                    exprs: Vector[Expr]): Trace = {

      val updated = addNode(trace, sigma, expr, parentIdx)
      val idx = updated.nodes.indexOf((sigma, expr))

      var i = 0
      var traceAcc = updated
      while (i < exprs.length) {
        traceAcc = loop(sigma, traceAcc, exprs(i), Some(idx))
        i += 1
      }
      traceAcc
    }

    def loop(sigma: Sigma,
             trace: Trace,
             expr: Expr,
             parentIdx: Option[Int]): Trace = expr match {

      case Let(_, _, _, _, right) =>
        loop(sigma, trace, right, parentIdx)

      case Solve(_, constraints, child) =>
        foldThrough(trace, sigma, expr, parentIdx, constraints :+ child)

      case Assert(_, pred, child) =>
        foldThrough(trace, sigma, expr, parentIdx, Vector(pred, child))

      case Observe(_, data, samples) =>
        foldThrough(trace, sigma, expr, parentIdx, Vector(data, samples))

      case New(_, child) =>
        foldThrough(trace, sigma, expr, parentIdx, Vector(child))

      case Relate(_, from, to, in) =>
        foldThrough(trace, sigma, expr, parentIdx, Vector(from, to, in))

      case (_: TicVar) =>
        addNode(trace, sigma, expr, parentIdx)

      case expr @ Dispatch(_, name, actuals) => {
          expr.binding match {
            case LetBinding(let) => {
                val ids = let.params map { Identifier(Vector(), _) }
                val sigma2 =
                  sigma ++ (ids zip Stream.continually(let) zip actuals)

                if (actuals.length > 0) {
                  val updated = addNode(trace, sigma, expr, parentIdx)
                  val idx = updated.nodes.indexOf((sigma, expr))

                  loop(sigma2, updated, let.left, Some(idx))
                } else {
                  loop(sigma2, trace, let.left, parentIdx)
                }
              }

            case FormalBinding(let) =>
              loop(sigma, trace, sigma((name, let)), parentIdx)

            case _ =>
              foldThrough(trace, sigma, expr, parentIdx, actuals)
          }
        }

      case NaryOp(_, values) =>
        foldThrough(trace, sigma, expr, parentIdx, values)
    }

    loop(sigma, Trace.empty, expr, None)
  }

  /**
    * Returns a set of backtraces, where each backtrace is a stack of expressions
    * and associated actual context.
    */
  def buildBacktrace(trace: Trace)(target0: Expr): List[List[(Sigma, Expr)]] = {
    val targetLocations: Array[Int] =
      trace.nodes.zipWithIndex collect {
        case ((_, expr), idx) if target0 == expr => idx
      }

    def loop(stack: List[(Sigma, Expr)])(location: Int): List[(Sigma, Expr)] = {
      val newTargets: Array[(Int, (Sigma, Expr))] = {
        trace.indices.zipWithIndex collect {
          case (bitset, idx) if bitset(location) => (idx, trace.nodes(idx))
        }
      }

      val result =
        newTargets flatMap {
          case (idx, grph) =>
            loop(stack :+ grph)(idx)
        }

      if (result.isEmpty) stack
      else result.toList
    }

    targetLocations map { loop(Nil) } toList
  }
}
