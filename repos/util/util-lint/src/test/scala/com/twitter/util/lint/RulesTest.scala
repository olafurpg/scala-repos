package com.twitter.util.lint

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RulesTest extends FunSuite

  private var flag = false

  private val maybeRule = Rule.apply(Category.Performance, "R1", "Maybe")
    if (flag) Seq(Issue("welp"))
    else Nil

  private val neverRule = Rule.apply(Category.Performance, "R2", "Good")
    Nil

  private val alwaysRule = Rule.apply(Category.Performance, "R3", "Nope")
    Seq(Issue("lol"))

  test("empty")
    val rs = new RulesImpl()
    assert(rs.iterable.isEmpty)

  test("add")
    val rs = new RulesImpl()
    rs.add(maybeRule)
    rs.add(neverRule)
    rs.add(alwaysRule)
    assert(Set(maybeRule, neverRule, alwaysRule) == rs.iterable.toSet)

  test("add duplicates")
    val rs = new RulesImpl()
    rs.add(maybeRule)
    rs.add(maybeRule)
    assert(Seq(maybeRule, maybeRule) == rs.iterable.toSeq)

  test("evaluation")
    val rs = new RulesImpl()
    rs.add(maybeRule)

    val rule = rs.iterable.iterator.next()
    assert(rule().isEmpty)

    flag = true
    assert(rule().contains(Issue("welp")))
