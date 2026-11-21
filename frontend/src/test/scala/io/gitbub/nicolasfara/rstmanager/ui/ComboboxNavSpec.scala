package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the keyboard navigation of the combobox list. */
final class ComboboxNavSpec extends AnyFunSuite with Matchers:

  test("move steps through the options and wraps around at both ends"):
    ComboboxNav.move(0, 1, 3) shouldBe 1
    ComboboxNav.move(2, 1, 3) shouldBe 0
    ComboboxNav.move(0, -1, 3) shouldBe 2
    ComboboxNav.move(1, -1, 3) shouldBe 0

  test("move from no active option starts at the first going down and at the last going up"):
    ComboboxNav.move(-1, 1, 4) shouldBe 0
    ComboboxNav.move(-1, -1, 4) shouldBe 3

  test("move from an index beyond a shrunk list restarts from the ends"):
    ComboboxNav.move(5, 1, 2) shouldBe 0
    ComboboxNav.move(5, -1, 2) shouldBe 1

  test("move with no options leaves nothing active"):
    ComboboxNav.move(0, 1, 0) shouldBe -1
    ComboboxNav.move(-1, -1, 0) shouldBe -1

  test("initial highlights the selected option, else the first, else none"):
    ComboboxNav.initial(List("a", "b", "c"), "b") shouldBe 1
    ComboboxNav.initial(List("a", "b", "c"), "") shouldBe 0
    ComboboxNav.initial(List("a", "b", "c"), "z") shouldBe 0
    ComboboxNav.initial(Nil, "a") shouldBe -1
end ComboboxNavSpec
