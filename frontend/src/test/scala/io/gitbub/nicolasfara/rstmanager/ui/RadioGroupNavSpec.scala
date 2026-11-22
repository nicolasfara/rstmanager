package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the keyboard model of the segmented radio group. */
final class RadioGroupNavSpec extends AnyFunSuite with Matchers:

  test("arrows step to the next and previous option, wrapping at both ends"):
    RadioGroupNav.target("ArrowRight", 0, 3) shouldBe Some(1)
    RadioGroupNav.target("ArrowDown", 2, 3) shouldBe Some(0)
    RadioGroupNav.target("ArrowLeft", 0, 3) shouldBe Some(2)
    RadioGroupNav.target("ArrowUp", 2, 3) shouldBe Some(1)

  test("Home and End jump to the first and last option"):
    RadioGroupNav.target("Home", 2, 3) shouldBe Some(0)
    RadioGroupNav.target("End", 0, 3) shouldBe Some(2)

  test("other keys, and a group without options, move nowhere"):
    RadioGroupNav.target("Enter", 1, 3) shouldBe None
    RadioGroupNav.target("Tab", 1, 3) shouldBe None
    RadioGroupNav.target("ArrowRight", 0, 0) shouldBe None
    RadioGroupNav.target("Home", 0, 0) shouldBe None
    RadioGroupNav.target("End", 0, 0) shouldBe None

  test("the Tab stop is the checked option, else the first"):
    RadioGroupNav.tabStop(List("a", "b", "c"), "c") shouldBe 2
    RadioGroupNav.tabStop(List("a", "b", "c"), "") shouldBe 0
    RadioGroupNav.tabStop(Nil, "a") shouldBe -1
end RadioGroupNavSpec
