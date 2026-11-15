package io.gitbub.nicolasfara.rstmanager.ui

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the planning-board filters. */
final class PlanningFilterSpec extends AnyFunSuite with Matchers:

  private val keys = PlanningFilter.Keys(employeeId = "emp-1", taskId = "task-1", manufacturingCode = "CAP-01")

  test("the empty filter matches every slice"):
    PlanningFilter.empty.isEmpty shouldBe true
    PlanningFilter.empty.matches(keys) shouldBe true

  test("each filter narrows on its own key"):
    PlanningFilter.empty.copy(employeeId = "emp-1").matches(keys) shouldBe true
    PlanningFilter.empty.copy(employeeId = "emp-2").matches(keys) shouldBe false
    PlanningFilter.empty.copy(taskId = "task-1").matches(keys) shouldBe true
    PlanningFilter.empty.copy(taskId = "task-2").matches(keys) shouldBe false
    PlanningFilter.empty.copy(manufacturingCode = "CAP-01").matches(keys) shouldBe true
    PlanningFilter.empty.copy(manufacturingCode = "CAP-02").matches(keys) shouldBe false

  test("filters combine in AND"):
    PlanningFilter("emp-1", "task-1", "CAP-01").matches(keys) shouldBe true
    PlanningFilter("emp-1", "task-2", "CAP-01").matches(keys) shouldBe false

  test("a filter does not match a slice whose key is not resolved yet"):
    PlanningFilter.empty.copy(taskId = "task-1").matches(keys.copy(taskId = "")) shouldBe false

  test("options list 'any' first, then distinct plan values sorted by label"):
    val labels = Map("b" -> "Bruno", "a" -> "anna", "c" -> "Carla")
    PlanningFilter.options("Tutti", List("c", "a", "b", "a", ""), "", labels(_)) shouldBe
      List("" -> "Tutti", "a" -> "anna", "b" -> "Bruno", "c" -> "Carla")

  test("options keep a selected value that is no longer in the plan"):
    PlanningFilter.options("Tutti", List("a"), "z", v => v.toUpperCase.nn) shouldBe List("" -> "Tutti", "a" -> "A", "z" -> "Z")
end PlanningFilterSpec
