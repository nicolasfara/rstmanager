package io.github.nicolasfara.rstmanager.work.domain.task

import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuiteLike

class TaskPriorityTest extends AnyFunSuiteLike:
  test("TaskPriority should have three levels: Low, Medium, High") {
    val priorities = TaskPriority.values
    assert(priorities.length == 3)
    assert(priorities.contains(TaskPriority.Low))
    assert(priorities.contains(TaskPriority.Medium))
    assert(priorities.contains(TaskPriority.High))
  }
  test("TaskPriority ordering should be Low < Medium < High") {
    assert(TaskPriority.Low < TaskPriority.Medium)
    assert(TaskPriority.Medium < TaskPriority.High)
    assert(TaskPriority.Low < TaskPriority.High)
  }
  test("TaskPriority Show instance should return correct string representation") {
    assert(TaskPriority.Low.show == "Low")
    assert(TaskPriority.Medium.show == "Medium")
    assert(TaskPriority.High.show == "High")
  }
