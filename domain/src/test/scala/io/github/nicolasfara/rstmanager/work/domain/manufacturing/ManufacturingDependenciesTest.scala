package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.task.TaskId
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

import java.util.UUID

class ManufacturingDependenciesTest extends AnyFlatSpecLike:
  "ManufacturingDependencies" should "detect cycles in dependencies" in:
    val taskA = TaskId(UUID.randomUUID().nn)
    val taskB = TaskId(UUID.randomUUID().nn)
    val taskC = TaskId(UUID.randomUUID().nn)

    val dependencyWithSimpleCycle = Map(
      taskA -> Set(taskB),
      taskB -> Set(taskA),
    )

    val dependenciesWithCycle = Map(
      taskA -> Set(taskB),
      taskB -> Set(taskC),
      taskC -> Set(taskA) // Cycle here
    )

    val dependenciesWithoutCycle = Map(
      taskA -> Set(taskB),
      taskB -> Set(taskC)
    )

    ManufacturingDependencies.containsCycles(dependencyWithSimpleCycle) shouldBe true
    ManufacturingDependencies.containsCycles(dependenciesWithCycle) shouldBe true
    ManufacturingDependencies.containsCycles(dependenciesWithoutCycle) shouldBe false
  it should "perform topological sort on dependencies" in:
    val taskA = TaskId(UUID.randomUUID().nn)
    val taskB = TaskId(UUID.randomUUID().nn)
    val taskC = TaskId(UUID.randomUUID().nn)
    val taskD = TaskId(UUID.randomUUID().nn)

    val dependenciesWithCycle = Map(
      taskA -> Set(taskB),
      taskB -> Set(taskC),
      taskC -> Set(taskA) // Cycle here
    )

    val dependenciesWithoutCycle = Map(
      taskA -> Set(taskB, taskC),
      taskB -> Set(taskD),
      taskC -> Set(taskD)
    )

    ManufacturingDependencies.topologicalSort(ManufacturingDependencies(dependenciesWithCycle)) shouldBe Left(
      "The dependency graph contains cycles."
    )

    val sortedResult = ManufacturingDependencies.topologicalSort(ManufacturingDependencies(dependenciesWithoutCycle))
    sortedResult.isRight shouldBe true
    sortedResult shouldBe Right(List(taskD, taskB, taskC, taskA))

