package io.github.nicolasfara.rstmanager.work.domain.task

import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

import java.util.UUID

class CompletableTaskTest extends AnyFlatSpecLike:
  private val mockedTaskId = TaskId(UUID.randomUUID())
  "A CompletableTask" should "compute the remaining hours" in:
    val completableTask = CompletableTask(
      completableTaskId = CompletableTaskId(UUID.randomUUID()),
      taskId = mockedTaskId,
      expectedHours = Hours(10),
      completedHours = Hours(5)
    )
    completableTask.remainingHours shouldEqual (Hours(5): Hours)
  it should "advance the completed hours" in:
    val completableTask = CompletableTask(
      completableTaskId = CompletableTaskId(UUID.randomUUID()),
      taskId = mockedTaskId,
      expectedHours = Hours(10),
      completedHours = Hours(5)
    )
    val advancedTask = completableTask.advance(Hours(3))
    advancedTask.completedHours shouldEqual (Hours(8): Hours)
  it should "de-advance the completed hours" in:
    val completableTask = CompletableTask(
      completableTaskId = CompletableTaskId(UUID.randomUUID()),
      taskId = mockedTaskId,
      expectedHours = Hours(10),
      completedHours = Hours(5)
    )
    val deAdvancedTask = completableTask.deAdvance(Hours(2))
    deAdvancedTask.completedHours shouldEqual (Hours(3): Hours)
  it should "update the expected hours" in:
    val completableTask = CompletableTask(
      completableTaskId = CompletableTaskId(UUID.randomUUID()),
      taskId = mockedTaskId,
      expectedHours = Hours(10),
      completedHours = Hours(5)
    )
    val updatedTask = completableTask.updateExpectedHours(Hours(15))
    updatedTask.expectedHours shouldEqual (Hours(15): Hours)
  it should "be an immutable data structure" in:
    val completableTask = CompletableTask(
      completableTaskId = CompletableTaskId(UUID.randomUUID()),
      taskId = mockedTaskId,
      expectedHours = Hours(10),
      completedHours = Hours(5)
    )
    val advancedTask = completableTask.advance(Hours(3))
    completableTask.completedHours shouldEqual (Hours(5): Hours)
    advancedTask.completedHours shouldEqual (Hours(8): Hours)
    advancedTask shouldNot be theSameInstanceAs completableTask 