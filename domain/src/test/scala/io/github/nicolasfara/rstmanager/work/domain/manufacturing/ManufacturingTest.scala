package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.iltotore.iron.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingError.TaskNotFound
import io.github.nicolasfara.rstmanager.work.domain.task.{CompletableTask, CompletableTaskId, Hours, TaskId}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class ManufacturingTest extends AnyFlatSpecLike:
  private val completableTasks = List(
    CompletableTask(
      completableTaskId = CompletableTaskId(java.util.UUID.randomUUID()),
      taskId = TaskId(java.util.UUID.randomUUID()),
      expectedHours = Hours(10),
      completedHours = Hours(5)
    ),
    CompletableTask(
      completableTaskId = CompletableTaskId(java.util.UUID.randomUUID()),
      taskId = TaskId(java.util.UUID.randomUUID()),
      expectedHours = Hours(20),
      completedHours = Hours(10)
    )
  )
  "A Manufacturing" should "compute the total expected hours" in:
    val manufacturing = Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = Some(ManufacturingDescription("This is a test manufacturing"): ManufacturingDescription),
      tasks = completableTasks
    )
    manufacturing.totalHours shouldEqual (Hours(30): Hours)
  it should "add a task" in:
    val manufacturing = Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = Some(ManufacturingDescription("This is a test manufacturing"): ManufacturingDescription),
      tasks = completableTasks
    )
    val newTask = CompletableTask(
      completableTaskId = CompletableTaskId(java.util.UUID.randomUUID()),
      taskId = TaskId(java.util.UUID.randomUUID()),
      expectedHours = Hours(15),
      completedHours = Hours(0)
    )
    val updatedManufacturing = manufacturing.addTask(newTask)
    updatedManufacturing.tasks should contain theSameElementsAs (completableTasks :+ newTask)
  it should "remove a task" in:
    val manufacturing = Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = Some(ManufacturingDescription("This is a test manufacturing"): ManufacturingDescription),
      tasks = completableTasks
    )
    val taskToRemove = completableTasks.head
    val updatedManufacturing = manufacturing.removeTask(taskToRemove.completableTaskId)
    updatedManufacturing.tasks should contain theSameElementsAs completableTasks.tail
  it should "advance a task's completed hours" in:
    val manufacturing = Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = Some(ManufacturingDescription("This is a test manufacturing"): ManufacturingDescription),
      tasks = completableTasks
    )
    val taskToAdvance = completableTasks.head
    val hoursToAdvance: Hours = Hours(3)
    val updatedManufacturingEither = manufacturing.advanceTask(taskToAdvance.completableTaskId, hoursToAdvance)
    updatedManufacturingEither match
      case Right(updatedManufacturing) =>
        val updatedTask = updatedManufacturing.tasks.find(_.completableTaskId == taskToAdvance.completableTaskId).get
        updatedTask.completedHours shouldEqual (taskToAdvance.completedHours + hoursToAdvance)
      case Left(_) => fail("Failed to advance task")
  it should "de-advance a task's completed hours" in:
    val manufacturing = Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = Some(ManufacturingDescription("This is a test manufacturing"): ManufacturingDescription),
      tasks = completableTasks
    )
    val taskToDeAdvance = completableTasks.head
    val hoursToDeAdvance: Hours = Hours(2)
    val updatedManufacturingEither = manufacturing.deAdvanceTask(taskToDeAdvance.completableTaskId, hoursToDeAdvance)
    updatedManufacturingEither match
      case Right(updatedManufacturing) =>
        val updatedTask =
          updatedManufacturing.tasks.find(_.completableTaskId == taskToDeAdvance.completableTaskId).get
        updatedTask.completedHours shouldEqual (taskToDeAdvance.completedHours - hoursToDeAdvance)
      case Left(_) => fail("Failed to de-advance task")
  it should "return an error when advancing a non-existent task" in:
    val manufacturing = Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = Some(ManufacturingDescription("This is a test manufacturing"): ManufacturingDescription),
      tasks = completableTasks
    )
    val nonExistentTaskId = CompletableTaskId(java.util.UUID.randomUUID())
    val hoursToAdvance: Hours = Hours(3)
    val result = manufacturing.advanceTask(nonExistentTaskId, hoursToAdvance)
    result.isLeft shouldBe true
    result.left.getOrElse(fail("Not supposed to be here")) shouldBe TaskNotFound(nonExistentTaskId)
  it should "return an error when de-advancing a non-existent task" in:
    val manufacturing = Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = Some(ManufacturingDescription("This is a test manufacturing"): ManufacturingDescription),
      tasks = completableTasks
    )
    val nonExistentTaskId = CompletableTaskId(java.util.UUID.randomUUID())
    val hoursToDeAdvance: Hours = Hours(2)
    val result = manufacturing.deAdvanceTask(nonExistentTaskId, hoursToDeAdvance)
    result.isLeft shouldBe true
    result.left.getOrElse(fail("Not supposed to be here")) shouldBe TaskNotFound(nonExistentTaskId)
