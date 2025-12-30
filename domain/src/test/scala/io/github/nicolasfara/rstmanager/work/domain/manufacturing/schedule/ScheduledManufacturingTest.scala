package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, TaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.{SchedulableTaskId, ScheduledTask, TaskStatus}
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

import java.util.UUID

class ScheduledManufacturingTest extends AnyFlatSpecLike:
  
  private def createScheduledTask(
      taskId: TaskId,
      expectedHours: Int,
      completedHours: Int,
      status: TaskStatus
  ): ScheduledTask =
    ScheduledTask(
      id = SchedulableTaskId(UUID.randomUUID().nn),
      taskId = taskId,
      priority = OrderPriority.Normal,
      expectedHours = Hours(expectedHours).getOrElse(fail("Invalid hours")),
      completedHours = Hours(completedHours).getOrElse(fail("Invalid hours")),
      deadline = DateTime.now().nn + 7.days,
      status = status
    )

  private def createScheduledManufacturing(
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies,
      status: ManufacturingStatus
  ): ScheduledManufacturing =
    ScheduledManufacturing(
      id = ScheduledManufacturingId(UUID.randomUUID().nn),
      manufacturingCode = ManufacturingCode("MFG-001").getOrElse(fail("Invalid manufacturing code")),
      priority = OrderPriority.Normal,
      expectedCompletionDate = DateTime.now().nn + 30.days,
      dueDate = DateTime.now().nn + 30.days,
      tasks = tasks,
      dependencies = dependencies,
      status = status
    )

  "ScheduledManufacturing" should "calculate total estimated hours correctly" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 0, TaskStatus.NotStarted)
    val task2 = createScheduledTask(TaskId(UUID.randomUUID().nn), 20, 0, TaskStatus.NotStarted)
    val task3 = createScheduledTask(TaskId(UUID.randomUUID().nn), 15, 0, TaskStatus.NotStarted)
    
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1, task2, task3),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    manufacturing.totalEstimatedHours shouldBe Hours(45).getOrElse(fail("Invalid hours"))

  it should "calculate total completed hours correctly" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 5, TaskStatus.InProgress)
    val task2 = createScheduledTask(TaskId(UUID.randomUUID().nn), 20, 20, TaskStatus.Done)
    val task3 = createScheduledTask(TaskId(UUID.randomUUID().nn), 15, 10, TaskStatus.InProgress)
    
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1, task2, task3),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.InProgress
    )
    
    manufacturing.totalCompletedHours shouldBe Hours(35).getOrElse(fail("Invalid hours"))

  it should "add a new task without dependencies" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 0, TaskStatus.NotStarted)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    val newTask = createScheduledTask(TaskId(UUID.randomUUID().nn), 15, 0, TaskStatus.NotStarted)
    val updatedManufacturing = manufacturing.addTask(newTask, Set.empty)
    
    updatedManufacturing.tasks.size shouldBe 2
    updatedManufacturing.tasks.toList should contain(newTask)

  it should "add a new task with dependencies" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 0, TaskStatus.NotStarted)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    val newTask = createScheduledTask(TaskId(UUID.randomUUID().nn), 15, 0, TaskStatus.NotStarted)
    val updatedManufacturing = manufacturing.addTask(newTask, Set(task1.taskId))
    
    updatedManufacturing.tasks.size shouldBe 2
    updatedManufacturing.tasks.toList should contain(newTask)

  it should "remove a task successfully when multiple tasks exist" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 0, TaskStatus.NotStarted)
    val task2 = createScheduledTask(TaskId(UUID.randomUUID().nn), 20, 0, TaskStatus.NotStarted)
    val task3 = createScheduledTask(TaskId(UUID.randomUUID().nn), 15, 0, TaskStatus.NotStarted)
    
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1, task2, task3),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    val result = manufacturing.removeTask(task2.taskId)
    
    result.isRight shouldBe true
    val updatedManufacturing = result.getOrElse(fail("Failed to remove task"))
    updatedManufacturing.tasks.size shouldBe 2
    updatedManufacturing.tasks.toList should not contain task2

  it should "fail to remove a task when it's the only one" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 0, TaskStatus.NotStarted)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    val result = manufacturing.removeTask(task1.taskId)
    
    result.isLeft shouldBe true
    result.left.toOption should contain(ScheduledManufacturingError.ManufacturingWithNoTasks)

  it should "update task progress successfully" in:
    val taskId = TaskId(UUID.randomUUID().nn)
    val task1 = createScheduledTask(taskId, 10, 0, TaskStatus.NotStarted)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    val result = manufacturing.updateTaskProgress(taskId, Hours(5).getOrElse(fail("Invalid hours")))
    
    result.isRight shouldBe true
    val updatedManufacturing = result.getOrElse(fail("Failed to update task progress"))
    val updatedTask = updatedManufacturing.tasks.find(_.taskId == taskId).getOrElse(fail("Task not found"))
    updatedTask.completedHours shouldBe Hours(5).getOrElse(fail("Invalid hours"))
    updatedTask.status shouldBe TaskStatus.Done

  it should "fail to update task progress for non-existent task" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 0, TaskStatus.NotStarted)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    val nonExistentTaskId = TaskId(UUID.randomUUID().nn)
    val result = manufacturing.updateTaskProgress(nonExistentTaskId, Hours(5).getOrElse(fail("Invalid hours")))
    
    result.isLeft shouldBe true
    result.left.toOption match
      case Some(ScheduledManufacturingError.TaskIdNotFound(id)) => id shouldBe nonExistentTaskId
      case _                                                     => fail("Expected TaskIdNotFound error")

  it should "complete a task successfully" in:
    val taskId = TaskId(UUID.randomUUID().nn)
    val task1 = createScheduledTask(taskId, 10, 0, TaskStatus.InProgress)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.InProgress
    )
    
    val result = manufacturing.completeTask(taskId)
    
    result.isRight shouldBe true
    val updatedManufacturing = result.getOrElse(fail("Failed to complete task"))
    val updatedTask = updatedManufacturing.tasks.find(_.taskId == taskId).getOrElse(fail("Task not found"))
    updatedTask.status shouldBe TaskStatus.Done

  it should "fail to complete a non-existent task" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 0, TaskStatus.NotStarted)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.NotStarted
    )
    
    val nonExistentTaskId = TaskId(UUID.randomUUID().nn)
    val result = manufacturing.completeTask(nonExistentTaskId)
    
    result.isLeft shouldBe true
    result.left.toOption match
      case Some(ScheduledManufacturingError.TaskIdNotFound(id)) => id shouldBe nonExistentTaskId
      case _                                                     => fail("Expected TaskIdNotFound error")

  it should "transition to Done status when all tasks are completed" in:
    val taskId1 = TaskId(UUID.randomUUID().nn)
    val taskId2 = TaskId(UUID.randomUUID().nn)
    val task1 = createScheduledTask(taskId1, 10, 0, TaskStatus.Done)
    val task2 = createScheduledTask(taskId2, 20, 0, TaskStatus.InProgress)
    
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1, task2),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.InProgress
    )
    
    val result = manufacturing.completeTask(taskId2)
    
    result.isRight shouldBe true
    val updatedManufacturing = result.getOrElse(fail("Failed to complete task"))
    assert(updatedManufacturing.status == ManufacturingStatus.Done)
    updatedManufacturing.tasks.forall(_.isCompleted) shouldBe true

  it should "not transition to Done status when some tasks are not completed" in:
    val taskId1 = TaskId(UUID.randomUUID().nn)
    val taskId2 = TaskId(UUID.randomUUID().nn)
    val task1 = createScheduledTask(taskId1, 10, 0, TaskStatus.NotStarted)
    val task2 = createScheduledTask(taskId2, 20, 0, TaskStatus.InProgress)
    
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1, task2),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.InProgress
    )
    
    val result = manufacturing.completeTask(taskId2)
    
    result.isRight shouldBe true
    val updatedManufacturing = result.getOrElse(fail("Failed to complete task"))
    assert(updatedManufacturing.status == ManufacturingStatus.InProgress)

  it should "maintain correct total hours after adding tasks" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 5, TaskStatus.InProgress)
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.InProgress
    )
    
    val newTask = createScheduledTask(TaskId(UUID.randomUUID().nn), 15, 10, TaskStatus.InProgress)
    val updatedManufacturing = manufacturing.addTask(newTask, Set.empty)
    
    updatedManufacturing.totalEstimatedHours shouldBe Hours(25).getOrElse(fail("Invalid hours"))
    updatedManufacturing.totalCompletedHours shouldBe Hours(15).getOrElse(fail("Invalid hours"))

  it should "maintain correct total hours after removing tasks" in:
    val task1 = createScheduledTask(TaskId(UUID.randomUUID().nn), 10, 5, TaskStatus.InProgress)
    val task2 = createScheduledTask(TaskId(UUID.randomUUID().nn), 20, 15, TaskStatus.InProgress)
    val task3 = createScheduledTask(TaskId(UUID.randomUUID().nn), 15, 10, TaskStatus.InProgress)
    
    val manufacturing = createScheduledManufacturing(
      NonEmptyList.of(task1, task2, task3),
      ManufacturingDependencies(Map.empty),
      ManufacturingStatus.InProgress
    )
    
    val result = manufacturing.removeTask(task2.taskId)
    
    result.isRight shouldBe true
    val updatedManufacturing = result.getOrElse(fail("Failed to remove task"))
    updatedManufacturing.totalEstimatedHours shouldBe Hours(25).getOrElse(fail("Invalid hours"))
    updatedManufacturing.totalCompletedHours shouldBe Hours(15).getOrElse(fail("Invalid hours"))

