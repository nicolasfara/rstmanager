package io.github.nicolasfara.rstmanager.work.domain.task.scheduled

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority.Normal
import io.github.nicolasfara.rstmanager.work.domain.task.{TaskHours, TaskId}

import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class SchedulableTaskTest extends AnyFlatSpecLike:
  "A SchedulableTask" should "return the remaining hours" in:
    val taskId = TaskId(UUID.randomUUID().nn)
    val requiredHours: TaskHours = TaskHours(10)
    val schedulableTask = ScheduledTask(
      id = ScheduledTaskId(UUID.randomUUID().nn),
      taskId = taskId,
      priority = Normal,
      expectedHours = requiredHours,
      completedHours = TaskHours(4),
      deadline = DateTime.now().nn + 5.days,
      status = TaskStatus.InProgress
    )

    val remainingHours = schedulableTask.remainingHours
    remainingHours shouldEqual (TaskHours(6): TaskHours)
  it should "return the completed percentage" in:
    val taskId = TaskId(UUID.randomUUID().nn)
    val requiredHours: TaskHours = TaskHours(20)
    val completedHours: TaskHours = TaskHours(5)
    val schedulableTask = ScheduledTask(
      id = ScheduledTaskId(UUID.randomUUID().nn),
      taskId = taskId,
      priority = Normal,
      expectedHours = requiredHours,
      completedHours = completedHours,
      deadline = DateTime.now().nn + 10.days,
      status = TaskStatus.InProgress
    )
    val completedPercentage = schedulableTask.completedPercentage
    completedPercentage shouldEqual (Percentage(25): Percentage)
  it should "mark the task as completed with given hours" in:
    val taskId = TaskId(UUID.randomUUID().nn)
    val schedulableTask = ScheduledTask(
      id = ScheduledTaskId(UUID.randomUUID().nn),
      taskId = taskId,
      priority = Normal,
      expectedHours = TaskHours(15),
      completedHours = TaskHours(5),
      deadline = DateTime.now().nn + 7.days,
      status = TaskStatus.InProgress
    )

    val hoursToComplete: TaskHours = TaskHours(12)
    val completedTask = schedulableTask.completeTaskWithHours(hoursToComplete)
    completedTask.status shouldEqual TaskStatus.Done
    completedTask.completedHours shouldEqual hoursToComplete
