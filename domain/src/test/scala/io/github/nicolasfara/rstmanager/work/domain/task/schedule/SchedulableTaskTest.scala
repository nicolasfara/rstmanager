package io.github.nicolasfara.rstmanager.work.domain.task.schedule

import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority.Normal
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, TaskId}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

import java.util.UUID

class SchedulableTaskTest extends AnyFlatSpecLike:
  "A SchedulableTask" should "return the remaining hours" in:
    val taskId = TaskId(UUID.randomUUID().nn)
    val requiredHours: Hours = Hours(10)
    val schedulableTask = ScheduledTask(
      id = SchedulableTaskId(UUID.randomUUID().nn),
      taskId = taskId,
      priority = Normal,
      expectedHours = requiredHours,
      completedHours = Hours(4),
      deadline = DateTime.now().nn + 5.days,
      status = TaskStatus.InProgress
    )

    val remainingHours = schedulableTask.remainingHours
    remainingHours shouldEqual (Hours(6): Hours)
  it should "return the completed percentage" in:
    val taskId = TaskId(UUID.randomUUID().nn)
    val requiredHours: Hours = Hours(20)
    val completedHours: Hours = Hours(5)
    val schedulableTask = ScheduledTask(
      id = SchedulableTaskId(UUID.randomUUID().nn),
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
      id = SchedulableTaskId(UUID.randomUUID().nn),
      taskId = taskId,
      priority = Normal,
      expectedHours = Hours(15),
      completedHours = Hours(5),
      deadline = DateTime.now().nn + 7.days,
      status = TaskStatus.InProgress
    )

    val hoursToComplete: Hours = Hours(12)
    val completedTask = schedulableTask.completeTaskWithHours(hoursToComplete)
    completedTask.status shouldEqual TaskStatus.Done
    completedTask.completedHours shouldEqual hoursToComplete
