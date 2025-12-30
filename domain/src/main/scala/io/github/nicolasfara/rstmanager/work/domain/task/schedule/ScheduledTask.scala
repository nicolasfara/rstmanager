package io.github.nicolasfara.rstmanager.work.domain.task.schedule

import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, Task, TaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.Percentage.*

import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*

final case class ScheduledTask(
    id: SchedulableTaskId,
    taskId: TaskId,
    priority: OrderPriority,
    expectedHours: Hours,
    completedHours: Hours,
    deadline: DateTime,
    status: TaskStatus
):
  def remainingHours: Hours = expectedHours - completedHours

  def completedPercentage: Percentage = completedHours.value.toPercentage(expectedHours.value)

  /** Mark the task as completed with the given hours worked. This method overrides any previous completed hours and
    * considers [[hoursWorked]] as the total hours worked to complete the task.
    * @param hoursWorked
    *   the total hours worked to complete the task.
    * @return
    *   a new SchedulableTask instance marked as done with the specified completed hours.
    */
  def completeTaskWithHours(hoursWorked: Hours): ScheduledTask =
    copy(completedHours = hoursWorked, status = TaskStatus.Done)

  /** Only marks the task as completed, without altering the completed hours.
    * @return
    *   a new SchedulableTask instance marked as done.
    */
  def completeTask: ScheduledTask = setState(TaskStatus.Done)

  /** Sets the state of the task to the given status.
    * @param status
    *   the new status of the task.
    * @return
    *   a new SchedulableTask instance with the updated status.
    */
  def setState(status: TaskStatus): ScheduledTask = copy(status = status)

  /** Changes the priority of the task to the new priority.
    * @param newPriority
    *   the new priority to set.
    * @return
    *   a new SchedulableTask instance with the updated priority.
    */
  def changePriority(newPriority: OrderPriority): ScheduledTask =
    copy(priority = newPriority)

  def isCompleted: Boolean = status == TaskStatus.Done

object ScheduledTask:
  extension (task: Task)
    def toSchedulable(
        id: SchedulableTaskId,
        deadline: DateTime,
        priority: OrderPriority,
        expectedHours: Hours,
        status: TaskStatus
    ): ScheduledTask =
      ScheduledTask(id, task.id, priority, expectedHours, Hours(0): Hours, deadline, status)
