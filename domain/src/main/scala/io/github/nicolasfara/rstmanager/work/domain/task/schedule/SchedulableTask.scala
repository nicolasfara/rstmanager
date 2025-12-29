package io.github.nicolasfara.rstmanager.work.domain.task.schedule

import io.github.iltotore.iron.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, Task, TaskId}

final case class SchedulableTask(
    id: SchedulableTaskId,
    taskId: TaskId,
    priority: OrderPriority,
    expectedHours: Hours,
    completedHours: Hours,
    deadline: DateTime
):
  def remainingHours: Hours = expectedHours - completedHours

  def completedPercentage: Percentage =
    if expectedHours.value == 0 then Percentage(100)
    else
      val percentage = (completedHours.value / expectedHours.value) * 100
      Percentage(percentage).valueOr(_ => Percentage(0))

object SchedulableTask:
  extension (task: Task)
    def toSchedulable(
        id: SchedulableTaskId,
        deadline: DateTime,
        priority: OrderPriority,
        expectedHours: Hours
    ): SchedulableTask =
      SchedulableTask(id, task.id, priority, expectedHours, Hours(0): Hours, deadline)
