package io.github.nicolasfara.rstmanager.work.domain.task

import cats.Order
import com.github.nscala_time.time.Imports.DateTime

final case class PlannedTask(task: CompletableTask, priority: TaskPriority, deadline: DateTime)
object PlannedTask:
  /** Defines the ordering for PlannedTask based on priority, deadline, and remaining hours.
    */
  given Order[PlannedTask] with
    override def compare(x: PlannedTask, y: PlannedTask): Int =
      val priorityComparison = Order[TaskPriority].compare(x.priority, y.priority)
      if priorityComparison != 0 then priorityComparison
      else
        val deadlineComparison = x.deadline.compareTo(y.deadline)
        if deadlineComparison != 0 then deadlineComparison
        else Order[Hours].compare(x.task.remainingHours, y.task.remainingHours)
