package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.SchedulableTask
import org.scalactic.anyvals.NonEmptySet

final case class SchedulableManufacturing(
    id: SchedulableManufacturingId,
    manufacturingCode: ManufacturingCode,
    priority: OrderPriority,
    expectedCompletionDate: DateTime,
    dueDate: DateTime,
    tasks: NonEmptySet[SchedulableTask],
    dependencies: ManufacturingDependencies
):

  def addTask(task: SchedulableTask, dependsOn: Set[TaskId]): SchedulableManufacturing =
    val updatedTasks = tasks + task
    val updatedDependencies = dependencies.addTaskDependency(task.taskId, dependsOn)
    copy(tasks = updatedTasks, dependencies = updatedDependencies)

  def removeTask(taskId: TaskId): Either[ScheduledManufacturingError, SchedulableManufacturing] =
    val filtered = tasks.filterNot(_.taskId == taskId).toList
    filtered match {
      case head :: tail => Right(copy(tasks = NonEmptySet(head, tail: _*)))
      case Nil          => Left(ScheduledManufacturingError.ManufacturingWithNoTasks)
    }
