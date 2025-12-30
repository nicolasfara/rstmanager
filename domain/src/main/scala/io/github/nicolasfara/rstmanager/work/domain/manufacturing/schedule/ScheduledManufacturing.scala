package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import io.github.iltotore.iron.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.ScheduledManufacturingError.TaskIdNotFound
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, TaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTask
import org.scalactic.anyvals.NonEmptySet

final case class ScheduledManufacturing(
                                         id: ScheduledManufacturingId,
                                         manufacturingCode: ManufacturingCode,
                                         priority: OrderPriority,
                                         expectedCompletionDate: DateTime,
                                         dueDate: DateTime,
                                         tasks: NonEmptySet[ScheduledTask],
                                         dependencies: ManufacturingDependencies,
                                         status: ManufacturingStatus
):

  def totalEstimatedHours: Hours = tasks.foldLeft[Hours](Hours(0))(_ + _.expectedHours)

  def totalCompletedHours: Hours = tasks.foldLeft[Hours](Hours(0))(_ + _.completedHours)

  def addTask(task: ScheduledTask, dependsOn: Set[TaskId]): ScheduledManufacturing =
    val updatedTasks = tasks + task
    val updatedDependencies = dependencies.addTaskDependency(task.taskId, dependsOn)
    copy(tasks = updatedTasks, dependencies = updatedDependencies)

  def removeTask(taskId: TaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    val filtered = tasks.filterNot(_.taskId == taskId).toList
    filtered match {
      case head :: tail => Right(copy(tasks = NonEmptySet(head, tail: _*)))
      case Nil          => Left(ScheduledManufacturingError.ManufacturingWithNoTasks)
    }

  def updateTaskProgress(taskId: TaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    doForTask(taskId)(_.completeTaskWithHours(hours))

  def completeTask(taskId: TaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    doForTask(taskId)(_.completeTask)

  private def updateTask(updatedTask: ScheduledTask): ScheduledManufacturing =
    val updatedTasks = tasks.map { task =>
      if task.taskId == updatedTask.taskId then updatedTask else task
    }
    if updatedTasks.forall(_.isCompleted) then copy(tasks = updatedTasks, status = ManufacturingStatus.Done)
    else copy(tasks = updatedTasks)

  private def doForTask(
      taskId: TaskId
  )(f: ScheduledTask => ScheduledTask): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    tasks
      .find(_.taskId == taskId)
      .toRight(TaskIdNotFound(taskId))
      .map { taskToUpdate =>
        val updatedTask = f(taskToUpdate)
        updateTask(updatedTask)
      }
