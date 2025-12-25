package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import cats.syntax.all.*
import io.github.nicolasfara.rstmanager.work.domain.task.{CompletableTask, CompletableTaskId, Hours}

final case class Manufacturing(
    code: ManufacturingCode,
    name: ManufacturingName,
    description: Option[ManufacturingDescription],
    tasks: List[CompletableTask]
):
  def totalHours: Hours = tasks.foldMap(_.expectedHours)

  def addTask(completableTask: CompletableTask): Manufacturing = copy(tasks = tasks :+ completableTask)

  def removeTask(completableTaskId: CompletableTaskId): Manufacturing =
    copy(tasks = tasks.filterNot(_.completableTaskId == completableTaskId))

  def advanceTask(completableTaskId: CompletableTaskId, hours: Hours): Either[ManufacturingError, Manufacturing] =
    updateTask(completableTaskId)(_.advance(hours))

  def deAdvanceTask(completableTaskId: CompletableTaskId, hours: Hours): Either[ManufacturingError, Manufacturing] =
    updateTask(completableTaskId)(_.deAdvance(hours))

  private def updateTask(completableTaskId: CompletableTaskId)(
      f: CompletableTask => CompletableTask
  ): Either[ManufacturingError, Manufacturing] = tasks
    .find(_.completableTaskId == completableTaskId)
    .toRight(ManufacturingError.TaskNotFound(completableTaskId))
    .map { taskToUpdate =>
      val updatedTask = f(taskToUpdate)
      val updatedTasks = tasks.map { task =>
        if task.completableTaskId == completableTaskId then updatedTask else task
      }
      copy(tasks = updatedTasks)
    }
