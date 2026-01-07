package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import cats.syntax.all.*
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.task.Hours
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.{CompletedTask, ScheduledTask, ScheduledTaskError, ScheduledTaskId}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.ScheduledManufacturingError.{
  ManufacturingWithNoTasks,
  TaskError,
  TaskIdNotFound
}

sealed trait ScheduledManufacturing:
  val id: ScheduledManufacturingId
  val code: ManufacturingCode
  val completionDate: DateTime
  val tasks: NonEmptyList[ScheduledTask]
  val dependencies: ManufacturingDependencies

final case class NotStartedManufacturing(
    override val id: ScheduledManufacturingId,
    override val code: ManufacturingCode,
    override val completionDate: DateTime,
    override val tasks: NonEmptyList[ScheduledTask],
    override val dependencies: ManufacturingDependencies
) extends ScheduledManufacturing

final case class InProgressManufacturing(
    override val id: ScheduledManufacturingId,
    override val code: ManufacturingCode,
    override val completionDate: DateTime,
    override val tasks: NonEmptyList[ScheduledTask],
    override val dependencies: ManufacturingDependencies,
    startedAt: DateTime
) extends ScheduledManufacturing

final case class CompletedManufacturing(
    override val id: ScheduledManufacturingId,
    override val code: ManufacturingCode,
    override val completionDate: DateTime,
    override val tasks: NonEmptyList[CompletedTask],
    override val dependencies: ManufacturingDependencies,
    startedAt: DateTime,
    completedAt: DateTime
) extends ScheduledManufacturing

final case class PausedManufacturing(
    override val id: ScheduledManufacturingId,
    override val code: ManufacturingCode,
    override val completionDate: DateTime,
    override val tasks: NonEmptyList[ScheduledTask],
    override val dependencies: ManufacturingDependencies,
    reason: Option[String],
    startedAt: DateTime,
    pausedAt: DateTime
) extends ScheduledManufacturing

object ScheduledManufacturing:
  extension (sm: ScheduledManufacturing)
    /** Calculate the total expected hours for all tasks in the manufacturing.
      * @return
      *   Total expected hours.
      */
    def expectedHours: Hours = sm.tasks.foldMap(_.expectedHours)

    /** Calculate the total remaining hours for all tasks in the manufacturing.
      * @return
      *   Total remaining hours.
      */
    def remainingHours: Hours = sm.tasks.foldMap(_.remainingHours)

    /** Calculate the total completed hours for all tasks in the manufacturing.
      * @return
      *   Total completed hours.
      */
    def completedHours: Hours = sm.tasks.foldMap(_.completedHours)

    /** Add a new task to the manufacturing with specified dependencies.
      * @param task
      *   the task to add.
      * @param dependsOn
      *   the set of task IDs that the new task depends on.
      * @return
      *   the updated [[ScheduledManufacturing]] with the new task added.
      */
    def addTask(task: ScheduledTask, dependsOn: Set[ScheduledTaskId]): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      updateTasks(sm.tasks :+ task, sm.dependencies.setDependency(task.id, dependsOn))

    /** Remove a task from the manufacturing. If removing the task results in no remaining tasks, an error is returned.
      * @param id
      *   the ID of the task to remove.
      * @return
      *   A [[TaskIdNotFound]] error if the task ID does not exist, a [[ManufacturingWithNoTasks]] error if removing the task would leave no tasks,
      *   otherwise the updated [[ScheduledManufacturing]] without the specified task.
      */
    def removeTask(id: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      for
        _ <- ensureTaskExists(id)
        updatedTasks <- sm.tasks.filterNot(_.id == id) match {
          case Nil          => ManufacturingWithNoTasks.asLeft
          case head :: tail => Right(NonEmptyList(head, tail))
        }
        updatedDependencies = sm.dependencies.removeTaskDependency(id)
        newManufacturing <- updateTasks(updatedTasks, updatedDependencies)
      yield newManufacturing

    /** Advance the progress of a task by a specified number of hours. If the task is not found, an error is returned.
      * @param taskId
      *   the ID of the task to advance.
      * @param hours
      *   the number of hours to advance the task's progress.
      * @return
      *   an error if the task ID does not exist, otherwise the updated ScheduledManufacturing with the task's progress advanced.
      */
    def advanceTask(taskId: ScheduledTaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      changeTaskState(taskId, _.advanceInProgressTask(hours))

    /** Rollback the progress of a task by a specified number of hours. If the task is not found, an error is returned.
      * @param taskId
      *   the ID of the task to rollback.
      * @param hours
      *   the number of hours to roll back the task's progress.
      * @return
      *   an error if the task ID does not exist, otherwise the updated ScheduledManufacturing with the task's progress rolled back.
      */
    def rollbackTask(taskId: ScheduledTaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      changeTaskState(taskId, _.rollbackInProgressTask(hours))

    /** Mark a task as completed. If all tasks are completed, the manufacturing transitions to CompletedManufacturing.
      * @param taskId
      *   the ID of the task to complete.
      * @return
      *   A [[TaskIdNotFound]] error if the task ID does not exist, or the updated ScheduledManufacturing with the task marked as completed.
      */
    def completeTask(taskId: ScheduledTaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      changeTaskState(taskId, _.completeTask(hours)).flatMap { updatedManufacturing =>
        if areAllTasksCompleted(updatedManufacturing) then transitionToCompleted(updatedManufacturing)
        else Right(updatedManufacturing)
      }

    /** Revert a completed task back to in-progress state. If the manufacturing was marked as completed, it will transition back to in-progress.
      * @param taskId
      *   The ID of the task to revert.
      * @return
      *   An error if the task ID does not exist or cannot be reverted, otherwise the updated ScheduledManufacturing.
      */
    def revertTaskToInProgress(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      changeTaskState(taskId, _.revertToInProgress)

    private def changeTaskState(
        taskId: ScheduledTaskId,
        f: ScheduledTask => Either[ScheduledTaskError, ScheduledTask]
    ): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      for
        _ <- ensureTaskExists(taskId)
        updatedTasks <- sm.tasks.traverse { task =>
          if task.id == taskId then f(task).leftMap(TaskError.apply)
          else Right(task)
        }
        newManufacturing <- updateTasks(updatedTasks, sm.dependencies)
      yield transitionToInProgressIfNeeded(newManufacturing)

    private def ensureTaskExists(taskId: ScheduledTaskId): Either[ScheduledManufacturingError, Unit] =
      Either.cond(sm.tasks.exists(_.id == taskId), (), TaskIdNotFound(taskId))

    private def transitionToInProgressIfNeeded(manufacturing: ScheduledManufacturing): ScheduledManufacturing =
      manufacturing match
        case NotStartedManufacturing(id, code, cd, tasks, deps) =>
          InProgressManufacturing(id, code, cd, tasks, deps, DateTime.now())
        case other => other
  
    private def areAllTasksCompleted(manufacturing: ScheduledManufacturing): Boolean =
      manufacturing.tasks.forall {
        case CompletedTask(_, _, _, _, _) => true
        case _                            => false
      }
  
    private def getStartedAt(manufacturing: ScheduledManufacturing): DateTime =
      manufacturing match
        case InProgressManufacturing(_, _, _, _, _, startedAt)   => startedAt
        case PausedManufacturing(_, _, _, _, _, _, startedAt, _) => startedAt
        case _                                                   => DateTime.now()
  
    private def transitionToCompleted(manufacturing: ScheduledManufacturing): Either[ScheduledManufacturingError, ScheduledManufacturing] =
      val completedAt = DateTime.now()
      val completedTasks = NonEmptyList.fromListUnsafe(manufacturing.tasks.collect { case ct: CompletedTask => ct })
      Right(
        CompletedManufacturing(
          manufacturing.id,
          manufacturing.code,
          manufacturing.completionDate,
          completedTasks,
          manufacturing.dependencies,
          getStartedAt(manufacturing),
          completedAt
        )
      )
  
    private def updateTasks(
        tasks: NonEmptyList[ScheduledTask],
        deps: ManufacturingDependencies
    ): Either[ScheduledManufacturingError, ScheduledManufacturing] = sm match
      case NotStartedManufacturing(id, code, cd, _, _) =>
        Right(NotStartedManufacturing(id, code, cd, tasks, deps))
      case InProgressManufacturing(id, code, cd, _, _, startedAt) =>
        Right(InProgressManufacturing(id, code, cd, tasks, deps, startedAt))
      case PausedManufacturing(id, code, cd, _, _, reason, startedAt, pausedAt) =>
        Right(PausedManufacturing(id, code, cd, tasks, deps, reason, startedAt, pausedAt))
      case CompletedManufacturing(id, code, cd, _, _, startedAt, _) =>
        // Allow updates when reverting tasks - transition back to InProgressManufacturing
        Right(InProgressManufacturing(id, code, cd, tasks, deps, startedAt))
