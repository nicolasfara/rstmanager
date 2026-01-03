package io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ManufacturingCode, ManufacturingDependencies}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.schedule.ScheduledManufacturingError.TaskIdNotFound
import io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority
import io.github.nicolasfara.rstmanager.work.domain.task.{Hours, TaskId}
import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTask

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*

/** Entity representing a scheduled manufacturing within an Order aggregate.
  *
  * This entity manages the execution of a manufacturing process with its tasks, dependencies, and status tracking. It
  * is not an aggregate root but an entity within the Order aggregate boundary.
  */
final case class ScheduledManufacturing(
    id: ScheduledManufacturingId,
    manufacturingCode: ManufacturingCode,
    priority: OrderPriority,
    expectedCompletionDate: DateTime,
    dueDate: DateTime,
    tasks: NonEmptyList[ScheduledTask],
    dependencies: ManufacturingDependencies,
    status: ManufacturingStatus
):

  /** Calculate total estimated hours across all tasks */
  def totalEstimatedHours: Hours = tasks.foldLeft[Hours](Hours(0))(_ + _.expectedHours)

  /** Calculate total completed hours across all tasks */
  def totalCompletedHours: Hours = tasks.foldLeft[Hours](Hours(0))(_ + _.completedHours)

  /** Check if all tasks are completed */
  def isCompleted: Boolean = status == ManufacturingStatus.Done

  /** Get completion percentage (0-100) */
  def completionPercentage: Double =
    val total = totalEstimatedHours.value
    val completed = totalCompletedHours.value
    if total > 0 then (completed.toDouble / total.toDouble) * 100.0 else 0.0

  /** Add a new task to the manufacturing with its dependencies.
    *
    * @param task
    *   the task to add
    * @param dependsOn
    *   set of task IDs this task depends on
    * @return
    *   updated manufacturing with the new task
    */
  def addTask(task: ScheduledTask, dependsOn: Set[TaskId]): ScheduledManufacturing =
    val updatedTasks = tasks :+ task
    val updatedDependencies = dependencies.addTaskDependency(task.taskId, dependsOn)
    copy(tasks = updatedTasks, dependencies = updatedDependencies)

  /** Remove a task from the manufacturing.
    *
    * @param taskId
    *   the ID of the task to remove
    * @return
    *   Either an error if removal would leave no tasks, or the updated manufacturing
    */
  def removeTask(taskId: TaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    val filtered = tasks.filterNot(_.taskId == taskId)
    filtered match
      case head :: tail =>
        val updatedDependencies = dependencies.removeTaskDependency(taskId)
        Right(copy(tasks = NonEmptyList(head, tail), dependencies = updatedDependencies))
      case Nil =>
        Left(ScheduledManufacturingError.ManufacturingWithNoTasks)

  /** Update task progress with hours worked.
    *
    * @param taskId
    *   the ID of the task to update
    * @param hours
    *   the hours worked
    * @return
    *   Either an error if task not found, or the updated manufacturing
    */
  def updateTaskProgress(taskId: TaskId, hours: Hours): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    doForTask(taskId)(_.completeTaskWithHours(hours))

  /** Mark a task as completed.
    *
    * @param taskId
    *   the ID of the task to complete
    * @return
    *   Either an error if task not found, or the updated manufacturing
    */
  def completeTask(taskId: TaskId): Either[ScheduledManufacturingError, ScheduledManufacturing] =
    doForTask(taskId)(_.completeTask)

  /** Change the priority of the manufacturing.
    *
    * @param newPriority
    *   the new priority level
    * @return
    *   updated manufacturing with new priority
    */
  def changePriority(newPriority: OrderPriority): ScheduledManufacturing =
    val updatedTasks = tasks.map(_.changePriority(newPriority))
    copy(priority = newPriority, tasks = updatedTasks)

  /** Update a single task and recalculate manufacturing status.
    *
    * @param updatedTask
    *   the task with updated state
    * @return
    *   manufacturing with updated task and potentially updated status
    */
  private def updateTask(updatedTask: ScheduledTask): ScheduledManufacturing =
    val updatedTasks = tasks.map { task =>
      if task.taskId == updatedTask.taskId then updatedTask else task
    }
    val newStatus =
      if updatedTasks.forall(_.isCompleted) then ManufacturingStatus.Done
      else if updatedTasks.exists(t => t.isInProgress || t.isCompleted) then ManufacturingStatus.InProgress
      else ManufacturingStatus.NotStarted
    copy(tasks = updatedTasks, status = newStatus)

  /** Execute an operation on a specific task.
    *
    * @param taskId
    *   the ID of the task to operate on
    * @param f
    *   the function to apply to the task
    * @return
    *   Either an error if task not found, or the updated manufacturing
    */
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

object ScheduledManufacturing:
  /** Create a new scheduled manufacturing with initial tasks.
    *
    * @param id
    *   unique identifier
    * @param manufacturingCode
    *   the manufacturing code reference
    * @param priority
    *   order priority
    * @param expectedCompletionDate
    *   when the manufacturing should be done
    * @param dueDate
    *   deadline for the manufacturing
    * @param initialTask
    *   the first task (at least one required)
    * @param dependencies
    *   manufacturing dependencies
    * @return
    *   new ScheduledManufacturing instance
    */
  def create(
      id: ScheduledManufacturingId,
      manufacturingCode: ManufacturingCode,
      priority: OrderPriority,
      expectedCompletionDate: DateTime,
      dueDate: DateTime,
      initialTask: ScheduledTask,
      dependencies: ManufacturingDependencies
  ): ScheduledManufacturing =
    ScheduledManufacturing(
      id = id,
      manufacturingCode = manufacturingCode,
      priority = priority,
      expectedCompletionDate = expectedCompletionDate,
      dueDate = dueDate,
      tasks = NonEmptyList.one(initialTask),
      dependencies = dependencies,
      status = ManufacturingStatus.NotStarted
    )
