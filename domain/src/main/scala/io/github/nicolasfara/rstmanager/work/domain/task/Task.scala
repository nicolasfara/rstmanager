package io.github.nicolasfara.rstmanager.work.domain.task

/**
 * Entity that represents a Task in the system.
 */
final case class Task(id: TaskId, name: TaskName, taskDescription: Option[TaskDescription], requiredHours: Hours)
