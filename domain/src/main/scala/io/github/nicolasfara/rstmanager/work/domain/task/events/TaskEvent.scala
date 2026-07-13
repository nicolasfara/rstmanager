package io.github.nicolasfara.rstmanager.work.domain.task.events

import io.github.nicolasfara.rstmanager.work.domain.task.Task

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.work.domain.task.TaskAggregate]].
 *
 * They form the audit trail of the task catalog lifecycle: creation, full-record updates, and deletion.
 */
enum TaskEvent derives CanEqual:
  /** The task definition was created. */
  case TaskCreated(task: Task)

  /** The task definition was replaced with new data. */
  case TaskUpdated(task: Task)

  /** The task definition was deleted. */
  case TaskDeleted
