package io.github.nicolasfara.rstmanager.work.domain.task

import io.github.nicolasfara.rstmanager.work.domain.task.TaskAggregate.*
import io.github.nicolasfara.rstmanager.work.domain.task.TaskError.*
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent.*

import cats.data.ValidatedNec
import cats.syntax.all.*
import edomata.core.*
import edomata.syntax.all.*

/**
 * Event-sourced aggregate holding one task catalog definition.
 *
 * Lifecycle:
 *   - [[TaskAggregate.Empty]] before the definition exists.
 *   - [[TaskAggregate.Active]] holds the current [[Task]].
 *   - [[TaskAggregate.Deleted]] keeps the last value for audit while rejecting further changes.
 */
enum TaskAggregate derives CanEqual:
  case Empty
  case Active(task: Task)
  case Deleted(task: Task)

  /** Creates the definition from the empty state. */
  def create(task: Task): Decision[TaskError, TaskEvent, TaskAggregate] = this.decide {
    case Empty => Decision.accept(TaskCreated(task))
    case _ => Decision.reject(TaskAlreadyExists)
  }.validate(_.mustBeActive)

  /** Replaces the definition data when it is active. */
  def update(task: Task): Decision[TaskError, TaskEvent, TaskAggregate] = this.decide {
    case Active(_) => Decision.accept(TaskUpdated(task))
    case _ => Decision.reject(TaskNotFound)
  }.validate(_.mustBeActive)

  /** Deletes the definition when it is active. */
  def delete: Decision[TaskError, TaskEvent, TaskAggregate] = this.decide {
    case Active(_) => Decision.accept(TaskDeleted)
    case _ => Decision.reject(TaskNotFound)
  }.validate(_.mustBeDeleted)

  private def mustBeActive: ValidatedNec[TaskError, Active] = this match
    case active: Active => active.validNec
    case _ => TaskNotFound.invalidNec

  private def mustBeDeleted: ValidatedNec[TaskError, Deleted] = this match
    case deleted: Deleted => deleted.validNec
    case _ => TaskNotFound.invalidNec
end TaskAggregate

/** `DomainModel` instance for the event-sourced [[TaskAggregate]]. */
object TaskAggregate extends DomainModel[TaskAggregate, TaskEvent, TaskError]:
  override def initial: TaskAggregate = Empty

  override def transition: TaskEvent => TaskAggregate => ValidatedNec[TaskError, TaskAggregate] = {
    case TaskCreated(task) => _ => Active(task).validNec
    case TaskUpdated(task) => _.mustBeActive.map(_ => Active(task))
    case TaskDeleted => _.mustBeActive.map(active => Deleted(active.task))
  }
end TaskAggregate
