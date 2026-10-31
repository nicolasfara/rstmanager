package io.github.nicolasfara.rstmanager.work.domain.task

import cats.Monad

/** Edomata service for the event-sourced [[TaskAggregate]]. */
object TaskService extends TaskAggregate.Service[TaskService.Command, TaskService.Notification]:
  enum Command derives CanEqual:
    case Create(task: Task)
    case Update(task: Task)
    case Delete

  enum Notification derives CanEqual:
    /** The task definition was created, updated, or deleted. */
    case TaskChanged(taskId: TaskId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(task) =>
      mustTargetOwnStream(task.id) >> App.state.decide(_.create(task)).void >> App.publish(Notification.TaskChanged(task.id))
    case Command.Update(task) =>
      mustTargetOwnStream(task.id) >> App.state.decide(_.update(task)).void >> App.publish(Notification.TaskChanged(task.id))
    case Command.Delete =>
      App.state.decide(_.delete).flatMap {
        case TaskAggregate.Deleted(task) => App.publish(Notification.TaskChanged(task.id))
        case TaskAggregate.Empty | TaskAggregate.Active(_) => App.unit
      }
  }

  /** Rejects a command whose task belongs to another stream, so the stream id and the stored task id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](id: TaskId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == id.toString then App.unit else App.reject(TaskError.TaskIdMismatch))
end TaskService
