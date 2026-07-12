package io.github.nicolasfara.rstmanager.work.domain.task

import cats.Monad

/** Edomata service for the event-sourced [[TaskAggregate]]. */
object TaskService extends TaskAggregate.Service[TaskService.Command, TaskService.Notification]:
  enum Command derives CanEqual:
    case Create(task: Task)
    case Update(task: Task)
    case Delete

  enum Notification derives CanEqual:
    /** The task definition changed. */
    case TaskChanged(taskId: TaskId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(task) =>
      App.state.decide(_.create(task)).void >> App.publish(Notification.TaskChanged(task.id))
    case Command.Update(task) =>
      App.state.decide(_.update(task)).void >> App.publish(Notification.TaskChanged(task.id))
    case Command.Delete =>
      App.state.decide(_.delete).void
  }
end TaskService
