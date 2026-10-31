package io.github.nicolasfara.rstmanager.hr.domain

import cats.Monad

/** Edomata service for the event-sourced [[EmployeeAggregate]]. */
object EmployeeService extends EmployeeAggregate.Service[EmployeeService.Command, EmployeeService.Notification]:
  enum Command derives CanEqual:
    case Create(employee: Employee)
    case Update(employee: Employee)
    case Delete

  enum Notification derives CanEqual:
    /** The employee record changed, so dependent planning may need recomputation. */
    case EmployeeChanged(employeeId: EmployeeId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(employee) =>
      mustTargetOwnStream(employee.id) >> App.state.decide(_.create(employee)).void >> App.publish(Notification.EmployeeChanged(employee.id))
    case Command.Update(employee) =>
      mustTargetOwnStream(employee.id) >> App.state.decide(_.update(employee)).void >> App.publish(Notification.EmployeeChanged(employee.id))
    case Command.Delete =>
      App.state.decide(_.delete).flatMap {
        case EmployeeAggregate.Deleted(employee) => App.publish(Notification.EmployeeChanged(employee.id))
        case EmployeeAggregate.Empty | EmployeeAggregate.Active(_) => App.unit
      }
  }

  /** Rejects a command whose employee belongs to another stream, so the stream id and the stored employee id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](employeeId: EmployeeId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == employeeId.toString then App.unit else App.reject(EmployeeError.EmployeeIdMismatch))
end EmployeeService
