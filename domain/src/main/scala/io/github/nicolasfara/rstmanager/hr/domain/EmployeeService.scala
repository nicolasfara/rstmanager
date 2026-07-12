package io.github.nicolasfara.rstmanager.hr.domain

import java.util.UUID

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
      App.state.decide(_.create(employee)).void >> App.publish(Notification.EmployeeChanged(employee.id))
    case Command.Update(employee) =>
      App.state.decide(_.update(employee)).void >> App.publish(Notification.EmployeeChanged(employee.id))
    case Command.Delete =>
      App.state.decide(_.delete).void >> publishEmployeeChanged
  }

  private def publishEmployeeChanged[F[_]: Monad]: App[F, Unit] =
    App.aggregateId.flatMap(id => App.publish(Notification.EmployeeChanged(UUID.fromString(id).nn)))
end EmployeeService
