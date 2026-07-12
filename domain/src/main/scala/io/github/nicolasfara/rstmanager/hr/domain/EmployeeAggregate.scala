package io.github.nicolasfara.rstmanager.hr.domain

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeAggregate.*
import io.github.nicolasfara.rstmanager.hr.domain.EmployeeError.*
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent.*

import cats.data.ValidatedNec
import cats.syntax.all.*
import edomata.core.*
import edomata.syntax.all.*

/**
 * Event-sourced aggregate holding one employee master-data record.
 *
 * Lifecycle:
 *   - [[EmployeeAggregate.Empty]] before the record exists.
 *   - [[EmployeeAggregate.Active]] holds the current [[Employee]].
 *   - [[EmployeeAggregate.Deleted]] keeps the last value for audit while rejecting further changes.
 */
enum EmployeeAggregate derives CanEqual:
  case Empty
  case Active(employee: Employee)
  case Deleted(employee: Employee)

  /** Creates the record from the empty state. */
  def create(employee: Employee): Decision[EmployeeError, EmployeeEvent, EmployeeAggregate] = this.decide {
    case Empty => Decision.accept(EmployeeCreated(employee))
    case _ => Decision.reject(EmployeeAlreadyExists)
  }.validate(_.mustBeActive)

  /** Replaces the record data when it is active. */
  def update(employee: Employee): Decision[EmployeeError, EmployeeEvent, EmployeeAggregate] = this.decide {
    case Active(_) => Decision.accept(EmployeeUpdated(employee))
    case _ => Decision.reject(EmployeeNotFound)
  }.validate(_.mustBeActive)

  /** Deletes the record when it is active. */
  def delete: Decision[EmployeeError, EmployeeEvent, EmployeeAggregate] = this.decide {
    case Active(_) => Decision.accept(EmployeeDeleted)
    case _ => Decision.reject(EmployeeNotFound)
  }.validate(_.mustBeDeleted)

  private def mustBeActive: ValidatedNec[EmployeeError, Active] = this match
    case active: Active => active.validNec
    case _ => EmployeeNotFound.invalidNec

  private def mustBeDeleted: ValidatedNec[EmployeeError, Deleted] = this match
    case deleted: Deleted => deleted.validNec
    case _ => EmployeeNotFound.invalidNec
end EmployeeAggregate

/** `DomainModel` instance for the event-sourced [[EmployeeAggregate]]. */
object EmployeeAggregate extends DomainModel[EmployeeAggregate, EmployeeEvent, EmployeeError]:
  override def initial: EmployeeAggregate = Empty

  override def transition: EmployeeEvent => EmployeeAggregate => ValidatedNec[EmployeeError, EmployeeAggregate] = {
    case EmployeeCreated(employee) => _ => Active(employee).validNec
    case EmployeeUpdated(employee) => _.mustBeActive.map(_ => Active(employee))
    case EmployeeDeleted => _.mustBeActive.map(active => Deleted(active.employee))
  }
end EmployeeAggregate
