package io.github.nicolasfara.rstmanager.customer.domain

import io.github.nicolasfara.rstmanager.customer.domain.CustomerAggregate.*
import io.github.nicolasfara.rstmanager.customer.domain.CustomerError.*
import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent
import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent.*

import cats.data.ValidatedNec
import cats.syntax.all.*
import edomata.core.*
import edomata.syntax.all.*

/**
 * Event-sourced aggregate holding one customer master-data record.
 *
 * Lifecycle:
 *   - [[CustomerAggregate.Empty]] before the record exists.
 *   - [[CustomerAggregate.Active]] holds the current [[Customer]].
 *   - [[CustomerAggregate.Deleted]] keeps the last value for audit while rejecting further changes.
 */
enum CustomerAggregate derives CanEqual:
  case Empty
  case Active(customer: Customer)
  case Deleted(customer: Customer)

  /** Creates the record from the empty state. */
  def create(customer: Customer): Decision[CustomerError, CustomerEvent, CustomerAggregate] = this.decide {
    case Empty => Decision.accept(CustomerCreated(customer))
    case _ => Decision.reject(CustomerAlreadyExists)
  }.validate(_.mustBeActive)

  /** Replaces the record data when it is active. */
  def update(customer: Customer): Decision[CustomerError, CustomerEvent, CustomerAggregate] = this.decide {
    case Active(_) => Decision.accept(CustomerUpdated(customer))
    case _ => Decision.reject(CustomerNotFound)
  }.validate(_.mustBeActive)

  /** Deletes the record when it is active. */
  def delete: Decision[CustomerError, CustomerEvent, CustomerAggregate] = this.decide {
    case Active(_) => Decision.accept(CustomerDeleted)
    case _ => Decision.reject(CustomerNotFound)
  }.validate(_.mustBeDeleted)

  private def mustBeActive: ValidatedNec[CustomerError, Active] = this match
    case active: Active => active.validNec
    case _ => CustomerNotFound.invalidNec

  private def mustBeDeleted: ValidatedNec[CustomerError, Deleted] = this match
    case deleted: Deleted => deleted.validNec
    case _ => CustomerNotFound.invalidNec
end CustomerAggregate

/** `DomainModel` instance for the event-sourced [[CustomerAggregate]]. */
object CustomerAggregate extends DomainModel[CustomerAggregate, CustomerEvent, CustomerError]:
  override def initial: CustomerAggregate = Empty

  override def transition: CustomerEvent => CustomerAggregate => ValidatedNec[CustomerError, CustomerAggregate] = {
    case CustomerCreated(customer) => _ => Active(customer).validNec
    case CustomerUpdated(customer) => _.mustBeActive.map(_ => Active(customer))
    case CustomerDeleted => _.mustBeActive.map(active => Deleted(active.customer))
  }
