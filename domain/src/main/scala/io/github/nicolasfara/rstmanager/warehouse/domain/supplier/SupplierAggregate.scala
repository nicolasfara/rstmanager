package io.github.nicolasfara.rstmanager.warehouse.domain.supplier

import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierAggregate.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierError.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent.*

import cats.data.ValidatedNec
import cats.syntax.all.*
import edomata.core.*
import edomata.syntax.all.*

/**
 * Event-sourced aggregate holding one supplier master-data record.
 *
 * Lifecycle:
 *   - [[SupplierAggregate.Empty]] before the record exists.
 *   - [[SupplierAggregate.Active]] holds the current [[Supplier]].
 *   - [[SupplierAggregate.Deleted]] keeps the last value (soft delete) so the purchase history can still name the supplier, while rejecting further
 *     changes.
 */
enum SupplierAggregate derives CanEqual:
  case Empty
  case Active(supplier: Supplier)
  case Deleted(supplier: Supplier)

  /** Creates the record from the empty state. */
  def create(supplier: Supplier): Decision[SupplierError, SupplierEvent, SupplierAggregate] = this.decide {
    case Empty => Decision.accept(SupplierCreated(supplier))
    case _ => Decision.reject(SupplierAlreadyExists)
  }.validate(_.mustBeActive)

  /** Replaces the record data when it is active. */
  def update(supplier: Supplier): Decision[SupplierError, SupplierEvent, SupplierAggregate] = this.decide {
    case Active(_) => Decision.accept(SupplierUpdated(supplier))
    case _ => Decision.reject(SupplierNotFound)
  }.validate(_.mustBeActive)

  /** Deletes the record when it is active, keeping its data. */
  def delete: Decision[SupplierError, SupplierEvent, SupplierAggregate] = this.decide {
    case Active(_) => Decision.accept(SupplierDeleted)
    case _ => Decision.reject(SupplierNotFound)
  }.validate(_.mustBeDeleted)

  private def mustBeActive: ValidatedNec[SupplierError, Active] = this match
    case active: Active => active.validNec
    case _ => SupplierNotFound.invalidNec

  private def mustBeDeleted: ValidatedNec[SupplierError, Deleted] = this match
    case deleted: Deleted => deleted.validNec
    case _ => SupplierNotFound.invalidNec
end SupplierAggregate

/** `DomainModel` instance for the event-sourced [[SupplierAggregate]]. */
object SupplierAggregate extends DomainModel[SupplierAggregate, SupplierEvent, SupplierError]:
  override def initial: SupplierAggregate = Empty

  override def transition: SupplierEvent => SupplierAggregate => ValidatedNec[SupplierError, SupplierAggregate] = {
    case SupplierCreated(supplier) => _ => Active(supplier).validNec
    case SupplierUpdated(supplier) => _.mustBeActive.map(_ => Active(supplier))
    case SupplierDeleted => _.mustBeActive.map(active => Deleted(active.supplier))
  }
