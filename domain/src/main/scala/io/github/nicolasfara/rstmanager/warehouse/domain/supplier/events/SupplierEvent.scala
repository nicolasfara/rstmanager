package io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events

import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.Supplier

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierAggregate]].
 *
 * They form the audit trail of the supplier master-data lifecycle: creation, full-record updates, and (soft) deletion.
 */
enum SupplierEvent derives CanEqual:
  /** The supplier record was created. */
  case SupplierCreated(supplier: Supplier)

  /** The supplier record was replaced with new data. */
  case SupplierUpdated(supplier: Supplier)

  /** The supplier was deleted; its record is kept for the purchase history. */
  case SupplierDeleted
