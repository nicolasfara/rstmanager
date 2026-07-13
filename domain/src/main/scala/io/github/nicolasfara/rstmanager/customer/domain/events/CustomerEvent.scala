package io.github.nicolasfara.rstmanager.customer.domain.events

import io.github.nicolasfara.rstmanager.customer.domain.Customer

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.customer.domain.CustomerAggregate]].
 *
 * They form the audit trail of the customer master-data lifecycle: creation, full-record updates, and deletion.
 */
enum CustomerEvent derives CanEqual:
  /** The customer record was created. */
  case CustomerCreated(customer: Customer)

  /** The customer record was replaced with new data. */
  case CustomerUpdated(customer: Customer)

  /** The customer record was deleted. */
  case CustomerDeleted
