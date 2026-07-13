package io.github.nicolasfara.rstmanager.hr.domain.events

import io.github.nicolasfara.rstmanager.hr.domain.Employee

/**
 * Domain events emitted by the [[io.github.nicolasfara.rstmanager.hr.domain.EmployeeAggregate]].
 *
 * They form the audit trail of the employee master-data lifecycle: creation, full-record updates, and deletion.
 */
enum EmployeeEvent derives CanEqual:
  /** The employee record was created. */
  case EmployeeCreated(employee: Employee)

  /** The employee record was replaced with new data. */
  case EmployeeUpdated(employee: Employee)

  /** The employee record was deleted. */
  case EmployeeDeleted
