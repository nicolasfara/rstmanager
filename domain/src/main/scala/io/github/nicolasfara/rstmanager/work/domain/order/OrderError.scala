package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.work.domain.task.schedule.ScheduledTaskId

/** Errors that can occur during Order aggregate operations */
enum OrderError derives CanEqual:
  /** Order must have at least one manufacturing */
  case OrderWithNoManufacturing

  /** Task not found in the order's manufacturings */
  case TaskNotFound(id: ScheduledTaskId)

  /** Invalid state transition attempted */
  case InvalidStateTransition(message: String)

  /** Cannot cancel order in current state */
  case CannotCancelInCurrentState

  /** Cannot update order in current state */
  case CannotUpdateInCurrentState

  /** Cannot suspend order in current state */
  case CannotSuspendInCurrentState

  /** Cannot reactivate order in current state */
  case CannotReactivateInCurrentState

  /** Cannot complete order in current state */
  case CannotCompleteInCurrentState

  /** Cannot complete order with pending manufacturing work */
  case CannotCompleteWithPendingManufacturing

  /** Cannot deliver order in current state */
  case CannotDeliverInCurrentState

  /** Cannot change priority in current state */
  case CannotChangePriorityInCurrentState

  /** Cannot add manufacturing in current state */
  case CannotAddManufacturingInCurrentState

  /** Cannot remove manufacturing in current state */
  case CannotRemoveManufacturingInCurrentState

