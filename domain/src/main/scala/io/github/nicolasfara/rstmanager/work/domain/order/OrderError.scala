package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturingError, ScheduledManufacturingId }

/** Errors raised while deciding or applying `Order` operations. */
enum OrderError derives CanEqual:
  case OrderAlreadyCreated
  case OrderMustBeInProgress
  case OrderMustBeSuspended
  case OrderMustBeCompleted
  case OrderMustBeCancelled
  case OrderMustBeDelivered
  case OrderMustBeInProgressOrPaused
  case OrderAlreadyCancelled
  case NoSuchOrder
  case OnlyCancelledOrdersCanBeReactivated
  case ManufacturingNotFound(manufacturingId: ScheduledManufacturingId)
  case ManufacturingError(error: ScheduledManufacturingError)
