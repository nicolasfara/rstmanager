package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturingError, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.task.TaskId
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

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
  case OnlyCancelledOrCompletedOrdersCanBeReopened
  case ManufacturingNotFound(manufacturingId: ScheduledManufacturingId)
  case ManufacturingError(error: ScheduledManufacturingError)
  case UnknownManufacturingInDependencies(manufacturingIds: Set[ScheduledManufacturingId])
  case ManufacturingDependencyCycle(cycle: Set[ScheduledManufacturingId])
  case UnknownTaskInDependencies(manufacturingId: ScheduledManufacturingId, taskIds: Set[TaskId])
  case TaskDependencyCycle(manufacturingId: ScheduledManufacturingId, cycle: Set[TaskId])
  case DuplicateManufacturingIds(manufacturingIds: Set[ScheduledManufacturingId])
  case DuplicateTaskIds(taskIds: Set[ScheduledTaskId])
  case CannotRemoveLastManufacturing
  case OrderIdMismatch
  case RepeatedCatalogTasks(manufacturingId: ScheduledManufacturingId, taskIds: Set[TaskId])
end OrderError
