package io.github.nicolasfara.rstmanager.planning

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import com.github.nscala_time.time.Imports.DateTime

/** Unique identifier for a planning request. */
type PlanningRequestId = UUID

/**
 * Reason why planning was requested.
 *
 * Planning is system-managed: every relevant operational change is represented as a trigger that can start a new planning attempt. Triggers are
 * intentionally coarse-grained and identify the affected order where that context is available.
 */
enum PlanningTrigger derives CanEqual:
  /** Regular day-by-day planning run. */
  case DailyPlanning

  /** An order-level change requires a new schedule. */
  case OrderChanged(orderId: OrderId)

  /** A manufacturing change inside an order requires a new schedule. */
  case ManufacturingChanged(orderId: OrderId, manufacturingId: ScheduledManufacturingId)

  /** Task progress or task structure changed inside an order. */
  case TaskChanged(orderId: OrderId, manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId)

  /** Employee capacity, calendar overrides, holidays, or absences changed. */
  case WorkforceCapacityChanged

  /** Recovery trigger used after a failed or rejected planning attempt. */
  case ManualRecovery

/**
 * Input metadata for a system-managed planning attempt.
 *
 * A request captures the planning window, the reason planning was triggered, and the open orders considered by that run. Open orders are expected to
 * be `InProgressOrder` values selected by the planning policy before the scheduling algorithm starts; suspended, completed, delivered, cancelled, and
 * new orders are excluded from `openOrderIds`.
 *
 * @param id
 *   Stable request identifier.
 * @param window
 *   Inclusive planning window.
 * @param trigger
 *   Domain change or scheduled run that requested planning.
 * @param requestedOn
 *   Timestamp when planning was requested.
 * @param openOrderIds
 *   Orders included in the planning attempt.
 */
final case class PlanningRequest(
    id: PlanningRequestId,
    window: PlanningWindow,
    trigger: PlanningTrigger,
    requestedOn: DateTime,
    openOrderIds: List[OrderId],
)
