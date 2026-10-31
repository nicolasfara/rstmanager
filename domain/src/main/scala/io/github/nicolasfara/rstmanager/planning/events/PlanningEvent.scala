package io.github.nicolasfara.rstmanager.planning.events

import io.github.nicolasfara.rstmanager.planning.*

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime

/**
 * Domain events emitted by the planning aggregate.
 *
 * These events form the audit trail for a planning attempt. A run is recorded as `PlanningRequested` followed by its terminal outcome, either a
 * computed schedule or a rejected schedule with structured errors. The per-fact events in between are legacy: they are no longer emitted, and are
 * kept so older journals still decode and fold.
 */
enum PlanningEvent derives CanEqual:
  /** A domain change requested a new scheduling attempt. */
  case PlanningRequested(request: PlanningRequest)

  /** Legacy: task hours were assigned to one employee for one production day. */
  case TaskSliceAssigned(slice: ScheduledTaskSlice, assignedOn: DateTime)

  /** Legacy: an order received a new planned completion date from planning. */
  case OrderDelayedByPlanning(delay: DelayedOrder, delayedOn: DateTime)

  /** Legacy: a manufacturing cannot complete by its expected date. */
  case ManufacturingDelayedByPlanning(delay: DelayedManufacturing, delayedOn: DateTime)

  /** Legacy: an order could not be planned with the known workforce and structural constraints. */
  case OrderMarkedUnplanned(unplannedOrder: UnplannedOrder, markedOn: DateTime)

  /** Legacy: a non-fatal planning issue was detected. */
  case PlanningWarningRaised(warning: PlanningWarning, raisedOn: DateTime)

  /** A feasible schedule was computed and can be exposed as the final planning result. */
  case ScheduleComputed(result: PlanningResult, computedOn: DateTime)

  /** The planning attempt hit terminal lifecycle or validation errors. */
  case ScheduleRejected(errors: NonEmptyList[PlanningError], rejectedOn: DateTime)
end PlanningEvent
