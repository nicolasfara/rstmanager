package io.github.nicolasfara.rstmanager.planning.events

import io.github.nicolasfara.rstmanager.planning.*

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime

/**
 * Domain events emitted by the planning aggregate.
 *
 * These events form the audit trail for a planning attempt. They record both the incremental facts produced while planning, such as task slice
 * assignments, and the terminal outcome, either a computed schedule or a rejected schedule with structured errors.
 */
enum PlanningEvent:
  /** A domain change requested a new scheduling attempt. */
  case PlanningRequested(request: PlanningRequest)

  /** Task hours were assigned to one employee for one production day. */
  case TaskSliceAssigned(slice: ScheduledTaskSlice, assignedOn: DateTime)

  /** An order received a new promised delivery date from planning. */
  case OrderDelayedByPlanning(delay: DelayedOrder, delayedOn: DateTime)

  /** A manufacturing cannot complete by its expected date. */
  case ManufacturingDelayedByPlanning(delay: DelayedManufacturing, delayedOn: DateTime)

  /** An order could not be planned with the known workforce and structural constraints. */
  case OrderMarkedUnplanned(unplannedOrder: UnplannedOrder, markedOn: DateTime)

  /** A non-fatal planning issue was detected. */
  case PlanningWarningRaised(warning: PlanningWarning, raisedOn: DateTime)

  /** A feasible schedule was computed and can be exposed as the final planning result. */
  case ScheduleComputed(result: PlanningResult, computedOn: DateTime)

  /** The planning attempt hit terminal lifecycle or validation errors. */
  case ScheduleRejected(errors: NonEmptyList[PlanningError], rejectedOn: DateTime)
end PlanningEvent
