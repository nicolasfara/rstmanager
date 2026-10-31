package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.planning.Planning.*
import io.github.nicolasfara.rstmanager.planning.PlanningError.*
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent.*

import cats.data.{ NonEmptyList, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import edomata.core.*
import edomata.syntax.all.*

/**
 * Event-sourced aggregate representing one planning attempt lifecycle.
 *
 * `Planning` records the outcome of each scheduling run. It does not run the scheduling algorithm: a run is recorded as `PlanningRequested` followed
 * by one `ScheduleComputed` (the full day-by-day result) or `ScheduleRejected` event. Older journals also contain one event per task slice, delay,
 * unplanned order, and warning before the final event; the fold still accepts them.
 *
 * Lifecycle:
 *   - [[Planning.NewPlanning]] has not received a request yet.
 *   - [[Planning.InProgressPlanning]] records task slices, delays, unplanned orders, and warnings for one active request.
 *   - [[Planning.CompletedPlanning]] stores the final feasible [[PlanningResult]].
 *   - [[Planning.RejectedPlanning]] stores terminal lifecycle or validation errors.
 *
 * Only an in-progress attempt can be concluded. A new request can be started from `NewPlanning` or after a previous attempt has completed or been
 * rejected.
 *
 * Decisions never read the clock: the instant recorded on the emitted events (`now`) is supplied by the caller.
 */
enum Planning derives CanEqual:
  /** Initial planning state, before a request has started. */
  case NewPlanning

  /**
   * Active planning attempt.
   *
   * The lists are filled only when replaying older journals, which recorded one event per fact before concluding; current runs conclude directly from
   * the request.
   */
  case InProgressPlanning(
      request: PlanningRequest,
      slices: List[ScheduledTaskSlice],
      delayedOrders: List[DelayedOrder],
      delayedManufacturings: List[DelayedManufacturing],
      unplannedOrders: List[UnplannedOrder],
      warnings: List[PlanningWarning],
  )

  /** Feasible planning attempt with its final day-by-day result. */
  case CompletedPlanning(request: PlanningRequest, result: PlanningResult, completedOn: DateTime)

  /** Rejected planning attempt with one or more terminal errors for user-facing reporting. */
  case RejectedPlanning(request: PlanningRequest, errors: NonEmptyList[PlanningError], rejectedOn: DateTime)

  /** Starts a planning attempt when no other attempt is in progress. */
  def request(request: PlanningRequest): Decision[PlanningError, PlanningEvent, Planning] = this.decide {
    case NewPlanning | CompletedPlanning(_, _, _) | RejectedPlanning(_, _, _) => Decision.accept(PlanningRequested(request))
    case InProgressPlanning(activeRequest, _, _, _, _, _) => Decision.reject(PlanningAlreadyInProgress(activeRequest.id))
  }.validate(_.mustBeInProgress)

  /**
   * Concludes an active attempt with the scheduler outcome, recorded as a single event: `ScheduleComputed` with the full day-by-day result, or
   * `ScheduleRejected` with every reason when the outcome does not form a valid result.
   */
  def conclude(outcome: SchedulingOutcome, now: DateTime): Decision[PlanningError, PlanningEvent, Planning] = this.decide {
    case _: InProgressPlanning =>
      PlanningResult
        .fromSlices(outcome.slices, outcome.delayedOrders, outcome.delayedManufacturings, outcome.unplannedOrders, outcome.warnings)
        .fold(errors => Decision.accept(ScheduleRejected(errors.toNonEmptyList, now)), result => Decision.accept(ScheduleComputed(result, now)))
    case _ => Decision.reject(PlanningMustBeInProgress)
  }.validate(_.mustBeConcluded)

  private def mustBeInProgress: ValidatedNec[PlanningError, InProgressPlanning] = this match
    case planning: InProgressPlanning => planning.validNec
    case _ => PlanningMustBeInProgress.invalidNec

  private def mustBeConcluded: ValidatedNec[PlanningError, CompletedPlanning | RejectedPlanning] = this match
    case planning: CompletedPlanning => planning.validNec
    case planning: RejectedPlanning => planning.validNec
    case _ => PlanningMustBeConcluded.invalidNec
end Planning

/** `DomainModel` instance for the event-sourced `Planning` aggregate. */
object Planning extends DomainModel[Planning, PlanningEvent, PlanningError]:
  override def initial: Planning = NewPlanning

  override def transition: PlanningEvent => Planning => ValidatedNec[PlanningError, Planning] = {
    case PlanningRequested(request) => _ => InProgressPlanning(request, Nil, Nil, Nil, Nil, Nil).validNec
    case TaskSliceAssigned(slice, _) =>
      _.mustBeInProgress.map { planning => planning.copy(slices = planning.slices :+ slice) }
    case OrderDelayedByPlanning(delay, _) =>
      _.mustBeInProgress.map { planning => planning.copy(delayedOrders = planning.delayedOrders :+ delay) }
    case ManufacturingDelayedByPlanning(delay, _) =>
      _.mustBeInProgress.map { planning => planning.copy(delayedManufacturings = planning.delayedManufacturings :+ delay) }
    case OrderMarkedUnplanned(unplannedOrder, _) =>
      _.mustBeInProgress.map { planning => planning.copy(unplannedOrders = planning.unplannedOrders :+ unplannedOrder) }
    case PlanningWarningRaised(warning, _) =>
      _.mustBeInProgress.map { planning => planning.copy(warnings = planning.warnings :+ warning) }
    case ScheduleComputed(result, computedOn) =>
      _.mustBeInProgress.map { planning => CompletedPlanning(planning.request, result, computedOn) }
    case ScheduleRejected(errors, rejectedOn) =>
      _.mustBeInProgress.map { planning => RejectedPlanning(planning.request, errors, rejectedOn) }
  }
end Planning
