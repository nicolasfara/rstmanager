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
 * `Planning` records the decisions made while producing a schedule. It does not run the scheduling algorithm; instead, it accepts the facts produced
 * by that algorithm as events. This keeps the planning audit trail explicit and recoverable in the same style as the order execution model.
 *
 * Lifecycle:
 *   - [[Planning.NewPlanning]] has not received a request yet.
 *   - [[Planning.InProgressPlanning]] records task slices, delays, and warnings for one active request.
 *   - [[Planning.CompletedPlanning]] stores the final feasible [[PlanningResult]].
 *   - [[Planning.RejectedPlanning]] stores the planning errors that made the request infeasible.
 *
 * Only an in-progress attempt can receive task slices, delays, warnings, completion, or rejection. A new request can be started from `NewPlanning` or
 * after a previous attempt has completed or been rejected.
 */
enum Planning derives CanEqual:
  /** Initial planning state, before a request has started. */
  case NewPlanning

  /**
   * Active planning attempt.
   *
   * The lists are append-only projections of events already accepted by the aggregate. They are useful while the attempt is still being built, but
   * the authoritative completed output is the [[PlanningResult]] carried by [[Planning.CompletedPlanning]].
   */
  case InProgressPlanning(
      request: PlanningRequest,
      slices: List[ScheduledTaskSlice],
      delayedOrders: List[DelayedOrder],
      delayedManufacturings: List[DelayedManufacturing],
      warnings: List[PlanningWarning],
  )

  /** Feasible planning attempt with its final day-by-day result. */
  case CompletedPlanning(request: PlanningRequest, result: PlanningResult, completedOn: DateTime)

  /** Infeasible planning attempt with one or more structured errors for user-facing reporting. */
  case RejectedPlanning(request: PlanningRequest, errors: NonEmptyList[PlanningError], rejectedOn: DateTime)

  /** Starts a planning attempt when no other attempt is in progress. */
  def request(request: PlanningRequest): Decision[PlanningError, PlanningEvent, Planning] = this.decide {
    case NewPlanning | CompletedPlanning(_, _, _) | RejectedPlanning(_, _, _) => Decision.accept(PlanningRequested(request))
    case InProgressPlanning(activeRequest, _, _, _, _) => Decision.reject(PlanningAlreadyInProgress(activeRequest.id))
  }.validate(_.mustBeInProgress)

  /** Records a daily task assignment during an active planning attempt. */
  def assignTaskSlice(slice: ScheduledTaskSlice): Decision[PlanningError, PlanningEvent, Planning] =
    this.perform(mustBeInProgress.toDecision *> TaskSliceAssigned(slice, DateTime.now()).accept).validate(_.mustBeInProgress)

  /** Records an order delay during an active planning attempt. */
  def delayOrder(delay: DelayedOrder): Decision[PlanningError, PlanningEvent, Planning] =
    this.perform(mustBeInProgress.toDecision *> OrderDelayedByPlanning(delay, DateTime.now()).accept).validate(_.mustBeInProgress)

  /** Records a manufacturing delay during an active planning attempt. */
  def delayManufacturing(delay: DelayedManufacturing): Decision[PlanningError, PlanningEvent, Planning] =
    this.perform(mustBeInProgress.toDecision *> ManufacturingDelayedByPlanning(delay, DateTime.now()).accept).validate(_.mustBeInProgress)

  /** Records a non-fatal planning warning during an active planning attempt. */
  def raiseWarning(warning: PlanningWarning): Decision[PlanningError, PlanningEvent, Planning] =
    this.perform(mustBeInProgress.toDecision *> PlanningWarningRaised(warning, DateTime.now()).accept).validate(_.mustBeInProgress)

  /** Completes an active planning attempt with a feasible result. */
  def complete(result: PlanningResult): Decision[PlanningError, PlanningEvent, Planning] =
    this.perform(mustBeInProgress.toDecision *> ScheduleComputed(result, DateTime.now()).accept).validate(_.mustBeCompleted)

  /** Rejects an active planning attempt with one or more planning errors. */
  def reject(errors: NonEmptyList[PlanningError]): Decision[PlanningError, PlanningEvent, Planning] =
    this.perform(mustBeInProgress.toDecision *> ScheduleRejected(errors, DateTime.now()).accept).validate(_.mustBeRejected)

  private def mustBeInProgress: ValidatedNec[PlanningError, InProgressPlanning] = this match
    case planning: InProgressPlanning => planning.validNec
    case _ => PlanningMustBeInProgress.invalidNec

  private def mustBeCompleted: ValidatedNec[PlanningError, CompletedPlanning] = this match
    case planning: CompletedPlanning => planning.validNec
    case _ => PlanningMustBeInProgress.invalidNec

  private def mustBeRejected: ValidatedNec[PlanningError, RejectedPlanning] = this match
    case planning: RejectedPlanning => planning.validNec
    case _ => PlanningMustBeInProgress.invalidNec
end Planning

/** `DomainModel` instance for the event-sourced `Planning` aggregate. */
object Planning extends DomainModel[Planning, PlanningEvent, PlanningError]:
  override def initial: Planning = NewPlanning

  override def transition: PlanningEvent => Planning => ValidatedNec[PlanningError, Planning] = {
    case PlanningRequested(request) => _ => InProgressPlanning(request, Nil, Nil, Nil, Nil).validNec
    case TaskSliceAssigned(slice, _) =>
      _.mustBeInProgress.map { planning => planning.copy(slices = planning.slices :+ slice) }
    case OrderDelayedByPlanning(delay, _) =>
      _.mustBeInProgress.map { planning => planning.copy(delayedOrders = planning.delayedOrders :+ delay) }
    case ManufacturingDelayedByPlanning(delay, _) =>
      _.mustBeInProgress.map { planning => planning.copy(delayedManufacturings = planning.delayedManufacturings :+ delay) }
    case PlanningWarningRaised(warning, _) =>
      _.mustBeInProgress.map { planning => planning.copy(warnings = planning.warnings :+ warning) }
    case ScheduleComputed(result, computedOn) =>
      _.mustBeInProgress.map { planning => CompletedPlanning(planning.request, result, computedOn) }
    case ScheduleRejected(errors, rejectedOn) =>
      _.mustBeInProgress.map { planning => RejectedPlanning(planning.request, errors, rejectedOn) }
  }
end Planning
