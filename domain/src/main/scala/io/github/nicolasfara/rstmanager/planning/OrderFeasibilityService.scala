package io.github.nicolasfara.rstmanager.planning

import scala.annotation.tailrec

import io.github.nicolasfara.rstmanager.hr.domain.Employee
import io.github.nicolasfara.rstmanager.hr.domain.calendar.CompanyCalendar
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder

import com.github.nscala_time.time.Imports.DateTime

/**
 * Domain service checking, before an order is created, whether it fits the current production plan with the dates the user chose.
 *
 * The candidate order is planned together with the real open orders, with its own priority and work deadline, exactly as a real recalculation would
 * do once it is created. Comparing that run with a baseline run (open orders only) tells:
 *   - whether the candidate itself would finish after its work deadline, or some of its manufacturings after their completion date;
 *   - which existing orders would become late (or unplannable) only because the candidate is inserted.
 *
 * When there is a delay, it also searches the earliest work deadline with which the candidate is on time and delays no existing order that a
 * different deadline could spare. Nothing is persisted; the result is a pure estimate.
 */
object OrderFeasibilityService:
  /** Identifier to give the candidate order: name-based, so it cannot collide with a real (random) order id. */
  val candidateId: OrderId = OrderId(OrderSimulationService.syntheticId("feasibility/order"))

  /**
   * An existing order that would be late only because the candidate is inserted.
   *
   * @param plannedCompletion
   *   Its completion with the candidate in the plan; `None` when it could no longer be planned at all.
   */
  final case class InducedDelay(orderId: OrderId, number: String, deadline: DateTime, plannedCompletion: Option[DateTime])

  /** A manufacturing of the candidate that would finish after its own completion date. */
  final case class LateManufacturing(manufacturingId: ScheduledManufacturingId, code: String, completionDate: DateTime, plannedCompletion: DateTime)

  /**
   * Outcome of a feasibility check.
   *
   * @param plannedCompletion
   *   Last production day planned for the candidate; `None` when it cannot be planned with the known workforce (see `unplannedReasons`).
   * @param late
   *   Whether the candidate would finish after its work deadline.
   * @param suggestedDeadline
   *   Earliest work deadline (start of day) without avoidable delays; present only when the check found a delay it can fix.
   */
  final case class FeasibilityResult(
      plannedCompletion: Option[DateTime],
      late: Boolean,
      lateManufacturings: List[LateManufacturing],
      inducedDelays: List[InducedDelay],
      suggestedDeadline: Option[DateTime],
      unplannedReasons: List[UnplannedReason],
  ):
    /** Whether inserting the candidate as it is produces any delay or cannot be planned. */
    def hasDelay: Boolean = plannedCompletion.isEmpty || late || lateManufacturings.nonEmpty || inducedDelays.nonEmpty

  /** Upper bound on the deadline search; each step moves the deadline at least one day later. */
  private val MaxSearchSteps = 60

  /**
   * Checks the candidate order against the current plan.
   *
   * @param now
   *   Timestamp of the check; planning starts on this day and today's residual working hours are applied like in a real run.
   * @param openOrders
   *   Current orders; only in-progress ones take part in the runs.
   * @param candidate
   *   The order about to be created, with its real priority and work deadline.
   */
  def check(
      now: DateTime,
      openOrders: List[Order],
      employees: List[Employee],
      calendar: CompanyCalendar,
      candidate: InProgressOrder,
  ): FeasibilityResult =
    val open = openOrders.collect { case order: InProgressOrder if !order.data.id.equals(candidate.data.id) => order }
    val runner = Runner(now, open, employees, calendar)
    val evaluation = runner.evaluate(candidate)
    val result = evaluation.toResult(None)
    evaluation.plannedCompletion match
      case Some(completion) if evaluation.late || evaluation.inducedDelays.nonEmpty =>
        val unavoidable = runner.evaluate(retarget(candidate, OrderSimulationService.horizon(now))).inducedIds
        val avoidableDelay = evaluation.late || evaluation.inducedDelays.exists(delay => !unavoidable.contains(delay.orderId))
        if avoidableDelay then result.copy(suggestedDeadline = suggest(runner, candidate, completion, evaluation, unavoidable)) else result
      case _ => result
  end check

  private def suggest(
      runner: Runner,
      candidate: InProgressOrder,
      completion: DateTime,
      first: Evaluation,
      unavoidable: Set[OrderId],
  ): Option[DateTime] =
    val original = day(candidate.promisedDeliveryDate)

    @tailrec
    def loop(deadline: DateTime, steps: Int): Option[DateTime] =
      val evaluation = runner.evaluate(retarget(candidate, deadline))
      evaluation.plannedCompletion match
        case None => None
        case Some(_) if !evaluation.late && evaluation.inducedIds.subsetOf(unavoidable) => Some(deadline)
        case Some(done) if steps < MaxSearchSteps => loop(nextDeadline(deadline, done, evaluation, unavoidable), steps + 1)
        case Some(_) => fallback

    def fallback: Option[DateTime] =
      runner.evaluate(retarget(candidate, OrderSimulationService.horizon(runner.now))).plannedCompletion.map(day)

    loop(nextDeadline(original, completion, first, unavoidable), 0)
  end suggest

  /**
   * Next deadline to try: never earlier than the planned completion, at least one day later than the current try, and past the deadline of the
   * earliest order the candidate still delays (so the priority policy plans the candidate after it).
   */
  private def nextDeadline(current: DateTime, completion: DateTime, evaluation: Evaluation, unavoidable: Set[OrderId]): DateTime =
    val pastAvoidable = evaluation.inducedDelays
      .filterNot(delay => unavoidable.contains(delay.orderId))
      .map(delay => day(delay.deadline).plusDays(1).nn)
      .minByOption(_.getMillis)
    val dayAfter = current.plusDays(1).nn
    val bound = if completion.isAfter(current) then day(completion) else dayAfter
    (bound :: pastAvoidable.toList).maxBy(_.getMillis)

  /** The candidate with a new work deadline; manufacturings that inherited the old deadline (same day) follow it. */
  private def retarget(candidate: InProgressOrder, deadline: DateTime): InProgressOrder =
    val original = day(candidate.promisedDeliveryDate)
    val manufacturings = candidate.data.setOfManufacturing.map { manufacturing =>
      if day(manufacturing.info.completionDate).getMillis == original.getMillis then manufacturing.withCompletionDate(deadline) else manufacturing
    }
    InProgressOrder(candidate.data.copy(setOfManufacturing = manufacturings), deadline)

  private def day(instant: DateTime): DateTime = instant.withTimeAtStartOfDay().nn

  /** What one run with the candidate says, compared with the baseline run. */
  private final case class Evaluation(
      plannedCompletion: Option[DateTime],
      late: Boolean,
      lateManufacturings: List[LateManufacturing],
      inducedDelays: List[InducedDelay],
      unplannedReasons: List[UnplannedReason],
  ):
    def inducedIds: Set[OrderId] = inducedDelays.map(_.orderId).toSet

    def toResult(suggestion: Option[DateTime]): FeasibilityResult =
      FeasibilityResult(plannedCompletion, late, lateManufacturings, inducedDelays, suggestion, unplannedReasons)

  /** Runs the scheduler over the open orders, alone (baseline, computed once) or with a candidate. */
  private final case class Runner(now: DateTime, open: List[InProgressOrder], employees: List[Employee], calendar: CompanyCalendar):
    private val baseline = run(None)
    private val baselineLate: Set[OrderId] =
      baseline.delayedOrders.map(_.orderId).toSet ++ baseline.unplannedOrders.map(_.orderId)

    def evaluate(candidate: InProgressOrder): Evaluation =
      val candidateId = candidate.data.id
      val outcome = run(Some(candidate))
      val completion = outcome.slices.filter(_.orderId.equals(candidateId)).map(_.day).maxByOption(_.getMillis)
      outcome.unplannedOrders.find(_.orderId.equals(candidateId)) match
        case Some(unplanned) => Evaluation(None, false, Nil, inducedDelays(outcome, candidateId), unplanned.blockedTasks.toList.map(_.reason))
        case None =>
          // Work already fully done plans no slice: deliverable right away.
          val done = completion.getOrElse(day(now))
          Evaluation(
            Some(done),
            done.isAfter(candidate.promisedDeliveryDate),
            lateManufacturings(outcome, candidate),
            inducedDelays(outcome, candidateId),
            Nil,
          )
    end evaluate

    private def run(candidate: Option[InProgressOrder]): SchedulingOutcome =
      val orders = open ++ candidate.toList
      val request = PlanningRequest(
        PlanningRequestId(OrderSimulationService.syntheticId("feasibility/request")),
        now,
        PlanningTrigger.DailyPlanning,
        now,
        orders.map(_.data.id),
      )
      SchedulingService.computeSchedule(request, orders, employees, calendar)

    private def lateManufacturings(outcome: SchedulingOutcome, candidate: InProgressOrder): List[LateManufacturing] =
      val byId = candidate.data.setOfManufacturing.toList.map(manufacturing => manufacturing.info.id -> manufacturing.info).toMap
      outcome.delayedManufacturings.filter(_.orderId.equals(candidate.data.id)).flatMap { delay =>
        byId.get(delay.manufacturingId).map { info =>
          LateManufacturing(info.id, info.code, delay.expectedCompletionDate, delay.computedCompletionDate)
        }
      }

    private def inducedDelays(outcome: SchedulingOutcome, candidateId: OrderId): List[InducedDelay] =
      val late = outcome.delayedOrders.map(delay => delay.orderId -> Option(delay.plannedCompletion)).toMap
      val unplanned = outcome.unplannedOrders.map(_.orderId -> Option.empty[DateTime]).toMap
      val affected = late ++ unplanned
      open.flatMap { order =>
        val id = order.data.id
        if id.equals(candidateId) || baselineLate.contains(id) then None
        else affected.get(id).map(InducedDelay(id, order.data.number, order.promisedDeliveryDate, _))
      }
  end Runner
end OrderFeasibilityService
