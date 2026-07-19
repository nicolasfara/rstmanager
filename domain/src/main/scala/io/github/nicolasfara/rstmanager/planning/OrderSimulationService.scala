package io.github.nicolasfara.rstmanager.planning

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.Employee
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ Manufacturing, ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingInfo }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderData, OrderId, OrderNumber, OrderPriority }
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskHours }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*

/**
 * Domain service estimating the first date on which a hypothetical new order could be completed.
 *
 * The simulation builds a synthetic in-progress order from the requested demand and runs one regular [[SchedulingService]] pass over the real open
 * orders plus the synthetic one. The synthetic order is given normal priority and a work deadline far beyond every real order, so the priority
 * policy plans it after all current commitments: existing orders keep exactly the capacity they would get today, and the synthetic order only
 * consumes what is left. Nothing is persisted; the result is a pure estimate.
 */
object OrderSimulationService:
  /** What the hypothetical order requires, in one of the two supported shapes. */
  enum SimulationDemand derives CanEqual:
    /** A single opaque block of work of the given total hours, with no internal task structure. */
    case TotalHours(hours: TaskHours)

    /**
     * One synthetic manufacturing per selected catalog template, each carrying its catalog tasks (already resolved by the caller) with their default
     * required hours, task dependency graph, and default employees.
     */
    case FromManufacturings(selection: NonEmptyList[(Manufacturing, NonEmptyList[Task])])

  /**
   * Outcome of one simulation run.
   *
   * @param totalHours
   *   Hours of work the hypothetical order requires.
   * @param startDate
   *   First production day on which work for the hypothetical order would start; `None` when nothing could be planned or nothing needs planning.
   * @param estimatedCompletionDate
   *   First date by which the hypothetical order would be fully completed; `None` when the order cannot be planned with the known workforce.
   * @param unplannedReasons
   *   Reasons the hypothetical order could not be placed, when planning failed.
   */
  final case class SimulationResult(
      totalHours: Int,
      startDate: Option[DateTime],
      estimatedCompletionDate: Option[DateTime],
      unplannedReasons: List[UnplannedReason],
  )

  /**
   * Estimates when a hypothetical new order could be completed given today's open orders and workforce.
   *
   * @param now
   *   Timestamp of the simulation; planning starts on this day and today's residual working hours are applied like in a real run.
   * @param openOrders
   *   Current orders; only in-progress ones take part in the run, with the same priority policy as real planning.
   * @param employees
   *   Workforce providing daily capacity.
   * @param demand
   *   Work required by the hypothetical order.
   */
  def simulate(
      now: DateTime,
      openOrders: List[Order],
      employees: List[Employee],
      demand: SimulationDemand,
  ): Either[NonEmptyList[PlanningError], SimulationResult] =
    val simulatedOrder = syntheticOrder(now, demand)
    val simulatedOrderId = simulatedOrder.data.id
    val totalHours = simulatedOrder.data.setOfManufacturing.toList.map(_.remainingHours.value).sum
    val openOrderIds = openOrders.collect { case order: InProgressOrder => order.data.id }
    val request = PlanningRequest(UUID.randomUUID().nn, now, PlanningTrigger.DailyPlanning, now, openOrderIds :+ simulatedOrderId)

    SchedulingService.computeSchedule(request, openOrders :+ simulatedOrder, employees).map { outcome =>
      outcome.unplannedOrders.find(_.orderId.equals(simulatedOrderId)) match
        case Some(unplanned) =>
          SimulationResult(totalHours, None, None, unplanned.blockedTasks.toList.map(_.reason))
        case None =>
          val days = outcome.slices.filter(_.orderId.equals(simulatedOrderId)).map(_.day)
          val completion = days.maxByOption(_.getMillis)
          SimulationResult(
            totalHours,
            days.minByOption(_.getMillis),
            // A demand of zero remaining hours produces no slices and is deliverable right away.
            completion.orElse(Some(now.withTimeAtStartOfDay().nn)),
            Nil,
          )
    }
  end simulate

  /**
   * Horizon pushing the synthetic order past every real one in [[PlanningPriorityPolicy]], so the simulation never steals capacity from current
   * commitments.
   */
  private def horizon(now: DateTime): DateTime = now.plusYears(50).nn

  private def syntheticOrder(now: DateTime, demand: SimulationDemand): InProgressOrder =
    val deadline = horizon(now)
    val manufacturings = demand match
      case SimulationDemand.TotalHours(hours) =>
        NonEmptyList.one(hoursOnlyManufacturing(hours, deadline))
      case SimulationDemand.FromManufacturings(selection) =>
        selection.map((template, tasks) => fromTemplate(template, tasks, deadline))
    InProgressOrder(
      OrderData(
        UUID.randomUUID().nn: OrderId,
        "SIMULAZIONE".refineUnsafe[OrderNumber],
        UUID.randomUUID().nn: CustomerId,
        now,
        deadline,
        OrderPriority.Normal,
        manufacturings,
      ),
      deadline,
    )

  private def hoursOnlyManufacturing(hours: TaskHours, deadline: DateTime): ScheduledManufacturing =
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        UUID.randomUUID().nn,
        "SIM".refineUnsafe[ManufacturingCode],
        deadline,
        NonEmptyList.one(PendingTask(UUID.randomUUID().nn: ScheduledTaskId, UUID.randomUUID().nn, hours)),
        ManufacturingDependencies(),
      ),
    )

  /** Instantiates one catalog template as a not-started scheduled manufacturing, like an order created from the catalog would. */
  private def fromTemplate(template: Manufacturing, tasks: NonEmptyList[Task], deadline: DateTime): ScheduledManufacturing =
    val scheduledTasks: NonEmptyList[ScheduledTask] =
      tasks.map(task => PendingTask(UUID.randomUUID().nn: ScheduledTaskId, task.id, task.requiredHours))
    val preferredEmployees = scheduledTasks.toList.flatMap { scheduled =>
      template.defaultEmployees.get(scheduled.taskId).map(scheduled.id -> _)
    }.toMap
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        UUID.randomUUID().nn,
        template.code,
        deadline,
        scheduledTasks,
        template.dependencies,
        taskPreferredEmployees = preferredEmployees,
      ),
    )
end OrderSimulationService
