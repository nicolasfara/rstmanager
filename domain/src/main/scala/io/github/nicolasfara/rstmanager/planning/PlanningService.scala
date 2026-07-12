package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.hr.domain.Employee
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime

/**
 * Edomata service for the event-sourced [[Planning]] aggregate.
 *
 * One `ComputePlan` command runs a full planning attempt: it starts the attempt on the aggregate, executes [[SchedulingService.computeSchedule]],
 * records every produced fact (task slices, delays, warnings) as planning events, and finally completes or rejects the attempt. Downstream contexts
 * are informed through notifications, so, for example, the order service can update promised delivery dates when planning delays an order.
 *
 * The command carries the open orders and the workforce snapshot because planning combines work demand with HR capacity without reaching into other
 * aggregates; the application layer is responsible for loading them before dispatching the command.
 */
object PlanningService extends Planning.Service[PlanningService.Command, PlanningService.Notification]:
  enum Command derives CanEqual:
    /** Requests a planning attempt for `request`, considering the given orders and workforce. */
    case ComputePlan(request: PlanningRequest, orders: List[Order], employees: List[Employee])

  enum Notification derives CanEqual:
    /** A feasible schedule was computed and recorded for the request. */
    case PlanningCompleted(requestId: PlanningRequestId)

    /** No feasible schedule exists for the request under the current constraints. */
    case PlanningRejected(requestId: PlanningRequestId, errors: NonEmptyList[PlanningError])

    /** Planning moved the promised delivery date of an order beyond its expected delivery date. */
    case OrderDelayed(orderId: OrderId, expectedDeliveryDate: DateTime, promisedDeliveryDate: DateTime)

    /** Planning computed a manufacturing completion beyond its expected completion date. */
    case ManufacturingDelayed(
        orderId: OrderId,
        manufacturingId: ScheduledManufacturingId,
        expectedCompletionDate: DateTime,
        computedCompletionDate: DateTime,
    )

  def apply[F[_]: Monad]: App[F, Unit] = App.router { case Command.ComputePlan(request, orders, employees) =>
    // `App.state` always reads the state the command arrived on, so every decision after the first
    // is taken on the state returned by the previous one.
    App.state.decide(_.request(request)).flatMap { inProgress =>
      SchedulingService.computeSchedule(request, orders, employees) match
        case Left(errors) => rejectPlan(inProgress, request, errors)
        case Right(outcome) => completePlan(inProgress, request, outcome)
    }
  }

  private def rejectPlan[F[_]: Monad](planning: Planning, request: PlanningRequest, errors: NonEmptyList[PlanningError]): App[F, Unit] =
    App.decide(planning.reject(errors)).void >> App.publish(Notification.PlanningRejected(request.id, errors))

  private def completePlan[F[_]: Monad](planning: Planning, request: PlanningRequest, outcome: SchedulingOutcome): App[F, Unit] =
    val delayNotifications =
      outcome.delayedOrders.map(delay => Notification.OrderDelayed(delay.orderId, delay.expectedDeliveryDate, delay.promisedDeliveryDate)) ++
        outcome.delayedManufacturings.map { delay =>
          Notification.ManufacturingDelayed(delay.orderId, delay.manufacturingId, delay.expectedCompletionDate, delay.computedCompletionDate)
        }
    for
      afterSlices <- outcome.slices.foldM(planning)((state, slice) => App.decide(state.assignTaskSlice(slice)))
      afterOrderDelays <- outcome.delayedOrders.foldM(afterSlices)((state, delay) => App.decide(state.delayOrder(delay)))
      afterManufacturingDelays <- outcome.delayedManufacturings.foldM(afterOrderDelays)((state, delay) => App.decide(state.delayManufacturing(delay)))
      afterWarnings <- outcome.warnings.foldM(afterManufacturingDelays)((state, warning) => App.decide(state.raiseWarning(warning)))
      _ <- App.decide(afterWarnings.complete)
      _ <- App.publish(delayNotifications :+ Notification.PlanningCompleted(request.id)*)
    yield ()
end PlanningService
