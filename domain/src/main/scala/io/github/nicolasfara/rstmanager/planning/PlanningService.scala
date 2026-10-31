package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.hr.domain.Employee
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }

import cats.Monad
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime

/**
 * Edomata service for the event-sourced [[Planning]] aggregate.
 *
 * One `ComputePlan` command runs a full planning attempt: it starts the attempt on the aggregate, executes [[SchedulingService.computeSchedule]], and
 * concludes the attempt with a single event carrying the whole result (or the reasons it was rejected), stamped with the command time. Downstream
 * contexts are informed through notifications, so, for example, the order service can update work-completion deadlines when planning delays an order.
 *
 * The command carries the open orders and the workforce snapshot because planning combines work demand with HR capacity without reaching into other
 * aggregates; the application layer is responsible for loading them before dispatching the command.
 */
object PlanningService extends Planning.Service[PlanningService.Command, PlanningService.Notification]:
  enum Command derives CanEqual:
    /** Requests a planning attempt for `request`, considering the given orders and workforce. */
    case ComputePlan(request: PlanningRequest, orders: List[Order], employees: List[Employee])

  enum Notification derives CanEqual:
    /** A planning result was computed and recorded for the request. */
    case PlanningCompleted(requestId: PlanningRequestId)

    /** The planning attempt hit terminal lifecycle or validation errors. */
    case PlanningRejected(requestId: PlanningRequestId, errors: NonEmptyList[PlanningError])

    /** Planning moved the computed completion of an order beyond its work deadline. */
    case OrderDelayed(orderId: OrderId, deadline: DateTime, plannedCompletion: DateTime)

    /** Planning computed a manufacturing completion beyond its expected completion date. */
    case ManufacturingDelayed(
        orderId: OrderId,
        manufacturingId: ScheduledManufacturingId,
        expectedCompletionDate: DateTime,
        computedCompletionDate: DateTime,
    )

    /** Planning could not place an order with the known workforce and structural constraints. */
    case OrderUnplanned(unplannedOrder: UnplannedOrder)
  end Notification

  def apply[F[_]: Monad]: App[F, Unit] = App.router { case Command.ComputePlan(request, orders, employees) =>
    App.read.flatMap { context =>
      val now = new DateTime(context.command.time.toEpochMilli)
      for
        inProgress <- App.decide(context.state.request(request))
        concluded <- App.decide(inProgress.conclude(SchedulingService.computeSchedule(request, orders, employees), now))
        _ <- publishOutcome(request, concluded)
      yield ()
    }
  }

  private def publishOutcome[F[_]: Monad](request: PlanningRequest, concluded: Planning): App[F, Unit] = concluded match
    case Planning.CompletedPlanning(_, result, _) =>
      val delayNotifications =
        result.delayedOrders.map(delay => Notification.OrderDelayed(delay.orderId, delay.deadline, delay.plannedCompletion)) ++
          result.delayedManufacturings.map { delay =>
            Notification.ManufacturingDelayed(delay.orderId, delay.manufacturingId, delay.expectedCompletionDate, delay.computedCompletionDate)
          }
      val unplannedNotifications = result.unplannedOrders.map(Notification.OrderUnplanned.apply)
      App.publish((delayNotifications ++ unplannedNotifications :+ Notification.PlanningCompleted(request.id))*)
    case Planning.RejectedPlanning(_, errors, _) => App.publish(Notification.PlanningRejected(request.id, errors))
    case Planning.NewPlanning | Planning.InProgressPlanning(_, _, _, _, _, _) => App.unit
end PlanningService
