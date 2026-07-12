package io.github.nicolasfara.rstmanager.work.domain.order

import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.Monad
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*

/** Edomata service for the event-sourced `Order` aggregate. */
object OrderService extends Order.Service[OrderService.Command, OrderService.Notification]:
  enum Command derives CanEqual:
    case Create(data: OrderData, promisedDeliveryDate: DateTime)
    case Suspend(reason: Option[String :| SuspensionReason])
    case Reactivate
    case Complete
    case Deliver
    case Cancel(reason: Option[String :| CancellationReason])
    case UpdatePromisedDeliveryDate(newPromisedDeliveryDate: DateTime)
    case Reopen
    case ChangePriority(newPriority: OrderPriority)
    case AddManufacturing(manufacturing: ScheduledManufacturing)
    case RemoveManufacturing(manufacturingId: ScheduledManufacturingId)
    case CompleteTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, withHours: TaskHours)
    case RevertTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId)

  enum Notification derives CanEqual:
    case SchedulingRecalculationRequested(orderId: OrderId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(data, promisedDeliveryDate) =>
      App.state.decide(_.create(data, promisedDeliveryDate)).void >> App.publish(Notification.SchedulingRecalculationRequested(data.id))
    case Command.Suspend(reason) =>
      App.state.decide(_.suspend(reason)).void >> publishSchedulingRecalculation
    case Command.Reactivate =>
      App.state.decide(_.reactivate).void >> publishSchedulingRecalculation
    case Command.Complete =>
      App.state.decide(_.complete).void >> publishSchedulingRecalculation
    case Command.Deliver =>
      App.state.decide(_.deliver).void >> publishSchedulingRecalculation
    case Command.Cancel(reason) =>
      App.state.decide(_.cancel(reason)).void >> publishSchedulingRecalculation
    case Command.UpdatePromisedDeliveryDate(newPromisedDeliveryDate) =>
      App.state.decide(_.updatePromisedDeliveryDate(newPromisedDeliveryDate)).void >> publishSchedulingRecalculation
    case Command.Reopen =>
      App.state.decide(_.reopen).void >> publishSchedulingRecalculation
    case Command.ChangePriority(newPriority) =>
      App.state.decide(_.changePriority(newPriority)).void >> publishSchedulingRecalculation
    case Command.AddManufacturing(manufacturing) =>
      App.state.decide(_.addManufacturing(manufacturing)).void >> publishSchedulingRecalculation
    case Command.RemoveManufacturing(manufacturingId) =>
      App.state.decide(_.removeManufacturing(manufacturingId)).void >> publishSchedulingRecalculation
    case Command.CompleteTask(manufacturingId, taskId, withHours) =>
      App.state.decide(_.completeTask(manufacturingId, taskId, withHours)).void >> publishSchedulingRecalculation
    case Command.RevertTask(manufacturingId, taskId) =>
      App.state.decide(_.revertTask(manufacturingId, taskId)).void >> publishSchedulingRecalculation
  }

  private def publishSchedulingRecalculation[F[_]: Monad]: App[F, Unit] =
    App.aggregateId.flatMap(id => App.publish(Notification.SchedulingRecalculationRequested(UUID.fromString(id).nn)))
end OrderService
