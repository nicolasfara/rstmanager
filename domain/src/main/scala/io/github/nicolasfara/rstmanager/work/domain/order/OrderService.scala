package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ManufacturingStatus, ScheduledManufacturing, ScheduledManufacturingId }
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

import cats.Monad
import com.github.nscala_time.time.Imports.DateTime
import edomata.core.Decision
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
    case ChangeDescription(newDescription: Option[String])
    case AddManufacturing(manufacturing: ScheduledManufacturing)
    case RemoveManufacturing(manufacturingId: ScheduledManufacturingId)
    case ChangeManufacturingDescription(manufacturingId: ScheduledManufacturingId, newDescription: Option[String])
    case ChangeManufacturingCompletionDate(manufacturingId: ScheduledManufacturingId, newCompletionDate: DateTime)
    case SetPreferredEmployee(manufacturingId: ScheduledManufacturingId, employeeId: Option[EmployeeId])
    case SetTaskPreferredEmployee(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, employeeId: Option[EmployeeId])
    case ChangeManufacturingStatus(manufacturingId: ScheduledManufacturingId, newStatus: ManufacturingStatus, reason: Option[String])
    case AddManufacturingTask(
        manufacturingId: ScheduledManufacturingId,
        task: ScheduledTask,
        dependsOn: List[TaskId],
        preferredEmployeeId: Option[EmployeeId] = None,
    )
    case RemoveManufacturingTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId)
    case SetTaskProgress(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, completedDuration: TaskDuration)
    case ChangeTaskExpectedHours(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, expectedDuration: TaskDuration)
    case CompleteTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, withDuration: TaskDuration)
    case RevertTask(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId)
    case ChangeManufacturingDependencies(dependencies: OrderDependencies)
    case ChangeTaskDependencies(manufacturingId: ScheduledManufacturingId, dependencies: ManufacturingDependencies)
  end Command

  enum Notification derives CanEqual:
    case SchedulingRecalculationRequested(orderId: OrderId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(data, promisedDeliveryDate) =>
      mustTargetOwnStream(data.id) >> App.state.decide(_.create(data, promisedDeliveryDate)).flatMap(publishSchedulingRecalculation)
    case Command.Suspend(reason) =>
      decideAt((order, now) => order.suspend(reason, now)).flatMap(publishSchedulingRecalculation)
    case Command.Reactivate =>
      decideAt((order, now) => order.reactivate(now)).flatMap(publishSchedulingRecalculation)
    case Command.Complete =>
      decideAt((order, now) => order.complete(now)).flatMap(publishSchedulingRecalculation)
    case Command.Deliver =>
      decideAt((order, now) => order.deliver(now)).flatMap(publishSchedulingRecalculation)
    case Command.Cancel(reason) =>
      decideAt((order, now) => order.cancel(reason, now)).flatMap(publishSchedulingRecalculation)
    case Command.UpdatePromisedDeliveryDate(newPromisedDeliveryDate) =>
      decideAt((order, now) => order.updatePromisedDeliveryDate(newPromisedDeliveryDate, now)).flatMap(publishSchedulingRecalculation)
    case Command.Reopen =>
      decideAt((order, now) => order.reopen(now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangePriority(newPriority) =>
      decideAt((order, now) => order.changePriority(newPriority, now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangeDescription(newDescription) =>
      decideAt((order, now) => order.changeDescription(newDescription, now)).flatMap(publishSchedulingRecalculation)
    case Command.AddManufacturing(manufacturing) =>
      decideAt((order, now) => order.addManufacturing(manufacturing, now)).flatMap(publishSchedulingRecalculation)
    case Command.RemoveManufacturing(manufacturingId) =>
      decideAt((order, now) => order.removeManufacturing(manufacturingId, now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangeManufacturingDescription(manufacturingId, newDescription) =>
      decideAt((order, now) => order.changeManufacturingDescription(manufacturingId, newDescription, now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangeManufacturingCompletionDate(manufacturingId, newCompletionDate) =>
      decideAt((order, now) => order.changeManufacturingCompletionDate(manufacturingId, newCompletionDate, now))
        .flatMap(publishSchedulingRecalculation)
    case Command.SetPreferredEmployee(manufacturingId, employeeId) =>
      decideAt((order, now) => order.changeManufacturingPreferredEmployee(manufacturingId, employeeId, now)).flatMap(publishSchedulingRecalculation)
    case Command.SetTaskPreferredEmployee(manufacturingId, taskId, employeeId) =>
      decideAt((order, now) => order.changeTaskPreferredEmployee(manufacturingId, taskId, employeeId, now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangeManufacturingStatus(manufacturingId, newStatus, reason) =>
      decideAt((order, now) => order.changeManufacturingStatus(manufacturingId, newStatus, reason, now)).flatMap(publishSchedulingRecalculation)
    case Command.AddManufacturingTask(manufacturingId, task, dependsOn, preferredEmployeeId) =>
      decideAt((order, now) => order.addManufacturingTask(manufacturingId, task, dependsOn, preferredEmployeeId, now))
        .flatMap(publishSchedulingRecalculation)
    case Command.RemoveManufacturingTask(manufacturingId, taskId) =>
      decideAt((order, now) => order.removeManufacturingTask(manufacturingId, taskId, now)).flatMap(publishSchedulingRecalculation)
    case Command.SetTaskProgress(manufacturingId, taskId, completedDuration) =>
      decideAt((order, now) => order.setTaskProgress(manufacturingId, taskId, completedDuration, now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangeTaskExpectedHours(manufacturingId, taskId, expectedDuration) =>
      decideAt((order, now) => order.changeTaskExpectedDuration(manufacturingId, taskId, expectedDuration, now))
        .flatMap(publishSchedulingRecalculation)
    case Command.CompleteTask(manufacturingId, taskId, withDuration) =>
      decideAt((order, now) => order.completeTask(manufacturingId, taskId, withDuration, now)).flatMap(publishSchedulingRecalculation)
    case Command.RevertTask(manufacturingId, taskId) =>
      decideAt((order, now) => order.revertTask(manufacturingId, taskId, now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangeManufacturingDependencies(dependencies) =>
      decideAt((order, now) => order.changeManufacturingDependencies(dependencies, now)).flatMap(publishSchedulingRecalculation)
    case Command.ChangeTaskDependencies(manufacturingId, dependencies) =>
      decideAt((order, now) => order.changeTaskDependencies(manufacturingId, dependencies, now)).flatMap(publishSchedulingRecalculation)
  }

  /** Decides on the current state at the instant the command was issued, so events never read the clock. */
  private def decideAt[F[_]: Monad](decision: (Order, DateTime) => Decision[OrderError, OrderEvent, Order]): App[F, Order] =
    App.read.flatMap(context => App.decide(decision(context.state, new DateTime(context.command.time.toEpochMilli))))

  /** Rejects a command whose order belongs to another stream, so the stream id and the stored order id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](orderId: OrderId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == orderId.toString then App.unit else App.reject(OrderError.OrderIdMismatch))

  /** Requests a planning recalculation for the order just changed, identified from its own state rather than by parsing the stream id. */
  private def publishSchedulingRecalculation[F[_]: Monad](order: Order): App[F, Unit] =
    order.orderData.fold(App.unit)(data => App.publish(Notification.SchedulingRecalculationRequested(data.id)))
end OrderService
