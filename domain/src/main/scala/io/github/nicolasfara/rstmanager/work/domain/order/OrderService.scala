package io.github.nicolasfara.rstmanager.work.domain.order

import java.util.UUID

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
    case SetPreferredEmployee(manufacturingId: ScheduledManufacturingId, employeeId: Option[UUID])
    case SetTaskPreferredEmployee(manufacturingId: ScheduledManufacturingId, taskId: ScheduledTaskId, employeeId: Option[UUID])
    case ChangeManufacturingStatus(manufacturingId: ScheduledManufacturingId, newStatus: ManufacturingStatus, reason: Option[String])
    case AddManufacturingTask(
        manufacturingId: ScheduledManufacturingId,
        task: ScheduledTask,
        dependsOn: List[TaskId],
        preferredEmployeeId: Option[UUID] = None,
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
      App.state.decide(_.create(data, promisedDeliveryDate)).void >> App.publish(Notification.SchedulingRecalculationRequested(data.id))
    case Command.Suspend(reason) =>
      decideAt((order, now) => order.suspend(reason, now)).void >> publishSchedulingRecalculation
    case Command.Reactivate =>
      decideAt((order, now) => order.reactivate(now)).void >> publishSchedulingRecalculation
    case Command.Complete =>
      decideAt((order, now) => order.complete(now)).void >> publishSchedulingRecalculation
    case Command.Deliver =>
      decideAt((order, now) => order.deliver(now)).void >> publishSchedulingRecalculation
    case Command.Cancel(reason) =>
      decideAt((order, now) => order.cancel(reason, now)).void >> publishSchedulingRecalculation
    case Command.UpdatePromisedDeliveryDate(newPromisedDeliveryDate) =>
      decideAt((order, now) => order.updatePromisedDeliveryDate(newPromisedDeliveryDate, now)).void >> publishSchedulingRecalculation
    case Command.Reopen =>
      decideAt((order, now) => order.reopen(now)).void >> publishSchedulingRecalculation
    case Command.ChangePriority(newPriority) =>
      decideAt((order, now) => order.changePriority(newPriority, now)).void >> publishSchedulingRecalculation
    case Command.ChangeDescription(newDescription) =>
      decideAt((order, now) => order.changeDescription(newDescription, now)).void >> publishSchedulingRecalculation
    case Command.AddManufacturing(manufacturing) =>
      decideAt((order, now) => order.addManufacturing(manufacturing, now)).void >> publishSchedulingRecalculation
    case Command.RemoveManufacturing(manufacturingId) =>
      decideAt((order, now) => order.removeManufacturing(manufacturingId, now)).void >> publishSchedulingRecalculation
    case Command.ChangeManufacturingDescription(manufacturingId, newDescription) =>
      decideAt((order, now) => order.changeManufacturingDescription(manufacturingId, newDescription, now)).void >> publishSchedulingRecalculation
    case Command.ChangeManufacturingCompletionDate(manufacturingId, newCompletionDate) =>
      decideAt((order, now) =>
        order.changeManufacturingCompletionDate(manufacturingId, newCompletionDate, now),
      ).void >> publishSchedulingRecalculation
    case Command.SetPreferredEmployee(manufacturingId, employeeId) =>
      decideAt((order, now) => order.changeManufacturingPreferredEmployee(manufacturingId, employeeId, now)).void >> publishSchedulingRecalculation
    case Command.SetTaskPreferredEmployee(manufacturingId, taskId, employeeId) =>
      decideAt((order, now) => order.changeTaskPreferredEmployee(manufacturingId, taskId, employeeId, now)).void >> publishSchedulingRecalculation
    case Command.ChangeManufacturingStatus(manufacturingId, newStatus, reason) =>
      decideAt((order, now) => order.changeManufacturingStatus(manufacturingId, newStatus, reason, now)).void >> publishSchedulingRecalculation
    case Command.AddManufacturingTask(manufacturingId, task, dependsOn, preferredEmployeeId) =>
      decideAt((order, now) =>
        order.addManufacturingTask(manufacturingId, task, dependsOn, preferredEmployeeId, now),
      ).void >> publishSchedulingRecalculation
    case Command.RemoveManufacturingTask(manufacturingId, taskId) =>
      decideAt((order, now) => order.removeManufacturingTask(manufacturingId, taskId, now)).void >> publishSchedulingRecalculation
    case Command.SetTaskProgress(manufacturingId, taskId, completedDuration) =>
      decideAt((order, now) => order.setTaskProgress(manufacturingId, taskId, completedDuration, now)).void >> publishSchedulingRecalculation
    case Command.ChangeTaskExpectedHours(manufacturingId, taskId, expectedDuration) =>
      decideAt((order, now) =>
        order.changeTaskExpectedDuration(manufacturingId, taskId, expectedDuration, now),
      ).void >> publishSchedulingRecalculation
    case Command.CompleteTask(manufacturingId, taskId, withDuration) =>
      decideAt((order, now) => order.completeTask(manufacturingId, taskId, withDuration, now)).void >> publishSchedulingRecalculation
    case Command.RevertTask(manufacturingId, taskId) =>
      decideAt((order, now) => order.revertTask(manufacturingId, taskId, now)).void >> publishSchedulingRecalculation
    case Command.ChangeManufacturingDependencies(dependencies) =>
      decideAt((order, now) => order.changeManufacturingDependencies(dependencies, now)).void >> publishSchedulingRecalculation
    case Command.ChangeTaskDependencies(manufacturingId, dependencies) =>
      decideAt((order, now) => order.changeTaskDependencies(manufacturingId, dependencies, now)).void >> publishSchedulingRecalculation
  }

  /** Decides on the current state at the instant the command was issued, so events never read the clock. */
  private def decideAt[F[_]: Monad](decision: (Order, DateTime) => Decision[OrderError, OrderEvent, Order]): App[F, Order] =
    App.read.flatMap(context => App.decide(decision(context.state, new DateTime(context.command.time.toEpochMilli))))

  private def publishSchedulingRecalculation[F[_]: Monad]: App[F, Unit] =
    App.aggregateId.flatMap(id => App.publish(Notification.SchedulingRecalculationRequested(UUID.fromString(id).nn)))
end OrderService
