package io.github.nicolasfara.rstmanager.work.domain.order

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderService.Command
import io.github.nicolasfara.rstmanager.work.domain.order.OrderService.Notification
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.InProgressTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.Id
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.Empty
import io.github.iltotore.iron.constraint.any.{ DescribedAs, Not }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class OrderServiceTest extends AnyFlatSpecLike:
  private val day: DateTime = DateTime.parse("2026-06-15").nn
  private val nextDay: DateTime = day.plusDays(1).nn
  private val orderId: OrderId = UUID.fromString("00000000-0000-0000-0000-000000000101").nn
  private val customerId: CustomerId = UUID.fromString("00000000-0000-0000-0000-000000000102").nn
  private val manufacturingId: ScheduledManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000103").nn
  private val taskId: ScheduledTaskId = UUID.fromString("00000000-0000-0000-0000-000000000104").nn
  private val templateTaskId: TaskId = UUID.fromString("00000000-0000-0000-0000-000000000105").nn
  private val otherTaskId: ScheduledTaskId = UUID.fromString("00000000-0000-0000-0000-000000000107").nn

  private def orderData(manufacturings: NonEmptyList[ScheduledManufacturing] = NonEmptyList.one(manufacturing())): OrderData =
    OrderData(
      orderId,
      "ORD-101".refineUnsafe[OrderNumber],
      customerId,
      day,
      nextDay,
      OrderPriority.Normal,
      manufacturings,
    )

  private def manufacturing(
      id: ScheduledManufacturingId = manufacturingId,
      tasks: NonEmptyList[ScheduledTask] = NonEmptyList.one(task()),
  ): ScheduledManufacturing =
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        id,
        new DescribedAs[Not[Empty], "The code manufacturing should be not empty"](): ManufacturingCode,
        nextDay,
        tasks,
        ManufacturingDependencies(),
      ),
    )

  private def task(id: ScheduledTaskId = taskId): InProgressTask =
    InProgressTask(id, templateTaskId, TaskHours(8), TaskHours(0))

  private def run(command: Command, state: Order): EdomatonResult[Order, io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent, OrderError, Notification] =
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, orderId.toString, command)
    OrderService[Id].execute(RequestContext(message, state))

  "OrderService" should "create an order through the event-sourced service" in:
    val data = orderData()
    val result = run(Command.Create(data, nextDay), NewOrder)

    result match
      case EdomatonResult.Accepted(newState, events, notifications) =>
        newState shouldEqual InProgressOrder(data, nextDay)
        events.toChain.toList shouldEqual List(OrderCreated(data, nextDay))
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "preserve aggregate rejections for invalid commands" in:
    val result = run(Command.Deliver, NewOrder)

    result match
      case EdomatonResult.Rejected(notifications, reasons) =>
        reasons.toChain.toList shouldEqual List(OrderMustBeCompleted)
        notifications shouldBe empty
      case other => fail(s"Unexpected result: $other")

  it should "route lifecycle commands to aggregate decisions" in:
    val data = orderData()
    val result = run(Command.Suspend(None), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: SuspendedOrder, events, notifications) =>
        newState.data shouldEqual data
        newState.promisedDeliveryDate shouldEqual nextDay
        events.toChain.toList should matchPattern { case List(OrderSuspended(_, None)) => }
        notifications shouldBe empty
      case other => fail(s"Unexpected result: $other")

  it should "delegate nested task commands through the order aggregate" in:
    val data = orderData(NonEmptyList.one(manufacturing(tasks = NonEmptyList.of(task(), task(otherTaskId)))))
    val result = run(Command.CompleteTask(manufacturingId, taskId, TaskHours(8)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        newState.data.id shouldEqual data.id
        events.toChain.toList shouldEqual List(ManufacturingTaskCompleted(manufacturingId, taskId, TaskHours(8)))
        notifications shouldBe empty
      case other => fail(s"Unexpected result: $other")

  it should "surface nested aggregate errors" in:
    val unknownManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000106").nn
    val data = orderData()
    val result = run(Command.CompleteTask(unknownManufacturingId, taskId, TaskHours(8)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Rejected(notifications, reasons) =>
        reasons.toChain.toList shouldEqual List(ManufacturingNotFound(unknownManufacturingId))
        notifications shouldBe empty
      case other => fail(s"Unexpected result: $other")
end OrderServiceTest
