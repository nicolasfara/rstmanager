package io.github.nicolasfara.rstmanager.work.domain.order

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.InProgressTask

import cats.Id
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
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
  private val secondManufacturingId: ScheduledManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000108").nn

  private def orderData(): OrderData =
    orderData(NonEmptyList.one(manufacturing()))

  private def orderData(manufacturings: NonEmptyList[ScheduledManufacturing]): OrderData =
    OrderData(
      orderId,
      "ORD-101".refineUnsafe[OrderNumber],
      customerId,
      day,
      nextDay,
      OrderPriority.Normal,
      manufacturings,
    )

  private def manufacturing(): ScheduledManufacturing =
    manufacturing(manufacturingId, NonEmptyList.one(task()))

  private def manufacturing(tasks: NonEmptyList[ScheduledTask]): ScheduledManufacturing =
    manufacturing(manufacturingId, tasks)

  private def manufacturing(
      id: ScheduledManufacturingId,
      tasks: NonEmptyList[ScheduledTask],
  ): ScheduledManufacturing =
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        id,
        "MFG-TEST".refineUnsafe[ManufacturingCode],
        nextDay,
        tasks,
        ManufacturingDependencies(),
      ),
    )

  private def task(): InProgressTask =
    task(taskId)

  private def task(id: ScheduledTaskId): InProgressTask =
    InProgressTask(id, templateTaskId, TaskHours(8), TaskHours(0))

  private def twoManufacturingsData(): OrderData =
    orderData(NonEmptyList.of(manufacturing(), manufacturing(secondManufacturingId, NonEmptyList.one(task(otherTaskId)))))

  private def run(
      command: Command,
      state: Order,
  ): EdomatonResult[Order, io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent, OrderError, Notification] =
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
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "delegate nested task commands through the order aggregate" in:
    val data = orderData(NonEmptyList.one(manufacturing(tasks = NonEmptyList.of(task(), task(otherTaskId)))))
    val result = run(Command.CompleteTask(manufacturingId, taskId, TaskHours(8)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        newState.data.id shouldEqual data.id
        events.toChain.toList shouldEqual List(ManufacturingTaskCompleted(manufacturingId, taskId, TaskHours(8)))
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "set a task's absolute progress, completing the manufacturing while keeping the order in progress" in:
    val data = orderData()
    val result = run(Command.SetTaskProgress(manufacturingId, taskId, TaskHours(8)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        events.toChain.toList shouldEqual List(ManufacturingTaskProgressSet(manufacturingId, taskId, TaskHours(8)))
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
        newState.data.setOfManufacturing.head shouldBe a[ScheduledManufacturing.CompletedManufacturing]
      case other => fail(s"Unexpected result: $other")

  it should "change a task's total expected hours" in:
    val data = orderData()
    val result = run(Command.ChangeTaskExpectedHours(manufacturingId, taskId, TaskHours(16)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        events.toChain.toList shouldEqual List(ManufacturingTaskExpectedHoursChanged(manufacturingId, taskId, TaskHours(16)))
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
        newState.data.setOfManufacturing.head.info.tasks.head.expectedHours shouldEqual TaskHours(16)
      case other => fail(s"Unexpected result: $other")

  it should "set and clear a task's preferred employee" in:
    val employeeId = UUID.fromString("00000000-0000-0000-0000-000000000109").nn
    val data = orderData()
    val afterSet = run(Command.SetTaskPreferredEmployee(manufacturingId, taskId, Some(employeeId)), InProgressOrder(data, nextDay))

    val stateWithEmployee = afterSet match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        events.toChain.toList match
          case List(ManufacturingTaskPreferredEmployeeChanged(eventManufacturingId, eventTaskId, eventEmployeeId, _)) =>
            eventManufacturingId shouldEqual manufacturingId
            eventTaskId shouldEqual taskId
            eventEmployeeId shouldEqual Some(employeeId)
          case other => fail(s"Unexpected events: $other")
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
        newState.data.setOfManufacturing.head.info.taskPreferredEmployees shouldEqual Map(taskId -> employeeId)
        newState
      case other => fail(s"Unexpected result: $other")

    run(Command.SetTaskPreferredEmployee(manufacturingId, taskId, None), stateWithEmployee) match
      case EdomatonResult.Accepted(newState: InProgressOrder, _, _) =>
        newState.data.setOfManufacturing.head.info.taskPreferredEmployees shouldBe empty
      case other => fail(s"Unexpected result: $other")

  it should "reject setting the preferred employee of an unknown task" in:
    val employeeId = UUID.fromString("00000000-0000-0000-0000-000000000109").nn
    val unknownTaskId: ScheduledTaskId = UUID.fromString("00000000-0000-0000-0000-00000000010a").nn
    val result = run(Command.SetTaskPreferredEmployee(manufacturingId, unknownTaskId, Some(employeeId)), InProgressOrder(orderData(), nextDay))

    result match
      case EdomatonResult.Rejected(notifications, reasons) =>
        reasons.toChain.toList should matchPattern { case List(ManufacturingError(_)) => }
        notifications shouldBe empty
      case other => fail(s"Unexpected result: $other")

  it should "change a manufacturing work deadline" in:
    val data = orderData()
    val newCompletionDate = nextDay.plusDays(2).nn
    val result = run(Command.ChangeManufacturingCompletionDate(manufacturingId, newCompletionDate), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        events.toChain.toList match
          case List(ManufacturingCompletionDateChanged(eventManufacturingId, eventCompletionDate, _)) =>
            eventManufacturingId shouldEqual manufacturingId
            eventCompletionDate shouldEqual newCompletionDate
          case other => fail(s"Unexpected events: $other")
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
        newState.data.setOfManufacturing.head.info.completionDate shouldEqual newCompletionDate
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

  it should "replace the dependency graph between the order manufacturings" in:
    val data = twoManufacturingsData()
    val dependencies = OrderDependencies.empty.addManufacturingDependencies(secondManufacturingId, Set(manufacturingId))
    val result = run(Command.ChangeManufacturingDependencies(dependencies), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        newState.data.dependencies.dependenciesOf(secondManufacturingId) shouldEqual Set(manufacturingId)
        events.toChain.toList should matchPattern { case List(ManufacturingDependenciesChanged(_, _)) => }
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "reject a manufacturing dependency graph referencing unknown manufacturings" in:
    val unknownId = UUID.fromString("00000000-0000-0000-0000-000000000109").nn
    val data = twoManufacturingsData()
    val dependencies = OrderDependencies.empty.addManufacturingDependencies(secondManufacturingId, Set(unknownId))
    val result = run(Command.ChangeManufacturingDependencies(dependencies), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Rejected(_, reasons) =>
        reasons.toChain.toList shouldEqual List(UnknownManufacturingInDependencies(Set(unknownId)))
      case other => fail(s"Unexpected result: $other")

  it should "reject a cyclic manufacturing dependency graph" in:
    val data = twoManufacturingsData()
    val dependencies = OrderDependencies.empty
      .addManufacturingDependencies(secondManufacturingId, Set(manufacturingId))
      .addManufacturingDependencies(manufacturingId, Set(secondManufacturingId))
    val result = run(Command.ChangeManufacturingDependencies(dependencies), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Rejected(_, reasons) =>
        reasons.toChain.toList should matchPattern { case List(ManufacturingDependencyCycle(_)) => }
      case other => fail(s"Unexpected result: $other")

  it should "replace the task dependency graph of a manufacturing" in:
    val secondTemplateId: TaskId = UUID.fromString("00000000-0000-0000-0000-00000000010a").nn
    val secondTask = InProgressTask(otherTaskId, secondTemplateId, TaskHours(8), TaskHours(0))
    val data = orderData(NonEmptyList.one(manufacturing(NonEmptyList.of(task(), secondTask))))
    val dependencies = ManufacturingDependencies().addTaskDependencies(secondTemplateId, Set(templateTaskId))
    val result = run(Command.ChangeTaskDependencies(manufacturingId, dependencies), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        newState.data.setOfManufacturing.head.info.dependencies.dependenciesOf(secondTemplateId) shouldEqual Set(templateTaskId)
        events.toChain.toList match
          case List(TaskDependenciesChanged(eventManufacturingId, _, _)) => eventManufacturingId shouldEqual manufacturingId
          case other => fail(s"Unexpected events: $other")
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "reject task dependencies referencing tasks outside the manufacturing" in:
    val unknownTemplateId: TaskId = UUID.fromString("00000000-0000-0000-0000-00000000010b").nn
    val data = orderData()
    val dependencies = ManufacturingDependencies().addTaskDependencies(templateTaskId, Set(unknownTemplateId))
    val result = run(Command.ChangeTaskDependencies(manufacturingId, dependencies), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Rejected(_, reasons) =>
        reasons.toChain.toList shouldEqual List(UnknownTaskInDependencies(manufacturingId, Set(unknownTemplateId)))
      case other => fail(s"Unexpected result: $other")

  it should "reject creating an order whose manufacturing dependency graph is invalid" in:
    val unknownId = UUID.fromString("00000000-0000-0000-0000-00000000010c").nn
    val data = twoManufacturingsData()
      .withDependencies(OrderDependencies.empty.addManufacturingDependencies(manufacturingId, Set(unknownId)))
    val result = run(Command.Create(data, nextDay), NewOrder)

    result match
      case EdomatonResult.Rejected(_, reasons) =>
        reasons.toChain.toList shouldEqual List(UnknownManufacturingInDependencies(Set(unknownId)))
      case other => fail(s"Unexpected result: $other")

  it should "reopen a completed order back to in progress" in:
    val data = orderData()
    val result = run(Command.Reopen, CompletedOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        newState.data shouldEqual data
        newState.promisedDeliveryDate shouldEqual data.deliveryDate
        events.toChain.toList should matchPattern { case List(OrderReactivated(_)) => }
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "reopen a cancelled order back to in progress" in:
    val data = orderData()
    val result = run(Command.Reopen, CancelledOrder(data, nextDay, None))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        newState.data shouldEqual data
        events.toChain.toList should matchPattern { case List(OrderReactivated(_)) => }
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "reject reopening an order that is neither cancelled nor completed" in:
    val data = orderData()
    val result = run(Command.Reopen, InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Rejected(_, reasons) =>
        reasons.toChain.toList shouldEqual List(OnlyCancelledOrCompletedOrdersCanBeReopened)
      case other => fail(s"Unexpected result: $other")

  it should "allow adding a manufacturing after reopening a completed order" in:
    val data = orderData()
    val reopened = run(Command.Reopen, CompletedOrder(data, nextDay)) match
      case EdomatonResult.Accepted(newState: InProgressOrder, _, _) => newState
      case other => fail(s"Unexpected result: $other")
    val added = manufacturing(secondManufacturingId, NonEmptyList.one(task(otherTaskId)))
    val result = run(Command.AddManufacturing(added), reopened)

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, _, _) =>
        newState.data.setOfManufacturing.toList.map(_.info.id) should contain(secondManufacturingId)
      case other => fail(s"Unexpected result: $other")

  it should "create an order carrying manufacturing dependencies" in:
    val data = twoManufacturingsData()
      .withDependencies(OrderDependencies.empty.addManufacturingDependencies(secondManufacturingId, Set(manufacturingId)))
    val result = run(Command.Create(data, nextDay), NewOrder)

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, _, _) =>
        newState.data.dependencies.dependenciesOf(secondManufacturingId) shouldEqual Set(manufacturingId)
      case other => fail(s"Unexpected result: $other")
end OrderServiceTest
