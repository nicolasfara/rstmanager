package io.github.nicolasfara.rstmanager.work.domain.order

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderError.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.InProgressTask

import cats.Id
import cats.data.NonEmptyList
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class OrderServiceTest extends AnyFlatSpecLike:
  private val day: DateTime = DateTime.parse("2026-06-15")
  private val nextDay: DateTime = day.plusDays(1).nn
  private val orderId: OrderId = OrderId(UUID.fromString("00000000-0000-0000-0000-000000000101").nn)
  private val customerId: CustomerId = CustomerId(UUID.fromString("00000000-0000-0000-0000-000000000102").nn)
  private val manufacturingId: ScheduledManufacturingId = ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-000000000103").nn)
  private val taskId: ScheduledTaskId = ScheduledTaskId(UUID.fromString("00000000-0000-0000-0000-000000000104").nn)
  private val templateTaskId: TaskId = TaskId(UUID.fromString("00000000-0000-0000-0000-000000000105").nn)
  private val otherTaskId: ScheduledTaskId = ScheduledTaskId(UUID.fromString("00000000-0000-0000-0000-000000000107").nn)
  private val secondManufacturingId: ScheduledManufacturingId = ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-000000000108").nn)

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
    InProgressTask(id, templateTaskId, TaskDuration(8), TaskDuration(0))

  private def twoManufacturingsData(): OrderData =
    orderData(NonEmptyList.of(manufacturing(), manufacturing(secondManufacturingId, NonEmptyList.one(task(otherTaskId)))))

  private def run(
      command: Command,
      state: Order,
  ): EdomatonResult[Order, io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent, OrderError, Notification] =
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, orderId.toString, command)
    OrderService[Id].execute(RequestContext(message, state))

  private def rejectionOf(
      result: EdomatonResult[Order, io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent, OrderError, Notification],
  ): List[OrderError] =
    result match
      case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
      case other => fail(s"Expected a rejection, got: $other")

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
    val result = run(Command.CompleteTask(manufacturingId, taskId, TaskDuration(8)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        newState.data.id shouldEqual data.id
        events.toChain.toList match
          case List(ManufacturingTaskCompleted(eventManufacturingId, eventTaskId, _, Some(_))) =>
            (eventManufacturingId, eventTaskId) shouldEqual (manufacturingId, taskId)
          case other => fail(s"Unexpected events: $other")
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "set a task's absolute progress, completing the manufacturing while keeping the order in progress" in:
    val data = orderData()
    val result = run(Command.SetTaskProgress(manufacturingId, taskId, TaskDuration(8)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        events.toChain.toList match
          case List(ManufacturingTaskProgressSet(eventManufacturingId, eventTaskId, _, Some(_))) =>
            (eventManufacturingId, eventTaskId) shouldEqual (manufacturingId, taskId)
          case other => fail(s"Unexpected events: $other")
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
        newState.data.setOfManufacturing.head shouldBe a[ScheduledManufacturing.CompletedManufacturing]
      case other => fail(s"Unexpected result: $other")

  it should "change a task's total expected hours" in:
    val data = orderData()
    val result = run(Command.ChangeTaskExpectedHours(manufacturingId, taskId, TaskDuration(16)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, events, notifications) =>
        events.toChain.toList match
          case List(ManufacturingTaskExpectedHoursChanged(eventManufacturingId, eventTaskId, _, Some(_))) =>
            (eventManufacturingId, eventTaskId) shouldEqual (manufacturingId, taskId)
          case other => fail(s"Unexpected events: $other")
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
        newState.data.setOfManufacturing.head.info.tasks.head.expectedDuration shouldEqual TaskDuration(16)
      case other => fail(s"Unexpected result: $other")

  it should "set and clear a task's preferred employee" in:
    val employeeId = EmployeeId(UUID.fromString("00000000-0000-0000-0000-000000000109").nn)
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
    val employeeId = EmployeeId(UUID.fromString("00000000-0000-0000-0000-000000000109").nn)
    val unknownTaskId: ScheduledTaskId = ScheduledTaskId(UUID.fromString("00000000-0000-0000-0000-00000000010a").nn)
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
    val unknownManufacturingId = ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-000000000106").nn)
    val data = orderData()
    val result = run(Command.CompleteTask(unknownManufacturingId, taskId, TaskDuration(8)), InProgressOrder(data, nextDay))

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
    val unknownId = ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-000000000109").nn)
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
    val secondTemplateId: TaskId = TaskId(UUID.fromString("00000000-0000-0000-0000-00000000010a").nn)
    val secondTask = InProgressTask(otherTaskId, secondTemplateId, TaskDuration(8), TaskDuration(0))
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
    val unknownTemplateId: TaskId = TaskId(UUID.fromString("00000000-0000-0000-0000-00000000010b").nn)
    val data = orderData()
    val dependencies = ManufacturingDependencies().addTaskDependencies(templateTaskId, Set(unknownTemplateId))
    val result = run(Command.ChangeTaskDependencies(manufacturingId, dependencies), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Rejected(_, reasons) =>
        reasons.toChain.toList shouldEqual List(UnknownTaskInDependencies(manufacturingId, Set(unknownTemplateId)))
      case other => fail(s"Unexpected result: $other")

  it should "reject creating an order whose manufacturing dependency graph is invalid" in:
    val unknownId = ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-00000000010c").nn)
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

  it should "stamp events with the command time instead of reading the clock" in:
    val commandTime = new DateTime(Instant.parse("2026-06-14T12:00:00Z").nn.toEpochMilli)
    val data = orderData(NonEmptyList.one(manufacturing(tasks = NonEmptyList.of(task(), task(otherTaskId)))))

    run(Command.Suspend(None), InProgressOrder(data, nextDay)) match
      case EdomatonResult.Accepted(newState: SuspendedOrder, _, _) => newState.pausedOn.getMillis shouldEqual commandTime.getMillis
      case other => fail(s"Unexpected result: $other")
    run(Command.CompleteTask(manufacturingId, taskId, TaskDuration(8)), InProgressOrder(data, nextDay)) match
      case EdomatonResult.Accepted(_, events, _) =>
        events.toChain.toList match
          case List(ManufacturingTaskCompleted(_, _, _, Some(occurredOn))) => occurredOn.getMillis shouldEqual commandTime.getMillis
          case other => fail(s"Unexpected events: $other")
      case other => fail(s"Unexpected result: $other")

  it should "keep a suspended order suspended when nested manufacturings and tasks are edited" in:
    val employeeId = EmployeeId(UUID.fromString("00000000-0000-0000-0000-000000000109").nn)
    val pausedOn = day.plusHours(3).nn
    val reason = Some("Waiting for materials".refineUnsafe[SuspensionReason])
    val suspended =
      SuspendedOrder(orderData(NonEmptyList.one(manufacturing(tasks = NonEmptyList.of(task(), task(otherTaskId))))), nextDay, pausedOn, reason)
    val commands = List(
      Command.ChangeManufacturingDescription(manufacturingId, Some("Updated")),
      Command.ChangeManufacturingCompletionDate(manufacturingId, nextDay.plusDays(3).nn),
      Command.ChangeManufacturingStatus(manufacturingId, ManufacturingStatus.Paused, None),
      Command.SetPreferredEmployee(manufacturingId, Some(employeeId)),
      Command.SetTaskPreferredEmployee(manufacturingId, taskId, Some(employeeId)),
      Command.SetTaskProgress(manufacturingId, taskId, TaskDuration(4)),
      Command.ChangeTaskExpectedHours(manufacturingId, taskId, TaskDuration(12)),
      Command.CompleteTask(manufacturingId, taskId, TaskDuration(8)),
      Command.RemoveManufacturingTask(manufacturingId, otherTaskId),
      Command.ChangeTaskDependencies(manufacturingId, ManufacturingDependencies()),
      Command.ChangeManufacturingDependencies(OrderDependencies.empty),
    )

    commands.foreach { command =>
      withClue(s"$command: ") {
        run(command, suspended) match
          case EdomatonResult.Accepted(newState: SuspendedOrder, _, _) =>
            newState.pausedOn shouldEqual pausedOn
            newState.reason shouldEqual reason
            newState.promisedDeliveryDate shouldEqual nextDay
          case other => fail(s"Unexpected result: $other")
      }
    }

  it should "complete the last task of an order, completing its manufacturing while keeping the order in progress" in:
    val data = orderData()
    val result = run(Command.CompleteTask(manufacturingId, taskId, TaskDuration(8)), InProgressOrder(data, nextDay))

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, _, notifications) =>
        newState.data.setOfManufacturing.head shouldBe a[ScheduledManufacturing.CompletedManufacturing]
        notifications.toList shouldEqual List(Notification.SchedulingRecalculationRequested(orderId))
      case other => fail(s"Unexpected result: $other")

  it should "rebuild the same state on every replay, taking lifecycle timestamps from the events" in:
    val completedOn = day.plusHours(10).nn
    val events = List(OrderCreated(orderData(), nextDay), ManufacturingTaskCompleted(manufacturingId, taskId, TaskDuration(8), Some(completedOn)))
    def replay = events.foldLeft(Order.initial.validNec[OrderError])((state, event) => state.andThen(Order.transition(event)))

    replay shouldEqual replay
    replay.toOption.collect { case order: InProgressOrder => order.data.setOfManufacturing.head } match
      case Some(ScheduledManufacturing.CompletedManufacturing(info, startedAt, completedAt)) =>
        startedAt shouldEqual completedOn
        completedAt shouldEqual completedOn
        info.tasks.head match
          case ScheduledTask.CompletedTask(_, _, _, _, taskCompletedOn) => taskCompletedOn shouldEqual completedOn
          case other => fail(s"Unexpected task: $other")
      case other => fail(s"Unexpected manufacturing: $other")

  it should "fold legacy task events without a timestamp deterministically, using the order creation date" in:
    val legacy = ManufacturingTaskCompleted(manufacturingId, taskId, TaskDuration(8))
    val state = Order.transition(legacy)(InProgressOrder(orderData(), nextDay))

    state.toOption.collect { case order: InProgressOrder => order.data.setOfManufacturing.head } match
      case Some(ScheduledManufacturing.CompletedManufacturing(_, startedAt, completedAt)) =>
        startedAt shouldEqual day
        completedAt shouldEqual day
      case other => fail(s"Unexpected manufacturing: $other")

  it should "replay a stored nested event that no longer applies without failing, leaving the content unchanged" in:
    val order = InProgressOrder(orderData(), nextDay)
    val unknownTaskId: ScheduledTaskId = ScheduledTaskId(UUID.fromString("00000000-0000-0000-0000-000000000110").nn)

    Order.transition(ManufacturingTaskCompleted(manufacturingId, unknownTaskId, TaskDuration(8), None))(order).toOption shouldEqual Some(order)

  it should "replay a stored dependency graph as recorded, without re-validating it under the current rules" in:
    val secondTemplateId: TaskId = TaskId(UUID.fromString("00000000-0000-0000-0000-000000000111").nn)
    val cyclic = ManufacturingDependencies()
      .addTaskDependencies(templateTaskId, Set(secondTemplateId))
      .addTaskDependencies(secondTemplateId, Set(templateTaskId))

    Order.transition(TaskDependenciesChanged(manufacturingId, cyclic, day))(InProgressOrder(orderData(), nextDay)).toOption match
      case Some(replayed: InProgressOrder) =>
        replayed.data.setOfManufacturing.head.info.dependencies.dependenciesOf(templateTaskId) shouldEqual Set(secondTemplateId)
      case other => fail(s"Unexpected replay: $other")

  it should "reject cancelling a completed or delivered order" in:
    val data = orderData()
    List(CompletedOrder(data, nextDay), DeliveredOrder(data, nextDay, nextDay)).foreach { state =>
      run(Command.Cancel(None), state) match
        case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList shouldEqual List(OrderMustBeInProgressOrPaused)
        case other => fail(s"Unexpected result: $other")
    }

  it should "reject an order whose manufacturings or tasks share an id" in:
    val sameManufacturingTwice = orderData(NonEmptyList.of(manufacturing(), manufacturing(manufacturingId, NonEmptyList.one(task(otherTaskId)))))
    val sameTaskInTwoManufacturings = orderData(NonEmptyList.of(manufacturing(), manufacturing(secondManufacturingId, NonEmptyList.one(task()))))

    rejectionOf(run(Command.Create(sameManufacturingTwice, nextDay), NewOrder)) shouldEqual List(DuplicateManufacturingIds(Set(manufacturingId)))
    rejectionOf(run(Command.Create(sameTaskInTwoManufacturings, nextDay), NewOrder)) shouldEqual List(DuplicateTaskIds(Set(taskId)))

  it should "reject scheduling the same catalog task twice in one manufacturing" in:
    // `task()` and `task(otherTaskId)` are distinct scheduled tasks of the same catalog task (`templateTaskId`).
    val repeated = orderData(NonEmptyList.one(manufacturing(NonEmptyList.of(task(), task(otherTaskId)))))
    val repeatedTask = InProgressTask(otherTaskId, templateTaskId, TaskDuration(8), TaskDuration(0))

    rejectionOf(run(Command.Create(repeated, nextDay), NewOrder)) shouldEqual List(RepeatedCatalogTasks(manufacturingId, Set(templateTaskId)))
    rejectionOf(run(Command.AddManufacturingTask(manufacturingId, repeatedTask, Nil), InProgressOrder(orderData(), nextDay))) shouldEqual
      List(RepeatedCatalogTasks(manufacturingId, Set(templateTaskId)))

  it should "not block other changes to an order that already repeats a catalog task" in:
    val legacy = InProgressOrder(orderData(NonEmptyList.one(manufacturing(NonEmptyList.of(task(), task(otherTaskId))))), nextDay)
    val thirdTaskId: ScheduledTaskId = ScheduledTaskId(UUID.fromString("00000000-0000-0000-0000-000000000112").nn)

    run(Command.AddManufacturing(manufacturing(secondManufacturingId, NonEmptyList.one(task(thirdTaskId)))), legacy) should matchPattern {
      case EdomatonResult.Accepted(_: InProgressOrder, _, _) =>
    }

  it should "reject adding a manufacturing whose id or task ids are already in the order" in:
    val order = InProgressOrder(orderData(), nextDay)

    rejectionOf(run(Command.AddManufacturing(manufacturing(manufacturingId, NonEmptyList.one(task(otherTaskId)))), order)) shouldEqual
      List(DuplicateManufacturingIds(Set(manufacturingId)))
    rejectionOf(run(Command.AddManufacturing(manufacturing(secondManufacturingId, NonEmptyList.one(task()))), order)) shouldEqual
      List(DuplicateTaskIds(Set(taskId)))

  it should "validate an added task: new id, known prerequisites, no cycle" in:
    val secondTemplateId: TaskId = TaskId(UUID.fromString("00000000-0000-0000-0000-00000000010d").nn)
    val unknownTemplateId: TaskId = TaskId(UUID.fromString("00000000-0000-0000-0000-00000000010e").nn)
    val newTask = InProgressTask(otherTaskId, secondTemplateId, TaskDuration(8), TaskDuration(0))
    val order = InProgressOrder(orderData(), nextDay)
    val withDependency = InProgressOrder(
      orderData(
        NonEmptyList.one(
          manufacturing(NonEmptyList.one(task())).withDependencies(
            ManufacturingDependencies().addTaskDependencies(templateTaskId, Set(secondTemplateId)),
          ),
        ),
      ),
      nextDay,
    )

    run(Command.AddManufacturingTask(manufacturingId, newTask, List(templateTaskId)), order) should matchPattern {
      case EdomatonResult.Accepted(_: InProgressOrder, _, _) =>
    }
    rejectionOf(run(Command.AddManufacturingTask(manufacturingId, task(), Nil), order)) shouldEqual List(DuplicateTaskIds(Set(taskId)))
    rejectionOf(run(Command.AddManufacturingTask(manufacturingId, newTask, List(unknownTemplateId)), order)) shouldEqual
      List(UnknownTaskInDependencies(manufacturingId, Set(unknownTemplateId)))
    rejectionOf(run(Command.AddManufacturingTask(manufacturingId, newTask, List(templateTaskId)), withDependency)) should matchPattern {
      case List(TaskDependencyCycle(_, _)) =>
    }

  it should "reject removing an unknown manufacturing or the last one" in:
    val unknownId: ScheduledManufacturingId = ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-00000000010f").nn)

    rejectionOf(run(Command.RemoveManufacturing(unknownId), InProgressOrder(twoManufacturingsData(), nextDay))) shouldEqual
      List(ManufacturingNotFound(unknownId))
    rejectionOf(run(Command.RemoveManufacturing(manufacturingId), InProgressOrder(orderData(), nextDay))) shouldEqual
      List(CannotRemoveLastManufacturing)
    run(Command.RemoveManufacturing(manufacturingId), InProgressOrder(twoManufacturingsData(), nextDay)) match
      case EdomatonResult.Accepted(newState: InProgressOrder, _, _) =>
        newState.data.setOfManufacturing.map(_.info.id).toList shouldEqual List(secondManufacturingId)
      case other => fail(s"Unexpected result: $other")

  it should "reject an order sent to another order's stream" in:
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, customerId.toString, Command.Create(orderData(), nextDay))

    rejectionOf(OrderService[Id].execute(RequestContext(message, NewOrder))) shouldEqual List(OrderIdMismatch)

  it should "create an order carrying manufacturing dependencies" in:
    val data = twoManufacturingsData()
      .withDependencies(OrderDependencies.empty.addManufacturingDependencies(secondManufacturingId, Set(manufacturingId)))
    val result = run(Command.Create(data, nextDay), NewOrder)

    result match
      case EdomatonResult.Accepted(newState: InProgressOrder, _, _) =>
        newState.data.dependencies.dependenciesOf(secondManufacturingId) shouldEqual Set(manufacturingId)
      case other => fail(s"Unexpected result: $other")
end OrderServiceTest
