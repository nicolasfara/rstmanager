package io.github.nicolasfara.rstmanager.planning

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.planning.Planning.CompletedPlanning
import io.github.nicolasfara.rstmanager.planning.PlanningService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{
  ScheduledManufacturing,
  ScheduledManufacturingId,
  ScheduledManufacturingInfo,
}
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask

import cats.Id
import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class SchedulingServiceTest extends AnyFlatSpecLike:
  private val monday: DateTime = DateTime.parse("2026-06-15").nn
  private val tuesday: DateTime = monday.plusDays(1).nn
  private val wednesday: DateTime = monday.plusDays(2).nn
  private val friday: DateTime = monday.plusDays(4).nn

  private val employeeA: EmployeeId = UUID.fromString("00000000-0000-0000-0000-0000000000a1").nn
  private val employeeB: EmployeeId = UUID.fromString("00000000-0000-0000-0000-0000000000b2").nn
  private val orderId: OrderId = UUID.fromString("00000000-0000-0000-0000-000000000002").nn
  private val otherOrderId: OrderId = UUID.fromString("00000000-0000-0000-0000-000000000003").nn
  private val manufacturingId: ScheduledManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000004").nn

  private def employee(id: EmployeeId): Employee =
    employee(id, 40, Nil, Contract.FullTime(monday.minusYears(1).nn))

  private def employee(id: EmployeeId, overrides: List[HoursOverride]): Employee =
    employee(id, 40, overrides, Contract.FullTime(monday.minusYears(1).nn))

  private def employee(id: EmployeeId, contract: Contract): Employee =
    employee(id, 40, Nil, contract)

  private def employee(
      id: EmployeeId,
      weeklyHours: Int,
      overrides: List[HoursOverride],
      contract: Contract,
  ): Employee =
    Employee(
      id,
      EmployeeInfo("Mario".refineUnsafe[Name], "Rossi".refineUnsafe[Surname]),
      contract,
      BudgetHours(WeeklyHours.applyUnsafe(weeklyHours), overrides),
    )

  private def pendingTask(templateId: TaskId, hours: Int): PendingTask =
    PendingTask(UUID.randomUUID().nn: ScheduledTaskId, templateId, TaskHours.applyUnsafe(hours))

  private def manufacturing(
      id: ScheduledManufacturingId,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
  ): ScheduledManufacturing =
    manufacturing(id, completionDate, tasks, ManufacturingDependencies())

  private def manufacturing(
      id: ScheduledManufacturingId,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies,
  ): ScheduledManufacturing =
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        id,
        "MFG-TEST".refineUnsafe[ManufacturingCode],
        completionDate,
        tasks,
        dependencies,
      ),
    )

  private def order(
      id: OrderId,
      deliveryDate: DateTime,
      manufacturings: NonEmptyList[ScheduledManufacturing],
  ): InProgressOrder =
    order(id, deliveryDate, manufacturings, OrderPriority.Normal)

  private def order(
      id: OrderId,
      deliveryDate: DateTime,
      manufacturings: NonEmptyList[ScheduledManufacturing],
      priority: OrderPriority,
  ): InProgressOrder =
    order(id, deliveryDate, deliveryDate, manufacturings, priority)

  private def order(
      id: OrderId,
      deliveryDate: DateTime,
      workDeadline: DateTime,
      manufacturings: NonEmptyList[ScheduledManufacturing],
      priority: OrderPriority,
  ): InProgressOrder =
    InProgressOrder(
      OrderData(
        id,
        s"ORD-$id".refineUnsafe[OrderNumber],
        UUID.randomUUID().nn: CustomerId,
        monday.minusDays(7).nn,
        deliveryDate,
        priority,
        manufacturings,
      ),
      workDeadline,
    )

  private def request(start: DateTime, orderIds: List[OrderId]): PlanningRequest =
    PlanningRequest(
      UUID.fromString("00000000-0000-0000-0000-000000000005").nn,
      start,
      PlanningTrigger.DailyPlanning,
      monday,
      orderIds,
    )

  private def scheduleOrFail(
      planningRequest: PlanningRequest,
      orders: List[Order],
      employees: List[Employee],
  ): SchedulingOutcome =
    SchedulingService.computeSchedule(planningRequest, orders, employees) match
      case Right(outcome) => outcome
      case Left(errors) => fail(s"Expected a feasible schedule, got: ${errors.toList.mkString(", ")}")

  "SchedulingService" should "schedule a task on a single day when the daily capacity suffices" in:
    val task = pendingTask(UUID.randomUUID().nn, 6)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(slice => (slice.day, slice.candidateEmployee.assignedHours.value, slice.remainingHoursAfterSlice.value)) shouldEqual
      List((monday, 6, 0))
    outcome.delayedOrders shouldBe empty
    outcome.delayedManufacturings shouldBe empty

  it should "split a task over consecutive days when it exceeds the daily capacity" in:
    val task = pendingTask(UUID.randomUUID().nn, 20)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(slice => (slice.day, slice.candidateEmployee.assignedHours.value, slice.remainingHoursAfterSlice.value)) shouldEqual
      List((monday, 8, 12), (tuesday, 8, 4), (wednesday, 4, 0))

  it should "split a task over multiple employees on the same day" in:
    val task = pendingTask(UUID.randomUUID().nn, 12)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA), employee(employeeB)))

    outcome.slices.map(_.day).distinct shouldEqual List(monday)
    outcome.slices.map(_.candidateEmployee.assignedHours.value).sum shouldEqual 12
    outcome.slices.map(_.candidateEmployee.employeeId).toSet shouldEqual Set(employeeA, employeeB)

  it should "schedule a dependent task starting the day after its prerequisite completes" in:
    val prerequisiteTemplate: TaskId = UUID.randomUUID().nn
    val dependentTemplate: TaskId = UUID.randomUUID().nn
    val prerequisite = pendingTask(prerequisiteTemplate, 8)
    val dependent = pendingTask(dependentTemplate, 4)
    val dependencies = ManufacturingDependencies().addTaskDependencies(dependentTemplate, Set(prerequisiteTemplate))
    val theOrder =
      order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.of(dependent, prerequisite), dependencies)))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA), employee(employeeB)))

    val sliceByTask = outcome.slices.groupBy(_.taskId)
    sliceByTask(prerequisite.id).map(_.day) shouldEqual List(monday)
    sliceByTask(dependent.id).map(_.day) shouldEqual List(tuesday)

  it should "skip Saturdays and Sundays" in:
    val task = pendingTask(UUID.randomUUID().nn, 16)
    val nextMonday = monday.plusDays(7).nn
    val theOrder = order(orderId, nextMonday, NonEmptyList.one(manufacturing(manufacturingId, nextMonday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(friday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(_.day) shouldEqual List(friday, nextMonday)

  it should "skip employee vacation days" in:
    val task = pendingTask(UUID.randomUUID().nn, 8)
    val vacation = VacationOverride.createVacationOverride(monday, tuesday).toOption.getOrElse(fail("invalid vacation"))
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA, overrides = List(vacation))))

    outcome.slices.map(_.day) shouldEqual List(tuesday)

  it should "report order and manufacturing delays when work overruns the expected dates" in:
    val task = pendingTask(UUID.randomUUID().nn, 20)
    val theOrder = order(orderId, monday, NonEmptyList.one(manufacturing(manufacturingId, monday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.delayedManufacturings shouldEqual List(DelayedManufacturing(orderId, manufacturingId, monday, wednesday))
    outcome.delayedOrders shouldEqual List(DelayedOrder(orderId, monday, wednesday))

  it should "report order delays against the work deadline before the customer delivery date" in:
    val task = pendingTask(UUID.randomUUID().nn, 20)
    val theOrder =
      order(orderId, friday, monday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))), OrderPriority.Normal)
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.delayedManufacturings shouldBe empty
    outcome.delayedOrders shouldEqual List(DelayedOrder(orderId, monday, wednesday))

  it should "give urgent orders precedence over normal orders" in:
    val urgentTask = pendingTask(UUID.randomUUID().nn, 8)
    val normalTask = pendingTask(UUID.randomUUID().nn, 8)
    val urgentManufacturingId: ScheduledManufacturingId = UUID.randomUUID().nn
    val urgent =
      order(orderId, friday, NonEmptyList.one(manufacturing(urgentManufacturingId, friday, NonEmptyList.one(urgentTask))), OrderPriority.Urgent)
    val normal = order(otherOrderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(normalTask))))
    val outcome = scheduleOrFail(request(monday, List(orderId, otherOrderId)), List(normal, urgent), List(employee(employeeA)))

    val sliceByOrder = outcome.slices.groupBy(_.orderId)
    sliceByOrder(orderId).map(_.day) shouldEqual List(monday)
    sliceByOrder(otherOrderId).map(_.day) shouldEqual List(tuesday)

  it should "keep planning after the former window would have ended" in:
    val task = pendingTask(UUID.randomUUID().nn, 50)
    val nextTuesday = monday.plusDays(8).nn
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(_.candidateEmployee.assignedHours.value).sum shouldEqual 50
    outcome.slices.last.day shouldEqual nextTuesday
    outcome.unplannedOrders shouldBe empty

  it should "return an unplanned order when no employee can provide future capacity" in:
    val task = pendingTask(UUID.randomUUID().nn, 8)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), Nil)

    outcome.slices shouldBe empty
    outcome.unplannedOrders shouldEqual List(
      UnplannedOrder(orderId, NonEmptyList.one(UnplannedTask(manufacturingId, task.id, UnplannedReason.NoFutureCapacity(TaskHours(8))))),
    )

  it should "plan higher-priority orders and mark later orders unplanned when fixed-term capacity runs out" in:
    val urgentTask = pendingTask(UUID.randomUUID().nn, 16)
    val normalTask = pendingTask(UUID.randomUUID().nn, 8)
    val urgentManufacturingId: ScheduledManufacturingId = UUID.randomUUID().nn
    val limitedContract = Contract.FixedTerm(monday.minusDays(1).nn, wednesday)
    val urgent =
      order(orderId, friday, NonEmptyList.one(manufacturing(urgentManufacturingId, friday, NonEmptyList.one(urgentTask))), OrderPriority.Urgent)
    val normal = order(otherOrderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(normalTask))))
    val outcome =
      scheduleOrFail(request(monday, List(orderId, otherOrderId)), List(normal, urgent), List(employee(employeeA, contract = limitedContract)))

    outcome.slices.map(slice => (slice.orderId, slice.day, slice.candidateEmployee.assignedHours.value)) shouldEqual
      List((orderId, monday, 8), (orderId, tuesday, 8))
    outcome.unplannedOrders.map(_.orderId) shouldEqual List(otherOrderId)

  it should "discard partial slices when a later manufacturing makes the order unplanned" in:
    val validTask = pendingTask(UUID.randomUUID().nn, 8)
    val firstTemplate: TaskId = UUID.randomUUID().nn
    val secondTemplate: TaskId = UUID.randomUUID().nn
    val cycleDependencies = ManufacturingDependencies()
      .addTaskDependencies(firstTemplate, Set(secondTemplate))
      .addTaskDependencies(secondTemplate, Set(firstTemplate))
    val cycleTasks = NonEmptyList.of(pendingTask(firstTemplate, 4), pendingTask(secondTemplate, 4))
    val firstManufacturing = manufacturing(UUID.randomUUID().nn: ScheduledManufacturingId, monday, NonEmptyList.one(validTask))
    val secondManufacturing = manufacturing(manufacturingId, friday, cycleTasks, cycleDependencies)
    val theOrder = order(orderId, friday, NonEmptyList.of(firstManufacturing, secondManufacturing))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices shouldBe empty
    outcome.unplannedOrders.map(_.orderId) shouldEqual List(orderId)

  it should "mark a manufacturing with a dependency cycle as unplanned" in:
    val firstTemplate: TaskId = UUID.randomUUID().nn
    val secondTemplate: TaskId = UUID.randomUUID().nn
    val dependencies = ManufacturingDependencies()
      .addTaskDependencies(firstTemplate, Set(secondTemplate))
      .addTaskDependencies(secondTemplate, Set(firstTemplate))
    val tasks = NonEmptyList.of(pendingTask(firstTemplate, 4), pendingTask(secondTemplate, 4))
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, tasks, dependencies)))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices shouldBe empty
    outcome.unplannedOrders.map(_.orderId) shouldEqual List(orderId)
    outcome.unplannedOrders.head.blockedTasks.head.reason match
      case UnplannedReason.DependencyCycle(cycle) =>
        cycle should contain allOf (firstTemplate, secondTemplate)
      case other => fail(s"Expected a dependency cycle reason, got: $other")

  it should "schedule a dependent manufacturing starting the day after its prerequisite completes" in:
    val prerequisiteId: ScheduledManufacturingId = UUID.randomUUID().nn
    val prerequisiteTask = pendingTask(UUID.randomUUID().nn, 8)
    val dependentTask = pendingTask(UUID.randomUUID().nn, 4)
    val prerequisite = manufacturing(prerequisiteId, friday, NonEmptyList.one(prerequisiteTask))
    // The dependent has the earliest completion date: without the dependency it would be planned first.
    val dependent = manufacturing(manufacturingId, monday, NonEmptyList.one(dependentTask))
    val base = order(orderId, friday, NonEmptyList.of(dependent, prerequisite))
    val dependencies = OrderDependencies.empty.addManufacturingDependencies(manufacturingId, Set(prerequisiteId))
    val theOrder = base.copy(data = base.data.withDependencies(dependencies))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    val sliceByManufacturing = outcome.slices.groupBy(_.manufacturingId)
    sliceByManufacturing(prerequisiteId).map(_.day) shouldEqual List(monday)
    sliceByManufacturing(manufacturingId).map(_.day) shouldEqual List(tuesday)

  it should "treat dependencies on manufacturings without remaining work as already satisfied" in:
    val completedId: ScheduledManufacturingId = UUID.randomUUID().nn
    val completedTask = ScheduledTask.CompletedTask(
      UUID.randomUUID().nn: ScheduledTaskId,
      UUID.randomUUID().nn: TaskId,
      TaskHours.applyUnsafe(8),
      TaskHours.applyUnsafe(8),
      monday.minusDays(1).nn,
    )
    val dependentTask = pendingTask(UUID.randomUUID().nn, 4)
    val completed = manufacturing(completedId, friday, NonEmptyList.one(completedTask))
    val dependent = manufacturing(manufacturingId, friday, NonEmptyList.one(dependentTask))
    val base = order(orderId, friday, NonEmptyList.of(dependent, completed))
    val dependencies = OrderDependencies.empty.addManufacturingDependencies(manufacturingId, Set(completedId))
    val theOrder = base.copy(data = base.data.withDependencies(dependencies))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(slice => (slice.manufacturingId, slice.day)) shouldEqual List((manufacturingId, monday))

  it should "mark an order with a manufacturing dependency cycle as unplanned" in:
    val firstId: ScheduledManufacturingId = UUID.randomUUID().nn
    val first = manufacturing(firstId, friday, NonEmptyList.one(pendingTask(UUID.randomUUID().nn, 4)))
    val second = manufacturing(manufacturingId, friday, NonEmptyList.one(pendingTask(UUID.randomUUID().nn, 4)))
    val dependencies = OrderDependencies.empty
      .addManufacturingDependencies(firstId, Set(manufacturingId))
      .addManufacturingDependencies(manufacturingId, Set(firstId))
    val base = order(orderId, friday, NonEmptyList.of(first, second))
    val theOrder = base.copy(data = base.data.withDependencies(dependencies))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices shouldBe empty
    outcome.unplannedOrders.map(_.orderId) shouldEqual List(orderId)
    outcome.unplannedOrders.head.blockedTasks.head.reason match
      case UnplannedReason.DependencyCycle(cycle) => cycle should contain allOf (firstId, manufacturingId)
      case other => fail(s"Expected a dependency cycle reason, got: $other")

  it should "mark a manufacturing depending on a missing manufacturing as unplanned" in:
    val missingId: ScheduledManufacturingId = UUID.randomUUID().nn
    val blockedTask = pendingTask(UUID.randomUUID().nn, 4)
    val blocked = manufacturing(manufacturingId, friday, NonEmptyList.one(blockedTask))
    val dependencies = OrderDependencies.empty.addManufacturingDependencies(manufacturingId, Set(missingId))
    val base = order(orderId, friday, NonEmptyList.one(blocked))
    val theOrder = base.copy(data = base.data.withDependencies(dependencies))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices shouldBe empty
    outcome.unplannedOrders shouldEqual List(
      UnplannedOrder(orderId, NonEmptyList.one(UnplannedTask(manufacturingId, blockedTask.id, UnplannedReason.MissingDependency(missingId)))),
    )

  it should "mark a task with a missing dependency as unplanned" in:
    val missingTemplate: TaskId = UUID.randomUUID().nn
    val blockedTemplate: TaskId = UUID.randomUUID().nn
    val blockedTask = pendingTask(blockedTemplate, 4)
    val dependencies = ManufacturingDependencies().addTaskDependencies(blockedTemplate, Set(missingTemplate))
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(blockedTask), dependencies)))
    val outcome = scheduleOrFail(request(monday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices shouldBe empty
    outcome.unplannedOrders shouldEqual List(
      UnplannedOrder(orderId, NonEmptyList.one(UnplannedTask(manufacturingId, blockedTask.id, UnplannedReason.MissingDependency(missingTemplate)))),
    )

  it should "wait for future contracts and ignore expired contracts" in:
    val task = pendingTask(UUID.randomUUID().nn, 8)
    val futureContract = Contract.FullTime(wednesday)
    val expiredContract = Contract.FixedTerm(monday.minusDays(10).nn, monday.minusDays(1).nn)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(
      request(monday, List(orderId)),
      List(theOrder),
      List(employee(employeeA, contract = expiredContract), employee(employeeB, contract = futureContract)),
    )

    outcome.slices.map(slice => (slice.day, slice.candidateEmployee.employeeId)) shouldEqual List((wednesday, employeeB))

  it should "complete an empty request without slices or unplanned orders" in:
    val task = pendingTask(UUID.randomUUID().nn, 6)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, Nil), List(theOrder), List(employee(employeeA)))

    outcome.slices shouldBe empty
    outcome.unplannedOrders shouldBe empty
    outcome.warnings shouldBe empty

  it should "warn about requested orders that are not in progress" in:
    val task = pendingTask(UUID.randomUUID().nn, 6)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, List(orderId, otherOrderId)), List(theOrder), List(employee(employeeA)))

    outcome.warnings.map(_.message) shouldEqual
      List(s"Order $otherOrderId was requested for planning but is not an in-progress order and was skipped")

  "PlanningService" should "complete a feasible planning attempt and publish the completion" in:
    val task = pendingTask(UUID.randomUUID().nn, 6)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val planningRequest = request(monday, List(orderId))
    val command = Command.ComputePlan(planningRequest, List(theOrder), List(employee(employeeA)))
    val context = RequestContext(CommandMessage("cmd-1", Instant.now().nn, "production-planning", command), Planning.initial)

    PlanningService[Id].execute(context) match
      case EdomatonResult.Accepted(state, _, notifications) =>
        state shouldBe a[CompletedPlanning]
        notifications.toList shouldEqual List(Notification.PlanningCompleted(planningRequest.id))
      case other => fail(s"Expected an accepted planning attempt, got: $other")

  it should "complete an attempt with unplanned orders and publish an unplanned notification" in:
    val task = pendingTask(UUID.randomUUID().nn, 50)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val planningRequest = request(monday, List(orderId))
    val command = Command.ComputePlan(planningRequest, List(theOrder), Nil)
    val context = RequestContext(CommandMessage("cmd-2", Instant.now().nn, "production-planning", command), Planning.initial)
    val expectedUnplanned =
      UnplannedOrder(orderId, NonEmptyList.one(UnplannedTask(manufacturingId, task.id, UnplannedReason.NoFutureCapacity(TaskHours(50)))))

    PlanningService[Id].execute(context) match
      case EdomatonResult.Accepted(state, _, notifications) =>
        state shouldBe a[CompletedPlanning]
        notifications.toList shouldEqual List(Notification.OrderUnplanned(expectedUnplanned), Notification.PlanningCompleted(planningRequest.id))
      case other => fail(s"Expected a completed planning attempt, got: $other")

  it should "publish delay notifications when planning delays an order" in:
    val task = pendingTask(UUID.randomUUID().nn, 20)
    val theOrder = order(orderId, monday, NonEmptyList.one(manufacturing(manufacturingId, monday, NonEmptyList.one(task))))
    val planningRequest = request(monday, List(orderId))
    val command = Command.ComputePlan(planningRequest, List(theOrder), List(employee(employeeA)))
    val context = RequestContext(CommandMessage("cmd-3", Instant.now().nn, "production-planning", command), Planning.initial)

    PlanningService[Id].execute(context) match
      case EdomatonResult.Accepted(_, _, notifications) =>
        notifications.toList shouldEqual List(
          Notification.OrderDelayed(orderId, monday, wednesday),
          Notification.ManufacturingDelayed(orderId, manufacturingId, monday, wednesday),
          Notification.PlanningCompleted(planningRequest.id),
        )
      case other => fail(s"Expected an accepted planning attempt, got: $other")
end SchedulingServiceTest
