package io.github.nicolasfara.rstmanager.planning

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.planning.Planning.{ CompletedPlanning, RejectedPlanning }
import io.github.nicolasfara.rstmanager.planning.PlanningError.*
import io.github.nicolasfara.rstmanager.planning.PlanningService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingId, ScheduledManufacturingInfo }
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask

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

  private def employee(id: EmployeeId, weeklyHours: Int = 40, overrides: List[HoursOverride] = Nil): Employee =
    Employee(
      id,
      EmployeeInfo("Mario".refineUnsafe[Name], "Rossi".refineUnsafe[Surname]),
      Contract.FullTime(monday.minusYears(1).nn),
      BudgetHours(WeeklyHours.applyUnsafe(weeklyHours), overrides),
    )

  private def pendingTask(templateId: TaskId, hours: Int): PendingTask =
    PendingTask(UUID.randomUUID().nn: ScheduledTaskId, templateId, TaskHours.applyUnsafe(hours))

  private def manufacturing(
      id: ScheduledManufacturingId,
      completionDate: DateTime,
      tasks: NonEmptyList[ScheduledTask],
      dependencies: ManufacturingDependencies = ManufacturingDependencies(),
  ): ScheduledManufacturing =
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        id,
        new DescribedAs[Not[Empty], "The code manufacturing should be not empty"](): ManufacturingCode,
        completionDate,
        tasks,
        dependencies,
      ),
    )

  private def order(
      id: OrderId,
      deliveryDate: DateTime,
      manufacturings: NonEmptyList[ScheduledManufacturing],
      priority: OrderPriority = OrderPriority.Normal,
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
      deliveryDate,
    )

  private def request(start: DateTime, end: DateTime, orderIds: List[OrderId]): PlanningRequest =
    PlanningRequest(
      UUID.fromString("00000000-0000-0000-0000-000000000005").nn,
      PlanningWindow(start, end),
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
    val outcome = scheduleOrFail(request(monday, friday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(slice => (slice.day, slice.candidateEmployee.assignedHours.value, slice.remainingHoursAfterSlice.value)) shouldEqual
      List((monday, 6, 0))
    outcome.delayedOrders shouldBe empty
    outcome.delayedManufacturings shouldBe empty

  it should "split a task over consecutive days when it exceeds the daily capacity" in:
    val task = pendingTask(UUID.randomUUID().nn, 20)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, friday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(slice => (slice.day, slice.candidateEmployee.assignedHours.value, slice.remainingHoursAfterSlice.value)) shouldEqual
      List((monday, 8, 12), (tuesday, 8, 4), (wednesday, 4, 0))

  it should "split a task over multiple employees on the same day" in:
    val task = pendingTask(UUID.randomUUID().nn, 12)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, friday, List(orderId)), List(theOrder), List(employee(employeeA), employee(employeeB)))

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
    val outcome = scheduleOrFail(request(monday, friday, List(orderId)), List(theOrder), List(employee(employeeA), employee(employeeB)))

    val sliceByTask = outcome.slices.groupBy(_.taskId)
    sliceByTask(prerequisite.id).map(_.day) shouldEqual List(monday)
    sliceByTask(dependent.id).map(_.day) shouldEqual List(tuesday)

  it should "skip Saturdays and Sundays" in:
    val task = pendingTask(UUID.randomUUID().nn, 16)
    val nextMonday = monday.plusDays(7).nn
    val theOrder = order(orderId, nextMonday, NonEmptyList.one(manufacturing(manufacturingId, nextMonday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(friday, nextMonday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.slices.map(_.day) shouldEqual List(friday, nextMonday)

  it should "skip employee vacation days" in:
    val task = pendingTask(UUID.randomUUID().nn, 8)
    val vacation = VacationOverride.createVacationOverride(monday, tuesday).toOption.getOrElse(fail("invalid vacation"))
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, friday, List(orderId)), List(theOrder), List(employee(employeeA, overrides = List(vacation))))

    outcome.slices.map(_.day) shouldEqual List(tuesday)

  it should "report order and manufacturing delays when work overruns the expected dates" in:
    val task = pendingTask(UUID.randomUUID().nn, 20)
    val theOrder = order(orderId, monday, NonEmptyList.one(manufacturing(manufacturingId, monday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, friday, List(orderId)), List(theOrder), List(employee(employeeA)))

    outcome.delayedManufacturings shouldEqual List(DelayedManufacturing(orderId, manufacturingId, monday, wednesday))
    outcome.delayedOrders shouldEqual List(DelayedOrder(orderId, monday, wednesday))

  it should "give urgent orders precedence over normal orders" in:
    val urgentTask = pendingTask(UUID.randomUUID().nn, 8)
    val normalTask = pendingTask(UUID.randomUUID().nn, 8)
    val urgentManufacturingId: ScheduledManufacturingId = UUID.randomUUID().nn
    val urgent =
      order(orderId, friday, NonEmptyList.one(manufacturing(urgentManufacturingId, friday, NonEmptyList.one(urgentTask))), OrderPriority.Urgent)
    val normal = order(otherOrderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(normalTask))))
    val outcome = scheduleOrFail(request(monday, friday, List(orderId, otherOrderId)), List(normal, urgent), List(employee(employeeA)))

    val sliceByOrder = outcome.slices.groupBy(_.orderId)
    sliceByOrder(orderId).map(_.day) shouldEqual List(monday)
    sliceByOrder(otherOrderId).map(_.day) shouldEqual List(tuesday)

  it should "reject the planning window when capacity is insufficient" in:
    val task = pendingTask(UUID.randomUUID().nn, 50)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val result = SchedulingService.computeSchedule(request(monday, tuesday, List(orderId)), List(theOrder), List(employee(employeeA)))

    result.left.map(_.toList) shouldEqual Left(
      List(InsufficientCapacity(PlanningWindow(monday, tuesday), TaskHours(50), TaskHours(16), List(orderId), List(manufacturingId))),
    )

  it should "reject a manufacturing whose dependency graph has a cycle" in:
    val firstTemplate: TaskId = UUID.randomUUID().nn
    val secondTemplate: TaskId = UUID.randomUUID().nn
    val dependencies = ManufacturingDependencies()
      .addTaskDependencies(firstTemplate, Set(secondTemplate))
      .addTaskDependencies(secondTemplate, Set(firstTemplate))
    val tasks = NonEmptyList.of(pendingTask(firstTemplate, 4), pendingTask(secondTemplate, 4))
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, tasks, dependencies)))
    val result = SchedulingService.computeSchedule(request(monday, friday, List(orderId)), List(theOrder), List(employee(employeeA)))

    result.left.map(_.toList) match
      case Left(List(TaskCannotBeScheduled(blockedOrderId, blockedManufacturingId, _, _, _))) =>
        blockedOrderId shouldEqual orderId
        blockedManufacturingId shouldEqual manufacturingId
      case other => fail(s"Expected a dependency cycle rejection, got: $other")

  it should "warn about requested orders that are not in progress" in:
    val task = pendingTask(UUID.randomUUID().nn, 6)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val outcome = scheduleOrFail(request(monday, friday, List(orderId, otherOrderId)), List(theOrder), List(employee(employeeA)))

    outcome.warnings.map(_.message) shouldEqual
      List(s"Order $otherOrderId was requested for planning but is not an in-progress order and was skipped")

  "PlanningService" should "complete a feasible planning attempt and publish the completion" in:
    val task = pendingTask(UUID.randomUUID().nn, 6)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val planningRequest = request(monday, friday, List(orderId))
    val command = Command.ComputePlan(planningRequest, List(theOrder), List(employee(employeeA)))
    val context = RequestContext(CommandMessage("cmd-1", Instant.now().nn, "production-planning", command), Planning.initial)

    PlanningService[Id].execute(context) match
      case EdomatonResult.Accepted(state, _, notifications) =>
        state shouldBe a[CompletedPlanning]
        notifications.toList shouldEqual List(Notification.PlanningCompleted(planningRequest.id))
      case other => fail(s"Expected an accepted planning attempt, got: $other")

  it should "reject an infeasible planning attempt and publish the rejection" in:
    val task = pendingTask(UUID.randomUUID().nn, 50)
    val theOrder = order(orderId, friday, NonEmptyList.one(manufacturing(manufacturingId, friday, NonEmptyList.one(task))))
    val planningRequest = request(monday, tuesday, List(orderId))
    val command = Command.ComputePlan(planningRequest, List(theOrder), List(employee(employeeA)))
    val context = RequestContext(CommandMessage("cmd-2", Instant.now().nn, "production-planning", command), Planning.initial)

    PlanningService[Id].execute(context) match
      case EdomatonResult.Accepted(state, _, notifications) =>
        state shouldBe a[RejectedPlanning]
        notifications.toList should matchPattern { case List(Notification.PlanningRejected(_, _)) => }
      case other => fail(s"Expected a recorded rejection, got: $other")

  it should "publish delay notifications when planning delays an order" in:
    val task = pendingTask(UUID.randomUUID().nn, 20)
    val theOrder = order(orderId, monday, NonEmptyList.one(manufacturing(manufacturingId, monday, NonEmptyList.one(task))))
    val planningRequest = request(monday, friday, List(orderId))
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
