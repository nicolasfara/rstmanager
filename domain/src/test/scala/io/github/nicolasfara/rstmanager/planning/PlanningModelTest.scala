package io.github.nicolasfara.rstmanager.planning

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.planning.Planning.*
import io.github.nicolasfara.rstmanager.planning.PlanningError.*
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.*
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.{ NonEmptyList, ValidatedNec }
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.Empty
import io.github.iltotore.iron.constraint.any.{ DescribedAs, Not }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningModelTest extends AnyFlatSpecLike:
  private def validOrFail[A](value: ValidatedNec[PlanningError, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private val day: DateTime = DateTime.parse("2026-06-15").nn
  private val nextDay: DateTime = day.plusDays(1).nn
  private val employeeId: EmployeeId = UUID.fromString("00000000-0000-0000-0000-000000000001").nn
  private val orderId: OrderId = UUID.fromString("00000000-0000-0000-0000-000000000002").nn
  private val manufacturingId: ScheduledManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000003").nn
  private val taskId: ScheduledTaskId = UUID.fromString("00000000-0000-0000-0000-000000000004").nn

  private def candidate: CandidateEmployee = candidateWithHours(8, 4)

  private def candidateWithHours(availableHours: Int, assignedHours: Int): CandidateEmployee =
    validOrFail(CandidateEmployee.create(employeeId, TaskHours.applyUnsafe(availableHours), TaskHours.applyUnsafe(assignedHours)))

  private def slice: ScheduledTaskSlice = sliceForDay(day)

  private def sliceForDay(sliceDay: DateTime): ScheduledTaskSlice =
    ScheduledTaskSlice(orderId, manufacturingId, taskId, sliceDay, candidate, TaskHours(12))

  private def dailySchedule: DailySchedule = dailyScheduleForDay(day)

  private def dailyScheduleForDay(scheduleDay: DateTime): DailySchedule =
    validOrFail(DailySchedule.create(scheduleDay, List(sliceForDay(scheduleDay))))

  private def planningResult: PlanningResult =
    validOrFail(PlanningResult.create(List(dailySchedule), Nil, Nil, Nil))

  private def planningWindow: PlanningWindow =
    validOrFail(PlanningWindow.create(day, nextDay))

  private def request: PlanningRequest = requestWithId(UUID.fromString("00000000-0000-0000-0000-000000000005").nn)

  private def requestWithId(requestId: PlanningRequestId): PlanningRequest =
    PlanningRequest(requestId, planningWindow, PlanningTrigger.DailyPlanning, day, List(orderId))

  private def orderData(
      id: OrderId,
      priority: OrderPriority,
      creationDate: DateTime,
      deliveryDate: DateTime,
  ): OrderData =
    OrderData(
      id,
      s"ORD-$id".refineUnsafe[OrderNumber],
      UUID.randomUUID().nn: CustomerId,
      creationDate,
      deliveryDate,
      priority,
      NonEmptyList.one(manufacturing()),
    )

  private def manufacturing(): ScheduledManufacturing =
    ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        manufacturingId,
        new DescribedAs[Not[Empty], "The code manufacturing should be not empty"](): ManufacturingCode,
        nextDay,
        NonEmptyList.one(PendingTask(UUID.randomUUID().nn, UUID.randomUUID().nn: TaskId, TaskHours(8))),
        ManufacturingDependencies(),
      ),
    )

  "PlanningWindow" should "accept an inclusive window" in:
    PlanningWindow.create(day, day).isValid shouldEqual true

  it should "reject a reversed window" in:
    PlanningWindow.create(nextDay, day).toEither.left.map(_.head) shouldEqual Left(InvalidPlanningWindow(nextDay, day))

  "CandidateEmployee" should "accept a positive assignment within available hours" in:
    candidateWithHours(8, 8).assignedHours shouldEqual TaskHours(8)

  it should "reject a zero assignment" in:
    CandidateEmployee.create(employeeId, TaskHours(8), TaskHours(0)).toEither.left.map(_.head) shouldEqual
      Left(InvalidEmployeeAssignment(TaskHours(8), TaskHours(0)))

  it should "reject an assignment above available hours" in:
    CandidateEmployee.create(employeeId, TaskHours(4), TaskHours(5)).toEither.left.map(_.head) shouldEqual
      Left(InvalidEmployeeAssignment(TaskHours(4), TaskHours(5)))

  "DailySchedule" should "reject empty slices" in:
    DailySchedule.create(day, Nil).toEither.left.map(_.head) shouldEqual Left(EmptyDailySchedule(day))

  it should "reject slices from another day" in:
    DailySchedule.create(day, List(sliceForDay(nextDay))).toEither.left.map(_.head) shouldEqual
      Left(TaskSliceOutsideScheduleDay(day, nextDay))

  it should "accept non-empty slices for the schedule day" in:
    dailySchedule.slices.head.day shouldEqual day

  "PlanningResult" should "represent planned days, delays, and warnings" in:
    val delayedOrder = DelayedOrder(orderId, day, nextDay)
    val delayedManufacturing = DelayedManufacturing(orderId, manufacturingId, day, nextDay)
    val warning = PlanningWarning("Capacity is tight")
    val result = validOrFail(PlanningResult.create(List(dailySchedule), List(delayedOrder), List(delayedManufacturing), List(warning)))

    result.schedules.head.day shouldEqual day
    result.delayedOrders should contain only delayedOrder
    result.delayedManufacturings should contain only delayedManufacturing
    result.warnings should contain only warning

  it should "reject an empty schedule collection" in:
    PlanningResult.create(Nil, Nil, Nil, Nil).toEither.left.map(_.head) shouldEqual Left(EmptyPlanningResult)

  "PlanningError" should "carry structured data for insufficient capacity" in:
    val error = InsufficientCapacity(planningWindow, TaskHours(16), TaskHours(8), List(orderId), List(manufacturingId))

    error match
      case InsufficientCapacity(window, requiredHours, availableHours, affectedOrders, affectedManufacturings) =>
        window shouldEqual planningWindow
        requiredHours shouldEqual TaskHours(16)
        availableHours shouldEqual TaskHours(8)
        affectedOrders should contain only orderId
        affectedManufacturings should contain only manufacturingId
      case other => fail(s"Unexpected error: $other")

  "Planning aggregate" should "transition from requested to completed through events" in:
    val active = validOrFail(Planning.transition(PlanningRequested(request))(Planning.initial))
    val withSlice = validOrFail(Planning.transition(TaskSliceAssigned(slice, day))(active))
    val completed = validOrFail(Planning.transition(ScheduleComputed(planningResult, nextDay))(withSlice))

    completed shouldBe a[CompletedPlanning]

  it should "transition from requested to rejected through events" in:
    val active = validOrFail(Planning.transition(PlanningRequested(request))(Planning.initial))
    val rejected = validOrFail(
      Planning.transition(
        ScheduleRejected(NonEmptyList.one(InsufficientCapacity(planningWindow, TaskHours(16), TaskHours(8), List(orderId), Nil)), nextDay),
      )(
        active,
      ),
    )

    rejected shouldBe a[RejectedPlanning]

  "PlanningPriorityPolicy" should "sort open orders by priority, delivery date, creation date, then id" in:
    val normalEarlyId = UUID.fromString("00000000-0000-0000-0000-000000000011").nn
    val urgentLaterId = UUID.fromString("00000000-0000-0000-0000-000000000012").nn
    val urgentEarlierId = UUID.fromString("00000000-0000-0000-0000-000000000013").nn
    val normalSameDateOlderId = UUID.fromString("00000000-0000-0000-0000-000000000014").nn
    val normalSameDateNewerId = UUID.fromString("00000000-0000-0000-0000-000000000015").nn

    val normalEarly = InProgressOrder(orderData(normalEarlyId, OrderPriority.Normal, day, day), day)
    val urgentLater = InProgressOrder(orderData(urgentLaterId, OrderPriority.Urgent, day, nextDay), nextDay)
    val urgentEarlier = InProgressOrder(orderData(urgentEarlierId, OrderPriority.Urgent, day, day), day)
    val normalSameDateOlder = InProgressOrder(orderData(normalSameDateOlderId, OrderPriority.Normal, day.minusDays(1).nn, day), day)
    val normalSameDateNewer = InProgressOrder(orderData(normalSameDateNewerId, OrderPriority.Normal, day, day), day)

    PlanningPriorityPolicy.sortOpenOrders(List(normalEarly, urgentLater, urgentEarlier, normalSameDateNewer, normalSameDateOlder)) shouldEqual
      List(urgentEarlier, urgentLater, normalSameDateOlder, normalEarly, normalSameDateNewer)

  it should "exclude non-open orders" in:
    val data = orderData(orderId, OrderPriority.Normal, day, day)
    val open = InProgressOrder(data, day)
    val suspended = SuspendedOrder(data, day, day, None)
    val completed = CompletedOrder(data, day)
    val delivered = DeliveredOrder(data, day, nextDay)
    val cancelled = CancelledOrder(data, day, None)

    PlanningPriorityPolicy.sortOpenOrders(List(NewOrder, suspended, completed, delivered, cancelled, open)) shouldEqual List(open)
end PlanningModelTest
