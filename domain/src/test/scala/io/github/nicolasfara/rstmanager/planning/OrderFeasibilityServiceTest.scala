package io.github.nicolasfara.rstmanager.planning

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.hr.domain.calendar.CompanyCalendar
import io.github.nicolasfara.rstmanager.planning.OrderFeasibilityService.FeasibilityResult
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{
  ScheduledManufacturing,
  ScheduledManufacturingId,
  ScheduledManufacturingInfo,
}
import io.github.nicolasfara.rstmanager.work.domain.order.{ OrderData, OrderId, OrderNumber, OrderPriority }
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class OrderFeasibilityServiceTest extends AnyFlatSpecLike:
  private val monday: DateTime = DateTime.parse("2026-06-15")
  private def weekday(offset: Int): DateTime = monday.plusDays(offset).nn
  private val tuesday = weekday(1)
  private val wednesday = weekday(2)
  private val friday = weekday(4)

  private val employeeA: EmployeeId = EmployeeId(UUID.fromString("00000000-0000-0000-0000-0000000000a1").nn)

  // One full-time employee: 8 hours on each working day.
  private val workforce: List[Employee] = List(
    Employee(
      employeeA,
      EmployeeInfo("Mario".refineUnsafe[Name], "Rossi".refineUnsafe[Surname]),
      Contract.FullTime(monday.toLocalDate.minusYears(1)),
      BudgetHours(WeeklyHours.applyUnsafe(40), Nil),
    ),
  )

  // Scenarios are expressed in whole hours; the domain stores minutes, so hour figures are scaled by 60.
  private def order(
      id: OrderId,
      hours: Int,
      deadline: DateTime,
      priority: OrderPriority,
      manufacturingCompletion: Option[DateTime],
  ): InProgressOrder =
    val manufacturing = ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        ScheduledManufacturingId(UUID.randomUUID().nn),
        "MFG-1".refineUnsafe[ManufacturingCode],
        manufacturingCompletion.getOrElse(deadline),
        NonEmptyList.one(PendingTask(ScheduledTaskId(UUID.randomUUID().nn), TaskId(UUID.randomUUID().nn), TaskDuration.applyUnsafe(hours * 60))),
        ManufacturingDependencies(),
      ),
    )
    InProgressOrder(
      OrderData(
        id,
        s"ORD-${id.toString.take(4)}".refineUnsafe[OrderNumber],
        CustomerId(UUID.randomUUID().nn),
        monday.minusDays(7).nn,
        deadline.plusDays(5).nn,
        priority,
        NonEmptyList.one(manufacturing),
      ),
      deadline,
    )
  end order

  private def existing(hours: Int, deadline: DateTime): InProgressOrder =
    order(OrderId(UUID.randomUUID().nn), hours, deadline, OrderPriority.Normal, None)

  private def candidate(hours: Int, deadline: DateTime): InProgressOrder =
    order(OrderFeasibilityService.candidateId, hours, deadline, OrderPriority.Normal, None)

  private def check(open: List[InProgressOrder], target: InProgressOrder): FeasibilityResult = checkWith(open, target, workforce)

  private def checkWith(open: List[InProgressOrder], target: InProgressOrder, employees: List[Employee]): FeasibilityResult =
    OrderFeasibilityService.check(monday, open, employees, CompanyCalendar.empty, target)

  "OrderFeasibilityService" should "report no delay when the order fits before its deadline" in:
    val result = check(Nil, candidate(8, wednesday))

    result.plannedCompletion shouldEqual Some(monday)
    result.late shouldBe false
    result.hasDelay shouldBe false
    result.suggestedDeadline shouldBe None

  it should "flag a late order and suggest the earliest deadline it can meet" in:
    val result = check(Nil, candidate(24, monday))

    result.plannedCompletion shouldEqual Some(wednesday)
    result.late shouldBe true
    result.suggestedDeadline shouldEqual Some(wednesday)
    check(Nil, candidate(24, wednesday)).hasDelay shouldBe false

  it should "report existing orders an urgent order delays, without a deadline that could avoid it" in:
    val other = existing(8, monday)
    val result = check(List(other), order(OrderFeasibilityService.candidateId, 8, friday, OrderPriority.Urgent, None))

    result.late shouldBe false
    result.inducedDelays.map(_.orderId) shouldEqual List(other.data.id)
    result.inducedDelays.flatMap(_.plannedCompletion) shouldEqual List(tuesday)
    result.hasDelay shouldBe true
    result.suggestedDeadline shouldBe None

  it should "suggest a deadline that plans the order after the existing ones it would delay" in:
    val other = existing(16, tuesday)
    val result = check(List(other), candidate(8, monday))

    // Planned first by its earlier deadline, the candidate pushes the existing order to Wednesday.
    result.late shouldBe false
    result.inducedDelays.map(_.orderId) shouldEqual List(other.data.id)
    result.suggestedDeadline shouldEqual Some(wednesday)
    check(List(other), candidate(8, wednesday)).hasDelay shouldBe false

  it should "not count orders that are already late without the candidate" in:
    val alreadyLate = existing(16, monday)
    val result = check(List(alreadyLate), candidate(8, friday))

    result.inducedDelays shouldBe empty
    result.hasDelay shouldBe false

  it should "report an order that cannot be planned with the known workforce" in:
    val result = checkWith(Nil, candidate(8, friday), Nil)

    result.plannedCompletion shouldBe None
    result.unplannedReasons should not be empty
    result.hasDelay shouldBe true
    result.suggestedDeadline shouldBe None

  it should "report manufacturings late on their own completion date" in:
    val result = check(Nil, order(OrderFeasibilityService.candidateId, 16, friday, OrderPriority.Normal, Some(monday)))

    result.late shouldBe false
    result.lateManufacturings.map(late => (late.completionDate, late.plannedCompletion)) shouldEqual List((monday, tuesday))
    result.hasDelay shouldBe true
    result.suggestedDeadline shouldBe None
end OrderFeasibilityServiceTest
