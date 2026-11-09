package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.hr.domain.calendar.{ ClosureId, CompanyCalendar, CompanyClosure }
import io.github.nicolasfara.rstmanager.planning.{ PlanningRequest, PlanningTrigger }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{
  ScheduledManufacturing,
  ScheduledManufacturingId,
  ScheduledManufacturingInfo,
}
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.NonEmptyList
import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningRecalculationServiceTest extends AnyFlatSpecLike:
  private final case class Captured(request: PlanningRequest, orders: List[Order], employees: List[Employee], calendar: CompanyCalendar)

  private val monday: DateTime = DateTime.parse("2026-06-15")
  private val orderId: OrderId = OrderId(UUID.fromString("00000000-0000-0000-0000-000000000011").nn)
  private val employeeId: EmployeeId = EmployeeId(UUID.fromString("00000000-0000-0000-0000-000000000012").nn)

  "PlanningRecalculationService" should "load a fresh automatic snapshot and compute a plan" in:
    val order = openOrder(orderId)
    val worker = employee(employeeId)
    val orderSelection = Ref.of[IO, Option[List[UUID]]](Some(Nil)).unsafeRunSync()
    val employeeSelection = Ref.of[IO, Option[List[UUID]]](Some(Nil)).unsafeRunSync()
    val captured = Ref.of[IO, Option[Captured]](None).unsafeRunSync()
    val closure = CompanyClosure
      .createAnnual(ClosureId(UUID.fromString("00000000-0000-0000-0000-000000000017").nn), 8, 15, "Ferragosto")
      .fold(errors => fail(errors.toChain.toList.mkString), identity)
    val calendar = CompanyCalendar(List(closure))
    val gateway = RecordingGateway(List(order), List(worker), calendar, orderSelection, employeeSelection)
    val recalculator = PlanningRecalculationService.fromCompute(gateway) { (_, request, orders, employees, calendar) =>
      captured.set(Some(Captured(request, orders, employees, calendar))).as(Right("command-1"))
    }

    val result = recalculator.recalculate(PlanningTrigger.WorkforceCapacityChanged).unsafeRunSync()
    val capturedValue = captured.get.unsafeRunSync().getOrElse(fail("Expected planning compute to be invoked."))

    result shouldEqual PlanningRecalculationResult(Some("command-1"), Nil)
    orderSelection.get.unsafeRunSync() shouldBe None
    employeeSelection.get.unsafeRunSync() shouldBe None
    capturedValue.request.trigger shouldEqual PlanningTrigger.WorkforceCapacityChanged
    capturedValue.request.openOrderIds shouldEqual List(orderId)
    capturedValue.orders shouldEqual List(order)
    capturedValue.employees shouldEqual List(worker)
    capturedValue.calendar shouldEqual calendar

  private final class RecordingGateway(
      orders: List[InProgressOrder],
      employees: List[Employee],
      calendar: CompanyCalendar,
      orderSelection: Ref[IO, Option[List[UUID]]],
      employeeSelection: Ref[IO, Option[List[UUID]]],
  ) extends PlanningEntityGateway:
    override def loadOpenOrders(selection: Option[List[UUID]]): IO[Either[PlanningEntityGateway.LoadError, List[InProgressOrder]]] =
      orderSelection.set(selection).as(orders.asRight)

    override def loadEmployees(selection: Option[List[UUID]]): IO[Either[PlanningEntityGateway.LoadError, List[Employee]]] =
      employeeSelection.set(selection).as(employees.asRight)

    override def loadCalendar: IO[CompanyCalendar] = IO.pure(calendar)

  private def employee(id: EmployeeId): Employee =
    Employee(
      id,
      EmployeeInfo("Mario".refineUnsafe[Name], "Rossi".refineUnsafe[Surname]),
      Contract.FullTime(monday.toLocalDate.minusYears(1)),
      BudgetHours(WeeklyHours.applyUnsafe(40), Nil),
    )

  private def openOrder(id: OrderId): InProgressOrder =
    val taskId: TaskId = TaskId(UUID.fromString("00000000-0000-0000-0000-000000000013").nn)
    val scheduledTask = PendingTask(ScheduledTaskId(UUID.fromString("00000000-0000-0000-0000-000000000014").nn), taskId, TaskDuration.applyUnsafe(8))
    val manufacturing = ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-000000000015").nn),
        "MFG-TEST".refineUnsafe[ManufacturingCode],
        monday.plusDays(2).nn,
        NonEmptyList.one(scheduledTask),
        ManufacturingDependencies(),
      ),
    )

    InProgressOrder(
      OrderData(
        id,
        "ORD-1".refineUnsafe[OrderNumber],
        CustomerId(UUID.fromString("00000000-0000-0000-0000-000000000016").nn),
        monday.minusDays(1).nn,
        monday.plusDays(5).nn,
        OrderPriority.Normal,
        NonEmptyList.one(manufacturing),
      ),
      monday.plusDays(5).nn,
    )
  end openOrder
end PlanningRecalculationServiceTest
