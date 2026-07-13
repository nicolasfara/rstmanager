package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.planning.{ PlanningRequest, PlanningTrigger }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingInfo }
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskHours, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask

import cats.data.NonEmptyList
import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningRecalculationServiceTest extends AnyFlatSpecLike:
  private final case class Captured(request: PlanningRequest, orders: List[Order], employees: List[Employee])

  private val monday: DateTime = DateTime.parse("2026-06-15").nn
  private val orderId: OrderId = UUID.fromString("00000000-0000-0000-0000-000000000011").nn
  private val employeeId: EmployeeId = UUID.fromString("00000000-0000-0000-0000-000000000012").nn

  "PlanningRecalculationService" should "load a fresh automatic snapshot and compute a plan" in:
    val order = openOrder(orderId)
    val worker = employee(employeeId)
    val orderSelection = Ref.of[IO, Option[List[UUID]]](Some(Nil)).unsafeRunSync()
    val employeeSelection = Ref.of[IO, Option[List[UUID]]](Some(Nil)).unsafeRunSync()
    val captured = Ref.of[IO, Option[Captured]](None).unsafeRunSync()
    val gateway = RecordingGateway(List(order), List(worker), orderSelection, employeeSelection)
    val recalculator = PlanningRecalculationService.fromCompute(gateway) { (_, request, orders, employees) =>
      captured.set(Some(Captured(request, orders, employees))).as(Right("command-1"))
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

  private final class RecordingGateway(
      orders: List[InProgressOrder],
      employees: List[Employee],
      orderSelection: Ref[IO, Option[List[UUID]]],
      employeeSelection: Ref[IO, Option[List[UUID]]],
  ) extends PlanningEntityGateway:
    override def loadOpenOrders(selection: Option[List[UUID]]): IO[Either[PlanningEntityGateway.LoadError, List[InProgressOrder]]] =
      orderSelection.set(selection).as(orders.asRight)

    override def loadEmployees(selection: Option[List[UUID]]): IO[Either[PlanningEntityGateway.LoadError, List[Employee]]] =
      employeeSelection.set(selection).as(employees.asRight)

  private def employee(id: EmployeeId): Employee =
    Employee(
      id,
      EmployeeInfo("Mario".refineUnsafe[Name], "Rossi".refineUnsafe[Surname]),
      Contract.FullTime(monday.minusYears(1).nn),
      BudgetHours(WeeklyHours.applyUnsafe(40), Nil),
    )

  private def openOrder(id: OrderId): InProgressOrder =
    val taskId: TaskId = UUID.fromString("00000000-0000-0000-0000-000000000013").nn
    val scheduledTask = PendingTask(UUID.fromString("00000000-0000-0000-0000-000000000014").nn, taskId, TaskHours.applyUnsafe(8))
    val manufacturing = ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        UUID.fromString("00000000-0000-0000-0000-000000000015").nn,
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
        UUID.fromString("00000000-0000-0000-0000-000000000016").nn: CustomerId,
        monday.minusDays(1).nn,
        monday.plusDays(5).nn,
        OrderPriority.Normal,
        NonEmptyList.one(manufacturing),
      ),
      monday.plusDays(5).nn,
    )
  end openOrder
end PlanningRecalculationServiceTest
