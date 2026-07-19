package io.github.nicolasfara.rstmanager.planning

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.planning.OrderSimulationService.{ SimulationDemand, SimulationResult }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ Manufacturing, ManufacturingCode, ManufacturingDependencies, ManufacturingName }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingInfo }
import io.github.nicolasfara.rstmanager.work.domain.order.{ OrderData, OrderId, OrderNumber, OrderPriority }
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskHours, TaskId, TaskName }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.PendingTask

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class OrderSimulationServiceTest extends AnyFlatSpecLike:
  private val monday: DateTime = DateTime.parse("2026-06-15").nn
  private val tuesday: DateTime = monday.plusDays(1).nn
  private val wednesday: DateTime = monday.plusDays(2).nn

  private val employeeA: EmployeeId = UUID.fromString("00000000-0000-0000-0000-0000000000a1").nn

  private def employee(id: EmployeeId): Employee =
    Employee(
      id,
      EmployeeInfo("Mario".refineUnsafe[Name], "Rossi".refineUnsafe[Surname]),
      Contract.FullTime(monday.minusYears(1).nn),
      BudgetHours(WeeklyHours.applyUnsafe(40), Nil),
    )

  private def existingOrder(id: OrderId, hours: Int): InProgressOrder =
    val manufacturing = ScheduledManufacturing.NotStartedManufacturing(
      ScheduledManufacturingInfo(
        UUID.randomUUID().nn,
        "MFG-EXISTING".refineUnsafe[ManufacturingCode],
        monday.plusDays(5).nn,
        NonEmptyList.one(PendingTask(UUID.randomUUID().nn, UUID.randomUUID().nn, TaskHours.applyUnsafe(hours))),
        ManufacturingDependencies(),
      ),
    )
    InProgressOrder(
      OrderData(
        id,
        s"ORD-$id".refineUnsafe[OrderNumber],
        UUID.randomUUID().nn: CustomerId,
        monday.minusDays(7).nn,
        monday.plusDays(5).nn,
        OrderPriority.Normal,
        NonEmptyList.one(manufacturing),
      ),
      monday.plusDays(5).nn,
    )
  end existingOrder

  private def catalogTask(id: TaskId, name: String, hours: Int): Task =
    Task(id, name.refineUnsafe[TaskName], None, TaskHours.applyUnsafe(hours))

  private def template(taskIds: NonEmptyList[TaskId], dependencies: ManufacturingDependencies): Manufacturing =
    Manufacturing(
      UUID.randomUUID().nn,
      "MFG-TPL".refineUnsafe[ManufacturingCode],
      "Serramenti".refineUnsafe[ManufacturingName],
      None,
      taskIds,
      dependencies,
    )

  private def simulateOrFail(
      openOrders: List[InProgressOrder],
      employees: List[Employee],
      demand: SimulationDemand,
  ): SimulationResult =
    OrderSimulationService.simulate(monday, openOrders, employees, demand) match
      case Right(result) => result
      case Left(errors) => fail(s"Expected a simulation result, got: ${errors.toList.mkString(", ")}")

  "OrderSimulationService" should "estimate the completion date of a total-hours demand on an empty plan" in:
    val result = simulateOrFail(Nil, List(employee(employeeA)), SimulationDemand.TotalHours(TaskHours(20)))

    result.totalHours shouldEqual 20
    result.startDate shouldEqual Some(monday)
    result.estimatedCompletionDate shouldEqual Some(wednesday)
    result.unplannedReasons shouldBe empty

  it should "plan the simulated order after the existing commitments" in:
    // The existing order fills Monday entirely (8h), so the simulated 8 hours only start on Tuesday.
    val result =
      simulateOrFail(List(existingOrder(UUID.randomUUID().nn, 8)), List(employee(employeeA)), SimulationDemand.TotalHours(TaskHours(8)))

    result.startDate shouldEqual Some(tuesday)
    result.estimatedCompletionDate shouldEqual Some(tuesday)

  it should "respect catalog task dependencies in the manufacturing-based mode" in:
    val cutting: TaskId = UUID.randomUUID().nn
    val assembly: TaskId = UUID.randomUUID().nn
    val dependencies = ManufacturingDependencies().addTaskDependencies(assembly, Set(cutting))
    val selection = NonEmptyList.one(
      template(NonEmptyList.of(cutting, assembly), dependencies) ->
        NonEmptyList.of(catalogTask(cutting, "Taglio", 8), catalogTask(assembly, "Assemblaggio", 4)),
    )
    val result = simulateOrFail(Nil, List(employee(employeeA)), SimulationDemand.FromManufacturings(selection))

    result.totalHours shouldEqual 12
    result.startDate shouldEqual Some(monday)
    // The dependent 4h task can only run the day after the 8h prerequisite completes.
    result.estimatedCompletionDate shouldEqual Some(tuesday)

  it should "report the simulated order as unfeasible when there is no workforce" in:
    val result = simulateOrFail(Nil, Nil, SimulationDemand.TotalHours(TaskHours(8)))

    result.estimatedCompletionDate shouldEqual None
    result.startDate shouldEqual None
    result.unplannedReasons should not be empty
end OrderSimulationServiceTest
