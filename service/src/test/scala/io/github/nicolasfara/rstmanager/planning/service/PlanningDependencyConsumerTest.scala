package io.github.nicolasfara.rstmanager.planning.service

import java.time.OffsetDateTime
import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeService
import io.github.nicolasfara.rstmanager.planning.PlanningTrigger
import io.github.nicolasfara.rstmanager.work.domain.order.OrderService

import edomata.backend.OutboxItem
import edomata.core.MessageMetadata
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningDependencyConsumerTest extends AnyFlatSpecLike:
  private val orderId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000021").nn
  private val employeeId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000022").nn
  private val time: OffsetDateTime = OffsetDateTime.parse("2026-06-15T10:15:30Z").nn

  "PlanningDependencyConsumer" should "derive a stable order recalculation cause from an outbox item" in:
    val item = OutboxItem(
      12L,
      orderId.toString,
      time,
      OrderService.Notification.SchedulingRecalculationRequested(orderId),
      MessageMetadata("order-command"),
    )

    val first = PlanningDependencyConsumer.orderCause(item)
    val second = PlanningDependencyConsumer.orderCause(item)

    first shouldEqual second
    first.trigger shouldEqual PlanningTrigger.OrderChanged(orderId)
    first.commandId shouldEqual Some(s"planning-recalc-orders-$orderId-12")
    first.requestId should not be empty

  it should "map employee changes to workforce recalculation" in:
    val item = OutboxItem(
      9L,
      employeeId.toString,
      time,
      EmployeeService.Notification.EmployeeChanged(employeeId),
      MessageMetadata("employee-command"),
    )

    val cause = PlanningDependencyConsumer.employeeCause(item)

    cause.trigger shouldEqual PlanningTrigger.WorkforceCapacityChanged
    cause.commandId shouldEqual Some(s"planning-recalc-employees-$employeeId-9")
    cause.requestId should not be empty
end PlanningDependencyConsumerTest
