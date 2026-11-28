package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.{ OrderFeasibilityService, PlanningTrigger }
import io.github.nicolasfara.rstmanager.planning.OrderFeasibilityService.{ FeasibilityResult, InducedDelay }
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.*
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.DateTime
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningApiDtosTest extends AnyFlatSpecLike:
  private val now: DateTime = DateTime.parse("2026-06-15")
  private val orderId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000001").nn
  private val employeeId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000002").nn
  private val generatedId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000003").nn

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  "PlanningAttemptRequest" should "support automatic entity selection when ids are omitted" in:
    val request = PlanningAttemptRequest(
      None,
      now.toString,
      PlanningTriggerDto("daily_planning", None, None, None),
      None,
      None,
      None,
    )

    val input = validOrFail(request.toDomain(now, generatedId))
    val planningRequest = input.toPlanningRequest(List(OrderId(orderId)))

    input.orderIds shouldBe None
    input.employeeIds shouldBe None
    planningRequest.id shouldEqual generatedId
    planningRequest.trigger shouldEqual PlanningTrigger.DailyPlanning
    planningRequest.openOrderIds shouldEqual List(orderId)

  it should "preserve explicit order and employee selections" in:
    val request = PlanningAttemptRequest(
      Some(generatedId),
      now.toString,
      PlanningTriggerDto("workforce_capacity_changed", None, None, None),
      Some(now.toString),
      Some(List(orderId)),
      Some(List(employeeId)),
    )

    val input = validOrFail(request.toDomain(now, UUID.randomUUID().nn))

    input.orderIds shouldEqual Some(List(orderId))
    input.employeeIds shouldEqual Some(List(employeeId))
    input.toPlanningRequest(List(OrderId(orderId))).trigger shouldEqual PlanningTrigger.WorkforceCapacityChanged

  "OrderFeasibilityResponse" should "expose the delays and the suggested deadline of a feasibility check" in:
    val tuesday = now.plusDays(1).nn
    val wednesday = now.plusDays(2).nn
    val result = FeasibilityResult(
      Some(wednesday),
      late = true,
      Nil,
      List(InducedDelay(OrderId(orderId), "ORD-1", now, None)),
      Some(wednesday),
      Nil,
    )

    val response = OrderFeasibilityResponse.fromDomain(tuesday, result)

    response.late shouldBe true
    response.hasDelay shouldBe true
    response.deadline shouldEqual tuesday.toString
    response.plannedCompletion shouldEqual Some(wednesday.toString)
    response.suggestedDeadline shouldEqual Some(wednesday.toString)
    response.inducedDelays.map(delay => (delay.orderId, delay.number, delay.plannedCompletion)) shouldEqual List((orderId, "ORD-1", None))
    OrderFeasibilityService.candidateId.value should not equal orderId
end PlanningApiDtosTest
