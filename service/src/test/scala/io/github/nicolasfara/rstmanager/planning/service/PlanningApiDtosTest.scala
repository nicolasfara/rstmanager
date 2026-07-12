package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.PlanningTrigger
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.*

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.DateTime
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningApiDtosTest extends AnyFlatSpecLike:
  private val now: DateTime = DateTime.parse("2026-06-15").nn
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
    val planningRequest = input.toPlanningRequest(List(orderId))

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
    input.toPlanningRequest(List(orderId)).trigger shouldEqual PlanningTrigger.WorkforceCapacityChanged
end PlanningApiDtosTest
