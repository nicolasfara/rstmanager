package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.{ DelayedOrder, PlanningRequestId, PlanningService }
import io.github.nicolasfara.rstmanager.planning.service.PlanningCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

import com.github.nscala_time.time.Imports.DateTime
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningCodecsTest extends AnyFlatSpecLike:
  private val orderId: OrderId = OrderId(UUID.fromString("00000000-0000-0000-0000-0000000000d1").nn)
  private val deadline = DateTime.parse("2026-06-15T00:00:00.000Z")
  private val plannedCompletion = DateTime.parse("2026-06-17T00:00:00.000Z")

  /** Delays as the journal and the outbox stored them before the fields were renamed. */
  private val persistedDelay =
    """{"orderId":"00000000-0000-0000-0000-0000000000d1","expectedDeliveryDate":"2026-06-15T00:00:00.000Z","promisedDeliveryDate":"2026-06-17T00:00:00.000Z"}"""

  "PlanningCodecs" should "keep the persisted JSON keys of a delayed order" in:
    val delay = DelayedOrder(orderId, deadline, plannedCompletion)

    decode[DelayedOrder](persistedDelay).map(decoded => (decoded.deadline.getMillis, decoded.plannedCompletion.getMillis)) shouldEqual
      Right((deadline.getMillis, plannedCompletion.getMillis))
    delay.asJson.hcursor.keys.map(_.toList) shouldEqual Some(List("orderId", "expectedDeliveryDate", "promisedDeliveryDate"))

  it should "keep the persisted JSON keys of the order-delayed notification and derive the others" in:
    val delayed: PlanningService.Notification = PlanningService.Notification.OrderDelayed(orderId, deadline, plannedCompletion)
    val completed: PlanningService.Notification =
      PlanningService.Notification.PlanningCompleted(PlanningRequestId(UUID.fromString("00000000-0000-0000-0000-0000000000d2").nn))

    decode[PlanningService.Notification](s"""{"OrderDelayed":$persistedDelay}""").map {
      case PlanningService.Notification.OrderDelayed(id, decodedDeadline, decodedCompletion) =>
        (id, decodedDeadline.getMillis, decodedCompletion.getMillis)
      case other => fail(s"Unexpected notification: $other")
    } shouldEqual Right((orderId, deadline.getMillis, plannedCompletion.getMillis))
    delayed.asJson.hcursor.downField("OrderDelayed").keys.map(_.toList) shouldEqual Some(
      List("orderId", "expectedDeliveryDate", "promisedDeliveryDate"),
    )
    decode[PlanningService.Notification](completed.asJson.noSpaces) shouldEqual Right(completed)
end PlanningCodecsTest
