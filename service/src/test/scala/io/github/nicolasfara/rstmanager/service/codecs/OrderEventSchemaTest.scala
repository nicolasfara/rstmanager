package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent

import org.scalatest.matchers.should.Matchers.*

class OrderEventSchemaTest extends JournalGoldenSuite[OrderEvent]("order-events", OrderEventSchema, JournalGoldenSuite.caseNames[OrderEvent]):
  def current: List[String] = v3Counterparts.map("v4-" + _)

  /** Every case written by the previous release (v3, while the material lines named `productId`): each v3 fixture has a v4 counterpart. */
  private def v3Counterparts: List[String] = v2Counterparts ++ List("manufacturing-added-with-materials", "manufacturing-materials-changed")

  /** Every case written before materials (v2): each v2 fixture has a v4 counterpart with `"materials": []`. */
  private def v2Counterparts: List[String] = List(
    "order-created",
    "order-created-minimal",
    "order-created-in-progress",
    "order-cancelled",
    "order-suspended",
    "order-reactivated",
    "order-completed",
    "order-delivered",
    "promised-delivery-date-changed",
    "priority-changed",
    "description-changed",
    "manufacturing-added",
    "manufacturing-removed",
    "manufacturing-description-changed",
    "manufacturing-completion-date-changed",
    "manufacturing-preferred-employee-changed",
    "task-preferred-employee-changed",
    "manufacturing-status-changed",
    "task-added",
    "task-removed",
    "task-advanced",
    "task-rolled-back",
    "task-progress-set",
    "task-expected-duration-changed",
    "task-completed",
    "task-completed-untimed",
    "task-reverted",
    "manufacturing-dependencies-changed",
    "task-dependencies-changed",
  )

  /**
   * Pre-version rows: in hours (v1, from the first release with its placeholder manufacturing code to the last one before minutes) and in minutes
   * before `occurredOn` existed (v2). Then the versioned v2 rows, written before materials existed, and the v3 rows naming `productId`.
   */
  def legacy: List[(String, String)] = List(
    "legacy-v1-order-created-first-release" -> "v4-order-created-minimal",
    "legacy-v1-order-created" -> "v4-order-created-in-progress",
    "legacy-v1-manufacturing-added" -> "v4-manufacturing-added",
    "legacy-v1-task-added" -> "v4-task-added",
    "legacy-v1-task-completed" -> "v4-task-completed-untimed",
    "legacy-v1-task-progress-set" -> "v4-task-progress-set",
    "legacy-v1-task-expected-hours-changed" -> "v4-task-expected-duration-changed",
    "legacy-v2-task-completed" -> "v4-task-completed-untimed",
  ) ++ v2Counterparts.map(name => s"v2-$name" -> s"v4-$name") ++
    v3Counterparts.map(name => s"v3-$name" -> s"v4-$name")

  "The order journal codec" should "read every manufacturing of a v2 row with no materials" in:
    val decoded = List("v2-order-created", "v2-order-created-in-progress", "v2-manufacturing-added").map(decode)
    val manufacturings = decoded.flatMap {
      case OrderEvent.OrderCreated(data, _) => data.setOfManufacturing.toList
      case OrderEvent.ManufacturingAdded(manufacturing, _) => List(manufacturing)
      case other => fail(s"Unexpected event $other")
    }
    manufacturings should not be empty
    manufacturings.foreach(_.info.materials shouldBe empty)

  it should "read the materials of a v3 row with their frozen prices" in:
    decode("v3-manufacturing-materials-changed") match
      case OrderEvent.ManufacturingMaterialsChanged(_, materials, _) =>
        materials.map(material => (material.quantity.toDecimalString, material.unitPrice.toDecimalString)) shouldEqual
          List(("2.500", "18.5000"), ("4.000", "12.5000"))
      case other => fail(s"Unexpected event $other")
end OrderEventSchemaTest
