package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent

class OrderEventSchemaTest extends JournalGoldenSuite[OrderEvent]("order-events", OrderEventSchema, JournalGoldenSuite.caseNames[OrderEvent]):
  def current: List[String] = List(
    "v2-order-created",
    "v2-order-created-minimal",
    "v2-order-created-in-progress",
    "v2-order-cancelled",
    "v2-order-suspended",
    "v2-order-reactivated",
    "v2-order-completed",
    "v2-order-delivered",
    "v2-promised-delivery-date-changed",
    "v2-priority-changed",
    "v2-description-changed",
    "v2-manufacturing-added",
    "v2-manufacturing-removed",
    "v2-manufacturing-description-changed",
    "v2-manufacturing-completion-date-changed",
    "v2-manufacturing-preferred-employee-changed",
    "v2-task-preferred-employee-changed",
    "v2-manufacturing-status-changed",
    "v2-task-added",
    "v2-task-removed",
    "v2-task-advanced",
    "v2-task-rolled-back",
    "v2-task-progress-set",
    "v2-task-expected-duration-changed",
    "v2-task-completed",
    "v2-task-completed-untimed",
    "v2-task-reverted",
    "v2-manufacturing-dependencies-changed",
    "v2-task-dependencies-changed",
  )

  /**
   * Pre-version rows: in hours (v1, from the first release with its placeholder manufacturing code to the last one before minutes) and in
   * minutes before `occurredOn` existed (v2).
   */
  def legacy: List[(String, String)] = List(
    "legacy-v1-order-created-first-release" -> "v2-order-created-minimal",
    "legacy-v1-order-created" -> "v2-order-created-in-progress",
    "legacy-v1-manufacturing-added" -> "v2-manufacturing-added",
    "legacy-v1-task-added" -> "v2-task-added",
    "legacy-v1-task-completed" -> "v2-task-completed-untimed",
    "legacy-v1-task-progress-set" -> "v2-task-progress-set",
    "legacy-v1-task-expected-hours-changed" -> "v2-task-expected-duration-changed",
    "legacy-v2-task-completed" -> "v2-task-completed-untimed",
  )
end OrderEventSchemaTest
