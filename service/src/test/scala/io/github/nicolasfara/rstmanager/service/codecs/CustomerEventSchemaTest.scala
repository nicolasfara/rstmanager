package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent
import io.github.nicolasfara.rstmanager.service.codecs.CustomerCodecs.given

class CustomerEventSchemaTest
    extends JournalGoldenSuite[CustomerEvent]("customer-events", CustomerEventSchema, JournalGoldenSuite.caseNames[CustomerEvent]):
  def current: List[String] = List("v1-customer-created", "v1-customer-created-minimal", "v1-customer-updated", "v1-customer-deleted")

  /** A pre-version row written before the master-data extension (no business name, PEC, notes, boat). */
  def legacy: List[(String, String)] = List(
    "legacy-v1-customer-created-before-master-data" -> "v1-customer-created-minimal",
    "legacy-v1-customer-deleted" -> "v1-customer-deleted",
  )
