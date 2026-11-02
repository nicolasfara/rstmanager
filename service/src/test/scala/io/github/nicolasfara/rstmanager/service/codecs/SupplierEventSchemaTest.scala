package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent

class SupplierEventSchemaTest
    extends JournalGoldenSuite[SupplierEvent]("supplier-events", SupplierEventSchema, JournalGoldenSuite.caseNames[SupplierEvent]):
  def current: List[String] = List("v1-supplier-created", "v1-supplier-created-minimal", "v1-supplier-updated", "v1-supplier-deleted")

  /** The supplier journal was versioned from its first release: there are no legacy rows. */
  def legacy: List[(String, String)] = Nil
