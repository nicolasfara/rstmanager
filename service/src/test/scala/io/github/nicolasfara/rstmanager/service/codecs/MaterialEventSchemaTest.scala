package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent

class MaterialEventSchemaTest
    extends JournalGoldenSuite[MaterialEvent]("material-events", MaterialEventSchema, JournalGoldenSuite.caseNames[MaterialEvent]):
  def current: List[String] = v1Counterparts.values.toList.map("v2-" + _)

  /** Every v1 fixture (written while the warehouse items were products) with its v2 counterpart. */
  private def v1Counterparts: Map[String, String] = Map(
    "product-created" -> "material-created",
    "product-created-minimal" -> "material-created-minimal",
    "product-updated" -> "material-updated",
    "product-updated-kilograms" -> "material-updated-kilograms",
    "product-archived" -> "material-archived",
    "purchase-recorded" -> "purchase-recorded",
    "purchase-recorded-no-note" -> "purchase-recorded-no-note",
    "stock-unloaded" -> "stock-unloaded",
    "stock-adjusted" -> "stock-adjusted",
    "stock-adjusted-outgoing" -> "stock-adjusted-outgoing",
    "movement-voided" -> "movement-voided",
    "consumption-set" -> "consumption-set",
    "consumption-set-reversal" -> "consumption-set-reversal",
  )

  /** The journal was versioned from its first release: the legacy rows are the v1 ones, with the product names. */
  def legacy: List[(String, String)] = v1Counterparts.toList.map((v1, v2) => s"v1-$v1" -> s"v2-$v2")
end MaterialEventSchemaTest
