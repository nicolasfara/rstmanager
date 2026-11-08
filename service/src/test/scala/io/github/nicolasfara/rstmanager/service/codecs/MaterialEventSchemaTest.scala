package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent

class MaterialEventSchemaTest
    extends JournalGoldenSuite[MaterialEvent]("material-events", MaterialEventSchema, JournalGoldenSuite.caseNames[MaterialEvent]):
  def current: List[String] =
    v1Counterparts.values.toList.map("v3-" + _) ++
      List("v3-material-created-expected-cost", "v3-mpn-added", "v3-delivery-note-purchases-set", "v3-delivery-note-purchases-set-removed")

  /** Every v1 fixture (written while the warehouse items were products) with its v2 and v3 counterpart. */
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

  /**
   * The journal was versioned from its first release: the legacy rows are the v1 ones, with the product names, and the v2 ones, whose MPNs have no
   * expected unit cost.
   */
  def legacy: List[(String, String)] =
    v1Counterparts.toList.flatMap((v1, name) => List(s"v1-$v1" -> s"v3-$name", s"v2-$name" -> s"v3-$name"))
end MaterialEventSchemaTest
