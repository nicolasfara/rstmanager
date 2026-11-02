package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent

class ProductEventSchemaTest
    extends JournalGoldenSuite[ProductEvent]("product-events", ProductEventSchema, JournalGoldenSuite.caseNames[ProductEvent]):
  def current: List[String] =
    List(
      "v1-product-created",
      "v1-product-created-minimal",
      "v1-product-updated",
      "v1-product-updated-kilograms",
      "v1-product-archived",
      "v1-purchase-recorded",
      "v1-purchase-recorded-no-note",
      "v1-stock-unloaded",
      "v1-stock-adjusted",
      "v1-stock-adjusted-outgoing",
      "v1-movement-voided",
      "v1-consumption-set",
      "v1-consumption-set-reversal",
    )

  /** The product journal was versioned from its first release: there are no legacy rows. */
  def legacy: List[(String, String)] = Nil
end ProductEventSchemaTest
