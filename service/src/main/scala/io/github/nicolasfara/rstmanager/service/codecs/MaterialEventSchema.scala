package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent

import io.circe.{ Decoder, JsonObject }

/**
 * Persisted schema of the [[MaterialEvent]] journal, stored in the `products` namespace (`products_journal`), the name the warehouse items had when
 * the journal was created. Every row is versioned. New event cases (stock movements, automatic consumptions) are added without a bump; changing the
 * payload of an existing case requires one.
 *
 * Versions:
 *   - v1: the warehouse items are products: `ProductCreated` / `ProductUpdated` / `ProductArchived`, with a `productType` in the info.
 *   - v2: they are materials: `MaterialCreated` / `MaterialUpdated` / `MaterialArchived`, with a `materialType`. Nothing else changed.
 */
object MaterialEventSchema extends DerivedEventSchema[MaterialEvent](using WarehouseCodecs.materialEventData):
  val currentVersion: Int = 2

  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = Map(
    1 -> Upcasts.rename(
      1,
      Map("ProductCreated" -> "MaterialCreated", "ProductUpdated" -> "MaterialUpdated", "ProductArchived" -> "MaterialArchived"),
      Map("productType" -> "materialType"),
    ),
  )

  def legacyVersion(tag: String, data: JsonObject): Int = 1
