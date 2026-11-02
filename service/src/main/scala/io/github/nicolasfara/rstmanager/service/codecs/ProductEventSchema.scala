package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent

import io.circe.{ Decoder, JsonObject }

/**
 * Persisted schema of the [[ProductEvent]] journal (`products.journal`). v1 is the first and only version: every row is versioned. New event cases
 * (stock movements, automatic consumptions) are added without a bump; changing the payload of an existing case requires one.
 */
object ProductEventSchema extends DerivedEventSchema[ProductEvent](using WarehouseCodecs.productEventData):
  val currentVersion: Int = 1
  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = EventSchema.noUpcasters
  def legacyVersion(tag: String, data: JsonObject): Int = 1
