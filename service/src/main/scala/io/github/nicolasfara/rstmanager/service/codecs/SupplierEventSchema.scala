package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent

import io.circe.{ Decoder, JsonObject }

/** Persisted schema of the [[SupplierEvent]] journal (`suppliers.journal`). v1 is the first and only version: every row is versioned. */
object SupplierEventSchema extends DerivedEventSchema[SupplierEvent](using WarehouseCodecs.supplierEventData):
  val currentVersion: Int = 1
  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = EventSchema.noUpcasters
  def legacyVersion(tag: String, data: JsonObject): Int = 1
