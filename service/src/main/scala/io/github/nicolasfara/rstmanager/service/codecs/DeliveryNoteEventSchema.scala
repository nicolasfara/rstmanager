package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events.DeliveryNoteEvent

import io.circe.{ Decoder, JsonObject }

/** Persisted schema of the [[DeliveryNoteEvent]] journal (`delivery_notes.journal`). v1 is the first and only version: every row is versioned. */
object DeliveryNoteEventSchema extends DerivedEventSchema[DeliveryNoteEvent](using WarehouseCodecs.deliveryNoteEventData):
  val currentVersion: Int = 1
  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = EventSchema.noUpcasters
  def legacyVersion(tag: String, data: JsonObject): Int = 1
