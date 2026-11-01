package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent

import io.circe.{ Decoder, JsonObject }

/**
 * Persisted schema of the [[CustomerEvent]] journal (`customers_journal`). v1 is the only version: rows written before the master-data extension lack
 * `businessName`, `pec`, `notes` and `boat`, which [[CustomerCodecs]] reads back as their defaults.
 */
object CustomerEventSchema extends DerivedEventSchema[CustomerEvent](using CustomerCodecs.customerEventData):
  val currentVersion: Int = 1
  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = EventSchema.noUpcasters
  def legacyVersion(tag: String, data: JsonObject): Int = 1
