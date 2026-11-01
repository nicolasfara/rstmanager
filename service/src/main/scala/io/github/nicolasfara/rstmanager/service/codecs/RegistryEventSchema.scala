package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.registry.EntityRegistryEvent

import io.circe.{ Decoder, JsonObject }

/** Persisted schema of the entity registry journals (`*_index_journal`). v1 is the only version. */
object RegistryEventSchema extends DerivedEventSchema[EntityRegistryEvent](using RegistryCodecs.registryEventData):
  val currentVersion: Int = 1
  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = EventSchema.noUpcasters
  def legacyVersion(tag: String, data: JsonObject): Int = 1
