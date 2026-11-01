package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.registry.{ EntityRegistryEvent, EntityRegistryService }

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** Circe codecs for the durable entity registry, used to persist [[EntityRegistryEvent]] in the journal. */
object RegistryCodecs:
  /** Current-version data of the registry journals; they are persisted through [[RegistryEventSchema]]. */
  val registryEventData: Codec.AsObject[EntityRegistryEvent] = deriveCodec
  given Codec[EntityRegistryEvent] = EventSchema.codec(RegistryEventSchema)

  given Codec[EntityRegistryService.Notification] = deriveCodec
