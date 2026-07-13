package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.registry.{ EntityRegistryEvent, EntityRegistryService }

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** Circe codecs for the durable entity registry, used to persist [[EntityRegistryEvent]] in the journal. */
object RegistryCodecs:
  given Codec[EntityRegistryEvent] = deriveCodec
  given Codec[EntityRegistryService.Notification] = deriveCodec
