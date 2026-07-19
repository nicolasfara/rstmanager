package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.customer.domain.*
import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent

import io.circe.Codec
import io.circe.derivation.{ Configuration, ConfiguredCodec }
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.circe.given

/** Circe codecs for the customer aggregate graph, used to persist [[CustomerEvent]] in the journal. */
object CustomerCodecs:
  // Defaults let events persisted before the master-data extension (business name, PEC, notes, boat) decode correctly.
  private given Configuration = Configuration.default.withDefaults
  given Codec[ContactInfo] = deriveCodec
  given Codec[Address] = deriveCodec
  given Codec[CustomerType] = deriveCodec
  given Codec[BoatInfo] = deriveCodec
  given Codec[Customer] = ConfiguredCodec.derived
  given Codec[CustomerEvent] = deriveCodec
  given Codec[CustomerService.Notification] = deriveCodec
