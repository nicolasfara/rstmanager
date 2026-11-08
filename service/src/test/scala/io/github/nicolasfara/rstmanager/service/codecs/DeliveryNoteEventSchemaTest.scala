package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WarehouseCodecs.given
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events.DeliveryNoteEvent

class DeliveryNoteEventSchemaTest
    extends JournalGoldenSuite[DeliveryNoteEvent]("delivery-note-events", DeliveryNoteEventSchema, JournalGoldenSuite.caseNames[DeliveryNoteEvent]):
  def current: List[String] =
    List("v1-delivery-note-registered", "v1-delivery-note-registered-minimal", "v1-delivery-note-changed", "v1-delivery-note-cancelled")

  /** The delivery note journal was versioned from its first release: there are no legacy rows. */
  def legacy: List[(String, String)] = Nil
