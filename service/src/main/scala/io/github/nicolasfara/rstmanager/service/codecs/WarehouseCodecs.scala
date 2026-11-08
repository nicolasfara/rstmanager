package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.warehouse.domain.UnitOfMeasure
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ ConsumptionRef, MaterialId, MaterialInfo, MaterialService, Mpn }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierService }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent

import io.circe.{ Codec, Decoder, Encoder }
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.circe.given

/** Circe codecs for the warehouse aggregates (suppliers, materials), used to persist their events in the journal. */
object WarehouseCodecs:
  import CommonCodecs.given

  /** Units are written by their stable wire name (`"linear_meters"`), as in the API, not by their Scala case name. */
  given Codec[UnitOfMeasure] = Codec.from(Decoder.decodeString.emap(UnitOfMeasure.fromWireName), Encoder.encodeString.contramap(_.wireName))

  given Codec[Supplier] = deriveCodec

  /** Current-version data of the supplier journal; it is persisted through [[SupplierEventSchema]]. */
  val supplierEventData: Codec.AsObject[SupplierEvent] = deriveCodec
  given Codec[SupplierEvent] = EventSchema.codec(SupplierEventSchema)

  given supplierNotificationCodec: Codec[SupplierService.Notification] = deriveCodec

  given Codec[Mpn] = deriveCodec
  given Codec[MaterialInfo] = deriveCodec
  given Codec[ConsumptionRef] = deriveCodec

  /** Current-version data of the material journal; it is persisted through [[MaterialEventSchema]]. */
  val materialEventData: Codec.AsObject[MaterialEvent] = deriveCodec
  given Codec[MaterialEvent] = EventSchema.codec(MaterialEventSchema)

  /**
   * Written in the derived shape; also reads the `{"ProductChanged": {"productId": …}}` rows written to the outbox while the materials were products,
   * so the rows still pending when that release is replaced are published.
   */
  given materialNotificationCodec: Codec[MaterialService.Notification] =
    val derived: Codec.AsObject[MaterialService.Notification] = deriveCodec
    val legacy: Decoder[MaterialService.Notification] =
      Decoder.instance(_.downField("ProductChanged").get[MaterialId]("productId").map(MaterialService.Notification.MaterialChanged(_)))
    Codec.from(derived.or(legacy), derived)
end WarehouseCodecs
