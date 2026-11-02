package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.warehouse.domain.UnitOfMeasure
import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ ConsumptionRef, Mpn, ProductInfo, ProductService }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierService }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent

import io.circe.{ Codec, Decoder, Encoder }
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.circe.given

/** Circe codecs for the warehouse aggregates (suppliers, products), used to persist their events in the journal. */
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
  given Codec[ProductInfo] = deriveCodec
  given Codec[ConsumptionRef] = deriveCodec

  /** Current-version data of the product journal; it is persisted through [[ProductEventSchema]]. */
  val productEventData: Codec.AsObject[ProductEvent] = deriveCodec
  given Codec[ProductEvent] = EventSchema.codec(ProductEventSchema)

  given productNotificationCodec: Codec[ProductService.Notification] = deriveCodec
end WarehouseCodecs
