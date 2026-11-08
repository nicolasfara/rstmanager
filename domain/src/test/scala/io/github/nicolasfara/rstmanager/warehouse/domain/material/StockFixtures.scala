package io.github.nicolasfara.rstmanager.warehouse.domain.material

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ SignedQuantity, UnitOfMeasure, UnitPrice, Valuation }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import org.scalacheck.Gen

/** Materials, ledgers, and generators shared by the stock ledger and warehouse report tests. */
object StockFixtures:
  val base: LocalDate = LocalDate.parse("2026-01-01")
  val firstSupplier: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005b1").nn)
  val secondSupplier: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005b2").nn)
  val reason: String :| MovementReason = "Inventario"
  val mpnCode: String :| MpnCode = "AC-100"

  def day(offset: Int): LocalDate = base.plusDays(offset).nn

  def movementId(n: Int): MovementId = MovementId(new UUID(0xabcL, n.toLong))

  def materialId(n: Int): MaterialId = MaterialId(new UUID(0xdefL, n.toLong))

  def orderId(n: Int): OrderId = OrderId(new UUID(0x0dL, n.toLong))

  def manufacturingId(n: Int): ScheduledManufacturingId = ScheduledManufacturingId(new UUID(0x3fL, n.toLong))

  /** The consumption of manufacturing `manufacturing` of order `order`. */
  def consumptionRef(order: Int, manufacturing: Int): ConsumptionRef = ConsumptionRef(orderId(order), manufacturingId(manufacturing))

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => sys.error(errors.toChain.toList.mkString(", ")), identity)

  def materialInfo(id: MaterialId, code: String, unit: UnitOfMeasure): MaterialInfo =
    valid(
      MaterialInfo.createMaterialInfo(
        id,
        code,
        s"Materiale $code",
        None,
        None,
        unit,
        UnitPrice.applyUnsafe(50000L),
        List(valid(Mpn.createMpn(mpnCode, firstSupplier, None)), valid(Mpn.createMpn(mpnCode, secondSupplier, None))),
      ),
    )

  def purchase(quantity: Long, cost: Long, supplierId: SupplierId): (SignedQuantity, MovementKind) =
    val parsed = SignedQuantity.applyUnsafe(quantity)
    (parsed, MovementKind.Purchase(mpnCode, supplierId, UnitPrice.applyUnsafe(cost), Valuation.amount(parsed.abs, UnitPrice.applyUnsafe(cost)), None))

  /** A raw movement for the generators: day offset, delta, kind, and whether it ends up voided. */
  final case class RawMovement(offset: Int, delta: SignedQuantity, kind: MovementKind, voided: Boolean)

  val genCost: Gen[UnitPrice] = Gen.chooseNum(0L, 200000L).map(UnitPrice.applyUnsafe(_))

  def genMovement(maxOffset: Int, voids: Boolean): Gen[RawMovement] =
    for
      offset <- Gen.chooseNum(0, maxOffset)
      magnitude <- Gen.chooseNum(1L, 9000L)
      incoming <- Gen.oneOf(true, false)
      supplierId <- Gen.oneOf(firstSupplier, secondSupplier)
      cost <- genCost
      optionalCost <- Gen.option(genCost)
      consumption = MovementKind.Consumption(consumptionRef(1, 1))
      kind <-
        if incoming then Gen.oneOf(purchase(magnitude, cost.value, supplierId)._2, MovementKind.Adjustment(optionalCost, reason), consumption)
        else Gen.oneOf(MovementKind.Unload(reason), MovementKind.Adjustment(None, reason), consumption)
      voided <- if voids then Gen.frequency(4 -> false, 1 -> true) else Gen.const(false)
    yield RawMovement(offset, SignedQuantity.applyUnsafe(if incoming then magnitude else -magnitude), kind, voided)

  /** The ledger of `raw` movements (ids from `firstId`), voids marked after every movement is recorded. */
  def ledgerOf(raw: List[RawMovement], firstId: Int, start: StockLedger): StockLedger =
    val withIds = raw.zipWithIndex.map((movement, index) => (movementId(firstId + index), movement))
    val recorded = withIds.foldLeft(start)((ledger, entry) => ledger.appended(entry._1, day(entry._2.offset), entry._2.delta, entry._2.kind))
    withIds.filter(_._2.voided).foldLeft(recorded)((ledger, entry) => ledger.voided(entry._1, MovementVoid(reason, day(entry._2.offset))))

  /** The ledger of `raw` movements alone. */
  def ledgerOf(raw: List[RawMovement]): StockLedger = ledgerOf(raw, 0, StockLedger.empty)

  def genLedger(maxOffset: Int, voids: Boolean): Gen[StockLedger] = Gen.listOf(genMovement(maxOffset, voids)).map(ledgerOf)
end StockFixtures
