package io.github.nicolasfara.rstmanager.warehouse.domain.material

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.{ Amount, Quantity, SignedQuantity, UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.Id
import cats.data.{ Chain, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class MaterialServiceTest extends AnyFlatSpecLike:
  private val materialId: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-0000000006a1").nn)
  private val firstSupplier: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005a1").nn)
  private val secondSupplier: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005a2").nn)

  /** 23:30 UTC on 14 June is already 15 June in Rome. */
  private val lateEvening: Instant = Instant.parse("2026-06-14T23:30:00Z").nn
  private val romeDay: LocalDate = LocalDate.parse("2026-06-15")

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def mpn(code: String, supplierId: SupplierId): Mpn = valid(Mpn.createMpn(code, supplierId, None))

  private def info(price: Long, unit: UnitOfMeasure, mpns: List[Mpn]): MaterialInfo =
    valid(
      MaterialInfo.createMaterialInfo(
        materialId,
        " tes-001 ",
        "Tessuto acrilico blu",
        Some("Tessuti"),
        None,
        unit,
        UnitPrice.applyUnsafe(price),
        mpns,
      ),
    )

  private val material: MaterialInfo = info(37000L, UnitOfMeasure.LinearMeters, List(mpn("ac-100", firstSupplier), mpn("ac-100", secondSupplier)))
  private val record: MaterialRecord = MaterialRecord.created(material, LocalDate.parse("2026-06-01"))

  private type Result = EdomatonResult[MaterialAggregate, MaterialEvent, MaterialError, Notification]

  private def run(command: Command, state: MaterialAggregate): Result = run(command, state, materialId, lateEvening)

  private def run(command: Command, state: MaterialAggregate, address: MaterialId, at: Instant): Result =
    MaterialService[Id].execute(RequestContext(CommandMessage("message-1", at, address.toString, command), state))

  private def rejection(result: Result): List[MaterialError] =
    result match
      case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
      case other => fail(s"Expected a rejection, got: $other")

  private def accepted(result: Result): (MaterialAggregate, List[MaterialEvent], List[Notification]) =
    result match
      case EdomatonResult.Accepted(state, events, notifications) => (state, events.toList, notifications.toList)
      case other => fail(s"Expected an acceptance, got: $other")

  private val movement: MovementId = MovementId(UUID.fromString("00000000-0000-0000-0000-0000000007a1").nn)
  private val code: String :| MpnCode = "AC-100"
  private val reason: String :| MovementReason = "Campionatura"
  private val ref: ConsumptionRef = StockFixtures.consumptionRef(1, 1)

  private def purchase(on: Option[LocalDate]): Command =
    Command.RecordPurchase(movement, code, secondSupplier, Quantity.applyUnsafe(2500L), UnitPrice.applyUnsafe(37000L), Some("DDT 7"), on)

  "MaterialInfo" should "normalize codes and derive its distinct suppliers from the MPNs" in:
    (material.internalCode: String) shouldEqual "TES-001"
    material.mpns.map(m => m.code: String) shouldEqual List("AC-100", "AC-100")
    material.suppliers shouldEqual List(firstSupplier, secondSupplier)
    info(1L, UnitOfMeasure.Pieces, List(mpn("A", firstSupplier), mpn("B", firstSupplier))).suppliers shouldEqual List(firstSupplier)

  "MaterialService" should "create a material dated on the business day of the command time" in:
    val (state, events, notifications) = accepted(run(Command.Create(material), MaterialAggregate.Empty))
    events shouldEqual List(MaterialEvent.MaterialCreated(material, romeDay))
    state shouldEqual MaterialAggregate.Active(MaterialRecord(material, romeDay, List(CustomerPricePoint(romeDay, material.customerPrice))))
    notifications shouldEqual List(Notification.MaterialChanged(materialId))

  it should "reject the same MPN code twice for one supplier, normalized" in:
    val duplicated = info(1L, UnitOfMeasure.Pieces, List(mpn("x-1", firstSupplier), mpn(" X-1 ", firstSupplier), mpn("X-1", secondSupplier)))
    rejection(run(Command.Create(duplicated), MaterialAggregate.Empty)) shouldEqual List(MaterialError.DuplicateMpn(firstSupplier, "X-1"))
    rejection(run(Command.Update(duplicated), MaterialAggregate.Active(record))) shouldEqual List(MaterialError.DuplicateMpn(firstSupplier, "X-1"))

  it should "reject creating an existing material" in:
    rejection(run(Command.Create(material), MaterialAggregate.Active(record))) shouldEqual List(MaterialError.MaterialAlreadyExists)
    rejection(run(Command.Create(material), MaterialAggregate.Archived(record, romeDay))) shouldEqual List(MaterialError.MaterialAlreadyExists)

  it should "update an active material, recording the new customer price from the business day" in:
    val repriced = material.copy(customerPrice = UnitPrice.applyUnsafe(40000L), unit = UnitOfMeasure.SquareMeters)
    val (state, events, _) = accepted(run(Command.Update(repriced), MaterialAggregate.Active(record)))
    events shouldEqual List(MaterialEvent.MaterialUpdated(repriced, romeDay))
    state match
      case MaterialAggregate.Active(updated) =>
        updated.info shouldEqual repriced
        updated.createdOn shouldEqual record.createdOn
        updated.customerPriceAt(LocalDate.parse("2026-06-14")) shouldEqual Some(material.customerPrice)
        updated.customerPriceAt(romeDay) shouldEqual Some(repriced.customerPrice)
        updated.customerPriceAt(LocalDate.parse("2026-05-31")) shouldEqual Some(material.customerPrice)
      case other => fail(s"Unexpected state: $other")

  it should "reject changes to a missing or archived material" in:
    rejection(run(Command.Update(material), MaterialAggregate.Empty)) shouldEqual List(MaterialError.MaterialNotFound)
    rejection(run(Command.Update(material), MaterialAggregate.Archived(record, romeDay))) shouldEqual List(MaterialError.MaterialIsArchived)
    rejection(run(Command.Archive, MaterialAggregate.Empty)) shouldEqual List(MaterialError.MaterialNotFound)
    rejection(run(Command.Archive, MaterialAggregate.Archived(record, romeDay))) shouldEqual List(MaterialError.MaterialIsArchived)

  it should "archive a material on the business day, keeping its record, and publish its id" in:
    val (state, events, notifications) = accepted(run(Command.Archive, MaterialAggregate.Active(record)))
    events shouldEqual List(MaterialEvent.MaterialArchived(romeDay))
    state shouldEqual MaterialAggregate.Archived(record, romeDay)
    notifications shouldEqual List(Notification.MaterialChanged(materialId))

  it should "reject a material sent to another material's stream" in:
    val otherStream = MaterialId(UUID.fromString("00000000-0000-0000-0000-0000000006a2").nn)
    rejection(run(Command.Create(material), MaterialAggregate.Empty, otherStream, lateEvening)) shouldEqual List(MaterialError.MaterialIdMismatch)

  "MaterialRecord price history" should "keep one point per day and drop unchanged prices" in:
    val day1 = LocalDate.parse("2026-06-01")
    val day2 = LocalDate.parse("2026-06-02")
    val price = (cents: Long) => material.copy(customerPrice = UnitPrice.applyUnsafe(cents))
    val history = MaterialRecord
      .created(price(100L), day1)
      .updated(price(100L), day1) // same price: no new point
      .updated(price(200L), day2)
      .updated(price(300L), day2) // same day: last price wins
      .updated(price(300L), day2.plusDays(1).nn) // unchanged: no new point
      .customerPrices
    history shouldEqual List(CustomerPricePoint(day1, UnitPrice.applyUnsafe(100L)), CustomerPricePoint(day2, UnitPrice.applyUnsafe(300L)))

  "MaterialAggregate fold" should "replay the whole lifecycle" in:
    val events = List(
      MaterialEvent.MaterialCreated(material, LocalDate.parse("2026-06-01")),
      MaterialEvent.MaterialUpdated(material, LocalDate.parse("2026-06-02")),
      MaterialEvent.MaterialArchived(LocalDate.parse("2026-06-03")),
    )
    val folded =
      events.foldLeft(MaterialAggregate.initial.validNec[MaterialError])((state, event) => state.andThen(MaterialAggregate.transition(event)))
    folded.toOption shouldEqual Some(MaterialAggregate.Archived(record, LocalDate.parse("2026-06-03")))
  "MaterialService movements" should "date a movement without a day on the business day of the command, and publish the material" in:
    val (state, events, notifications) = accepted(run(purchase(None), MaterialAggregate.Active(record)))
    events shouldEqual List(
      MaterialEvent.PurchaseRecorded(
        movement,
        code,
        secondSupplier,
        Quantity.applyUnsafe(2500L),
        UnitPrice.applyUnsafe(37000L),
        Amount.applyUnsafe(925L),
        romeDay,
        Some("DDT 7"),
      ),
    )
    notifications shouldEqual List(Notification.MaterialChanged(materialId))
    state match
      case MaterialAggregate.Active(material) =>
        material.hasMovements shouldEqual true
        material.ledger.balanceAt(romeDay) shouldEqual SignedQuantity.applyUnsafe(2500L)
      case other => fail(s"Unexpected state: $other")

  it should "keep a backdated day and reject a day after the command's business day" in:
    val (_, events, _) = accepted(run(purchase(Some(LocalDate.parse("2026-06-10"))), MaterialAggregate.Active(record)))
    events.collect { case event: MaterialEvent.PurchaseRecorded => event.on } shouldEqual List(LocalDate.parse("2026-06-10"))
    rejection(run(purchase(Some(LocalDate.parse("2026-06-16"))), MaterialAggregate.Active(record))) shouldEqual
      List(MaterialError.MovementInFuture(LocalDate.parse("2026-06-16"), romeDay))

  it should "lock the unit of measure once a movement exists, even voided" in:
    val (bought, _, _) = accepted(run(purchase(None), MaterialAggregate.Active(record)))
    val (voided, _, _) = accepted(run(Command.VoidMovement(movement, reason), bought))
    val otherUnit = material.copy(unit = UnitOfMeasure.SquareMeters)
    rejection(run(Command.Update(otherUnit), voided)) shouldEqual List(MaterialError.UnitChangeWithMovements)
    accepted(run(Command.Update(otherUnit), MaterialAggregate.Active(record)))

  it should "reject manual unloads beyond the stock and accept them within it" in:
    val (bought, _, _) = accepted(run(purchase(None), MaterialAggregate.Active(record)))
    val other = MovementId(UUID.fromString("00000000-0000-0000-0000-0000000007a2").nn)
    rejection(run(Command.Unload(other, Quantity.applyUnsafe(2501L), reason, None), bought)) shouldEqual
      List(MaterialError.InsufficientStock(romeDay, SignedQuantity.applyUnsafe(2500L)))
    val (_, events, _) = accepted(run(Command.Unload(other, Quantity.applyUnsafe(2500L), reason, None), bought))
    events shouldEqual List(MaterialEvent.StockUnloaded(other, Quantity.applyUnsafe(2500L), reason, romeDay))

  it should "reject movements on an archived material" in:
    rejection(run(purchase(None), MaterialAggregate.Archived(record, romeDay))) shouldEqual List(MaterialError.MaterialIsArchived)
    rejection(run(Command.Adjust(movement, SignedQuantity.applyUnsafe(1000L), None, reason, None), MaterialAggregate.Empty)) shouldEqual
      List(MaterialError.MaterialNotFound)

  "MaterialService consumptions" should "decide nothing, and publish nothing, when the quantity is already the requested one" in:
    val (consumed, events, _) = accepted(run(Command.SetConsumption(ref, Quantity.applyUnsafe(1500L), romeDay), MaterialAggregate.Active(record)))
    events shouldEqual List(MaterialEvent.ConsumptionSet(ref, Quantity.applyUnsafe(1500L), romeDay))
    run(Command.SetConsumption(ref, Quantity.applyUnsafe(1500L), romeDay.minusDays(3).nn), consumed) shouldEqual EdomatonResult.Indecisive(
      Chain.empty,
    )
    run(Command.SetConsumption(ref, Quantity.zero, romeDay), MaterialAggregate.Active(record)) shouldEqual EdomatonResult.Indecisive(Chain.empty)

  it should "be accepted on an archived material, beyond its stock" in:
    val (state, events, notifications) =
      accepted(run(Command.SetConsumption(ref, Quantity.applyUnsafe(2000L), romeDay), MaterialAggregate.Archived(record, romeDay)))
    events shouldEqual List(MaterialEvent.ConsumptionSet(ref, Quantity.applyUnsafe(2000L), romeDay))
    notifications shouldEqual List(Notification.MaterialChanged(materialId))
    state match
      case MaterialAggregate.Archived(material, _) => material.ledger.balanceAt(romeDay) shouldEqual SignedQuantity.applyUnsafe(-2000L)
      case other => fail(s"Unexpected state: $other")

  it should "reject the manual void of a consumption" in:
    val (consumed, _, _) = accepted(run(Command.SetConsumption(ref, Quantity.applyUnsafe(1500L), romeDay), MaterialAggregate.Active(record)))
    val consumption = StockLedger.consumptionMovementId(ref, 0)
    rejection(run(Command.VoidMovement(consumption, reason), consumed)) shouldEqual List(MaterialError.ConsumptionNotVoidable(consumption))
end MaterialServiceTest
