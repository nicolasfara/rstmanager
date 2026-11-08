package io.github.nicolasfara.rstmanager.warehouse.domain

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{
  DeliveryNotePurchaseLine,
  MaterialAggregate,
  MaterialId,
  MaterialRecord,
  MaterialService,
}
import io.github.nicolasfara.rstmanager.warehouse.domain.material.StockFixtures.*

import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.Decision
import io.github.iltotore.iron.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DeliveryNoteReconciliationTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:
  private type SetPurchases = MaterialService.Command.SetDeliveryNotePurchases

  private val today: LocalDate = day(30)
  private val noteId: DeliveryNoteId = DeliveryNoteId(new UUID(0xdd7L, 1L))
  private val otherNoteId: DeliveryNoteId = DeliveryNoteId(new UUID(0xdd7L, 2L))
  private val profile: MaterialId = materialId(1)
  private val screws: MaterialId = materialId(2)

  private val materials: List[MaterialRecord] =
    List(profile -> UnitOfMeasure.LinearMeters, screws -> UnitOfMeasure.Pieces).map((id, unit) =>
      MaterialRecord.created(materialInfo(id, s"P-${id.value.getLeastSignificantBits}", unit), base),
    )

  private def line(materialId: MaterialId, quantity: Long, cost: Long): DeliveryNoteLine =
    DeliveryNoteLine(materialId, mpnCode, Quantity.applyUnsafe(quantity), UnitOfMeasure.Pieces, UnitPrice.applyUnsafe(cost))

  private def info(id: DeliveryNoteId, number: String, lines: List[DeliveryNoteLine]): DeliveryNoteInfo =
    DeliveryNoteInfo(id, number.refineUnsafe[DeliveryNoteNumber], day(5), firstSupplier, None, lines)

  private def registered(info: DeliveryNoteInfo): DeliveryNoteAggregate = DeliveryNoteAggregate.Registered(DeliveryNoteRecord(info, day(5), None))

  /** Applies `steps` like the material backend, failing the test on a rejection. */
  private def applied(materials: List[MaterialRecord], steps: List[(MaterialId, SetPurchases)]): List[MaterialRecord] =
    steps.foldLeft(materials) { case (current, (materialId, command)) =>
      current.map { material =>
        if material.info.id != materialId then material
        else
          MaterialAggregate.Active(material).setDeliveryNotePurchases(command.ref, command.lines, today) match
            case Decision.Accepted(_, MaterialAggregate.Active(updated)) => updated
            case Decision.InDecisive(_) => material
            case other => fail(s"Unexpected decision: $other")
      }
    }

  private def plan(note: DeliveryNoteAggregate, current: List[MaterialRecord]): List[(MaterialId, SetPurchases)] =
    DeliveryNoteReconciliation.desired(note).fold(Nil)(DeliveryNoteReconciliation.plan(_, DeliveryNoteReconciliation.applied(noteId, current)))

  "DeliveryNoteReconciliation.desired" should "group the lines by material, in the order of the note, on the day of the note" in:
    val note = info(noteId, "7", List(line(profile, 1000L, 10000L), line(screws, 2000L, 500L), line(profile, 3000L, 12000L)))
    DeliveryNoteReconciliation.desired(registered(note)) shouldEqual Some(
      DesiredDeliveryNote(
        note.ref,
        Map(
          profile -> List(
            DeliveryNotePurchaseLine(mpnCode, firstSupplier, Quantity.applyUnsafe(1000L), UnitPrice.applyUnsafe(10000L), day(5)),
            DeliveryNotePurchaseLine(mpnCode, firstSupplier, Quantity.applyUnsafe(3000L), UnitPrice.applyUnsafe(12000L), day(5)),
          ),
          screws -> List(DeliveryNotePurchaseLine(mpnCode, firstSupplier, Quantity.applyUnsafe(2000L), UnitPrice.applyUnsafe(500L), day(5))),
        ),
      ),
    )
    DeliveryNoteReconciliation.desired(DeliveryNoteAggregate.Cancelled(DeliveryNoteRecord(note, day(5), None), day(6), "Doppio")) shouldEqual
      Some(DesiredDeliveryNote(note.ref, Map.empty))
    DeliveryNoteReconciliation.desired(DeliveryNoteAggregate.Empty) shouldEqual None

  "DeliveryNoteReconciliation.plan" should "set every material of a new note, then nothing" in:
    val note = registered(info(noteId, "7", List(line(profile, 1000L, 10000L), line(screws, 2000L, 500L))))
    val steps = plan(note, materials)
    steps.map(_._1) shouldEqual List(profile, screws)
    plan(note, applied(materials, steps)) shouldEqual Nil

  it should "clear a material removed from the note and every material of a cancelled note" in:
    val first = info(noteId, "7", List(line(profile, 1000L, 10000L), line(screws, 2000L, 500L)))
    val stocked = applied(materials, plan(registered(first), materials))
    val changed = registered(first.copy(lines = List(line(profile, 1000L, 10000L))))
    plan(changed, stocked) shouldEqual List(screws -> MaterialService.Command.SetDeliveryNotePurchases(first.ref, Nil))
    val cancelled = DeliveryNoteAggregate.Cancelled(DeliveryNoteRecord(first, day(5), None), day(6), "Doppio")
    plan(cancelled, stocked).map((id, command) => (id, command.lines)) shouldEqual List(profile -> Nil, screws -> Nil)
    applied(stocked, plan(cancelled, stocked)).map(_.ledger.balanceAt(today).value) shouldEqual List(0L, 0L)

  it should "set the purchases again when only the number changes, and ignore the other notes" in:
    val first = info(noteId, "7", List(line(profile, 1000L, 10000L)))
    val stocked = applied(materials, plan(registered(first), materials))
    plan(registered(first.copy(number = "7/A")), stocked).map(_._1) shouldEqual List(profile)
    DeliveryNoteReconciliation.applied(otherNoteId, stocked) shouldEqual Map.empty

  it should "report the materials still pending" in:
    val note = info(noteId, "7", List(line(profile, 1000L, 10000L), line(screws, 2000L, 500L)))
    val desired = DeliveryNoteReconciliation.desired(registered(note)).getOrElse(fail("No desired note"))
    val partly = applied(materials, plan(registered(note), materials).take(1))
    DeliveryNoteReconciliation.pending(desired, DeliveryNoteReconciliation.applied(noteId, partly)) shouldEqual Set(screws)

  it should "converge in one pass from any earlier version of the note" in:
    val genLines = Gen
      .listOf(Gen.zip(Gen.oneOf(profile, screws), Gen.chooseNum(1L, 9L), Gen.chooseNum(0L, 90000L)))
      .map(_.map { case (id, units, cost) =>
        line(id, units * 1000L, cost)
      })
    forAll(genLines, genLines) { (before, after) =>
      val stocked = applied(materials, plan(registered(info(noteId, "7", before)), materials))
      val now = registered(info(noteId, "7", after))
      plan(now, applied(stocked, plan(now, stocked))) shouldEqual Nil
    }
end DeliveryNoteReconciliationTest
