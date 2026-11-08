package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.*
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId

import cats.data.ValidatedNec
import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class WarehouseDeliveryNoteReconcilerTest extends AnyFlatSpecLike:
  private val today: LocalDate = LocalDate.parse("2026-11-05")
  private val noteDay: LocalDate = LocalDate.parse("2026-11-02")
  private val supplierId: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-000000000301").nn)
  private val noteId: DeliveryNoteId = DeliveryNoteId(UUID.fromString("00000000-0000-0000-0000-000000000b01").nn)
  private val profile: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000901").nn)
  private val glass: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000902").nn)
  private val missing: MaterialId = MaterialId(UUID.fromString("00000000-0000-0000-0000-000000000903").nn)
  private val code: String :| MpnCode = "AC-100"

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def material(id: MaterialId, internalCode: String): MaterialRecord =
    MaterialRecord.created(
      valid(
        MaterialInfo.createMaterialInfo(
          id,
          internalCode,
          s"Materiale $internalCode",
          None,
          None,
          UnitOfMeasure.LinearMeters,
          UnitPrice.applyUnsafe(10000L),
          List(valid(Mpn.createMpn(code, supplierId, None, None))),
        ),
      ),
      LocalDate.parse("2026-01-01"),
    )

  private def line(materialId: MaterialId, quantity: Long): DeliveryNoteLine =
    DeliveryNoteLine(materialId, code, Quantity.applyUnsafe(quantity), UnitOfMeasure.LinearMeters, UnitPrice.applyUnsafe(30000L))

  private def note(lines: List[DeliveryNoteLine]): DeliveryNoteInfo = DeliveryNoteInfo(noteId, "1234", noteDay, supplierId, None, lines)

  private def registered(info: DeliveryNoteInfo): DeliveryNoteAggregate = DeliveryNoteAggregate.Registered(DeliveryNoteRecord(info, noteDay, None))

  /** A gateway over in-memory notes and materials that applies commands like the material backend, failing on `broken`. */
  private final class FakeGateway(
      notes: Ref[IO, Map[DeliveryNoteId, DeliveryNoteAggregate]],
      materials: Ref[IO, Map[MaterialId, MaterialRecord]],
      sent: Ref[IO, List[String]],
      broken: Set[MaterialId],
  ) extends WarehouseDeliveryNoteGateway:
    override def deliveryNoteIds: IO[List[DeliveryNoteId]] = notes.get.map(_.keys.toList)

    override def loadDeliveryNote(id: DeliveryNoteId): IO[Option[DeliveryNoteAggregate]] = notes.get.map(_.get(id))

    override def listMaterials: IO[List[MaterialRecord]] = materials.get.map(_.values.toList.sortBy(_.info.id.value.toString))

    override def setDeliveryNotePurchases(
        materialId: MaterialId,
        command: MaterialService.Command.SetDeliveryNotePurchases,
        commandId: String,
    ): IO[Either[MaterialError, Unit]] =
      if broken.contains(materialId) then IO.raiseError(new RuntimeException(s"database down for $materialId"))
      else
        sent.update(_ :+ commandId) >> materials.modify { current =>
          current.get(materialId) match
            case None => (current, MaterialError.MaterialNotFound.asLeft)
            case Some(record) =>
              MaterialAggregate.Active(record).setDeliveryNotePurchases(command.ref, command.lines, today).toEither match
                case Right(MaterialAggregate.Active(updated)) => (current.updated(materialId, updated), ().asRight)
                case Right(_) => (current, MaterialError.MaterialNotFound.asLeft)
                case Left(errors) => (current, errors.head.asLeft)
        }
  end FakeGateway

  private final case class Fixture(
      reconciler: WarehouseDeliveryNoteReconciler,
      notes: Ref[IO, Map[DeliveryNoteId, DeliveryNoteAggregate]],
      materials: Ref[IO, Map[MaterialId, MaterialRecord]],
      sent: Ref[IO, List[String]],
  ):
    def stock(materialId: MaterialId): Long = materials.get.unsafeRunSync().get(materialId).fold(-1L)(_.ledger.balanceAt(today).value)
    def ledger(materialId: MaterialId): StockLedger = materials.get.unsafeRunSync().get(materialId).fold(StockLedger.empty)(_.ledger)

  private def fixture(initial: DeliveryNoteAggregate, broken: Set[MaterialId]): Fixture =
    (
      for
        notes <- Ref.of[IO, Map[DeliveryNoteId, DeliveryNoteAggregate]](Map(noteId -> initial))
        materials <- Ref.of[IO, Map[MaterialId, MaterialRecord]](Map(profile -> material(profile, "PRF-1"), glass -> material(glass, "VET-1")))
        sent <- Ref.of[IO, List[String]](Nil)
        reconciler <- WarehouseDeliveryNoteReconciler.make(FakeGateway(notes, materials, sent, broken))
      yield Fixture(reconciler, notes, materials, sent)
    ).unsafeRunSync()

  private val lines = List(line(profile, 2500L), line(glass, 2000L), line(missing, 1000L), line(profile, 500L))

  "WarehouseDeliveryNoteReconciler" should "record the purchases of a note on its materials, skipping unknown materials" in:
    val f = fixture(registered(note(lines)), Set.empty)
    f.reconciler.reconcile(noteId).unsafeRunSync() shouldEqual ReconciliationSummary(2, 1, 0)
    f.stock(profile) shouldEqual 3000L
    f.stock(glass) shouldEqual 2000L
    f.ledger(profile).active.map(movement => (movement.on, movement.deliveryNote.map(_.id))) shouldEqual
      Vector((noteDay, Some(noteId)), (noteDay, Some(noteId)))

  it should "do nothing the second time, follow a changed note, and clear a cancelled one" in:
    val f = fixture(registered(note(lines)), Set.empty)
    f.reconciler.reconcile(noteId).unsafeRunSync()
    f.reconciler.reconcile(noteId).unsafeRunSync() shouldEqual ReconciliationSummary(0, 1, 0)
    f.notes.set(Map(noteId -> registered(note(List(line(profile, 1000L)))))).unsafeRunSync()
    f.reconciler.reconcileAll.unsafeRunSync() shouldEqual ReconciliationSummary(2, 0, 0)
    f.stock(profile) shouldEqual 1000L
    f.stock(glass) shouldEqual 0L
    f.notes.set(Map(noteId -> DeliveryNoteAggregate.Cancelled(DeliveryNoteRecord(note(Nil), noteDay, None), today, "Doppio"))).unsafeRunSync()
    f.reconciler.reconcileAll.unsafeRunSync() shouldEqual ReconciliationSummary(1, 0, 0)
    f.stock(profile) shouldEqual 0L
    f.ledger(profile).deliveryNotes shouldEqual Map.empty

  it should "go on with the other materials when one fails, and catch up on the next run" in:
    val f = fixture(registered(note(lines)), Set(profile))
    f.reconciler.reconcile(noteId).unsafeRunSync() shouldEqual ReconciliationSummary(1, 1, 1)
    f.stock(profile) shouldEqual 0L
    f.stock(glass) shouldEqual 2000L

  it should "keep the purchases of a note that cannot be loaded" in:
    val f = fixture(registered(note(lines)), Set.empty)
    f.reconciler.reconcile(noteId).unsafeRunSync()
    f.notes.set(Map.empty).unsafeRunSync()
    f.reconciler.reconcile(noteId).unsafeRunSync() shouldEqual ReconciliationSummary.empty
    f.stock(profile) shouldEqual 3000L

  it should "send each step under a deterministic id that changes with the purchases and once the ledger grew" in:
    val first = note(List(line(profile, 2500L)))
    val f = fixture(registered(first), Set.empty)
    f.reconciler.reconcile(noteId).unsafeRunSync()
    f.notes.set(Map(noteId -> registered(note(List(line(profile, 1000L)))))).unsafeRunSync()
    f.reconciler.reconcile(noteId).unsafeRunSync()
    f.notes.set(Map(noteId -> registered(first))).unsafeRunSync()
    f.reconciler.reconcile(noteId).unsafeRunSync()
    val sent = f.sent.get.unsafeRunSync()
    val command: MaterialService.Command.SetDeliveryNotePurchases =
      MaterialService.Command.SetDeliveryNotePurchases(first.ref, DeliveryNoteReconciliation.purchaseLines(first).getOrElse(profile, Nil))
    sent.headOption shouldEqual Some(WarehouseDeliveryNoteReconciler.commandId(profile, command, 0))
    sent.lift(2) shouldEqual Some(WarehouseDeliveryNoteReconciler.commandId(profile, command, 2))
    sent.distinct should have size 3
    sent.foreach(_ should startWith(s"warehouse-delivery-note-$noteId-$profile-"))
    f.stock(profile) shouldEqual 2500L
end WarehouseDeliveryNoteReconcilerTest
