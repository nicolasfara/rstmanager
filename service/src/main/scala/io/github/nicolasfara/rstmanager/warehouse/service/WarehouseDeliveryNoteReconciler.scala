package io.github.nicolasfara.rstmanager.warehouse.service

import java.nio.charset.StandardCharsets
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.DeliveryNoteReconciliation
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.{ DeliveryNoteAggregate, DeliveryNoteId }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialError, MaterialId, MaterialRecord, MaterialService }

import cats.effect.IO
import cats.effect.std.Mutex
import cats.syntax.all.*
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Internal port the delivery note reconciliation reads notes and materials through, and sets purchases with (in process, no HTTP loopback). */
trait WarehouseDeliveryNoteGateway:
  /** Every delivery note id in the registry (cancelled notes included). */
  def deliveryNoteIds: IO[List[DeliveryNoteId]]

  /** The note, registered or cancelled, if it exists. */
  def loadDeliveryNote(id: DeliveryNoteId): IO[Option[DeliveryNoteAggregate]]

  /** Every material, archived ones included. */
  def listMaterials: IO[List[MaterialRecord]]

  /** Sends `command` to material `materialId` under `commandId`. */
  def setDeliveryNotePurchases(
      materialId: MaterialId,
      command: MaterialService.Command.SetDeliveryNotePurchases,
      commandId: String,
  ): IO[Either[MaterialError, Unit]]
end WarehouseDeliveryNoteGateway

object WarehouseDeliveryNoteGateway:
  /** The gateway over the delivery note and material backends of this process. */
  def fromStores(notes: DeliveryNoteApp.Store, materials: MaterialApp.Store): WarehouseDeliveryNoteGateway =
    new WarehouseDeliveryNoteGateway:
      override def deliveryNoteIds: IO[List[DeliveryNoteId]] = DeliveryNoteApp.ids(notes)

      override def loadDeliveryNote(id: DeliveryNoteId): IO[Option[DeliveryNoteAggregate]] = DeliveryNoteApp.find(notes, id)

      override def listMaterials: IO[List[MaterialRecord]] = MaterialApp.listAll(materials).map(_.map(_.material))

      override def setDeliveryNotePurchases(
          materialId: MaterialId,
          command: MaterialService.Command.SetDeliveryNotePurchases,
          commandId: String,
      ): IO[Either[MaterialError, Unit]] =
        MaterialApp.setDeliveryNotePurchases(materials, materialId, command, commandId)
end WarehouseDeliveryNoteGateway

/**
 * Keeps the purchases of the materials in line with the delivery notes: every line of a registered note is a purchase of its material on the day of
 * the note; a changed note voids and records again the purchases of the materials it changes, and a cancelled one clears them (see the pure
 * [[DeliveryNoteReconciliation]]).
 *
 * It runs right after every command on a note (in the HTTP request, so the answer already shows the stock), once at startup and once a day
 * ([[WarehouseDailyReconciler]]) to heal what a failed step left behind. Reconciliations run one at a time (a mutex), so two of them never plan from
 * the same stale materials. They never fail: an unknown material is skipped with a warning, a rejected or failing step is logged and left to the next
 * reconciliation, and the other steps go on.
 *
 * Each step is sent under a deterministic command id (note, material, ledger size, digest of the purchases), so a step sent twice for the same ledger
 * is handled once by the material backend, while a later step that sets the same purchases again gets a new id because the ledger has grown.
 */
final class WarehouseDeliveryNoteReconciler private (gateway: WarehouseDeliveryNoteGateway, mutex: Mutex[IO]):
  import WarehouseDeliveryNoteReconciler.*

  /** Reconciles the purchases of note `id`; a note that cannot be loaded keeps its purchases. */
  def reconcile(id: DeliveryNoteId): IO[ReconciliationSummary] =
    mutex.lock.surround(gateway.listMaterials.flatMap(reconcileWith(id, _))).handleErrorWith(failure(s"delivery note $id"))

  /** Reconciles every note of the registry against one read of the materials. */
  def reconcileAll: IO[ReconciliationSummary] =
    mutex.lock
      .surround(
        for
          ids <- gateway.deliveryNoteIds
          materials <- gateway.listMaterials
          summaries <- ids.traverse(id => reconcileWith(id, materials).handleErrorWith(failure(s"delivery note $id")))
          summary = summaries.combineAll
          _ <- logger.info(
            s"Warehouse delivery note reconciliation of ${ids.size} notes: ${summary.applied} applied, ${summary.unknownMaterials} unknown materials, " +
              s"${summary.failed} failed.",
          )
        yield summary,
      )
      .handleErrorWith(failure("every delivery note"))

  private def reconcileWith(id: DeliveryNoteId, materials: List[MaterialRecord]): IO[ReconciliationSummary] =
    gateway.loadDeliveryNote(id).map(_.flatMap(DeliveryNoteReconciliation.desired)).flatMap {
      case None => logger.warn(s"Delivery note $id not found: its warehouse purchases are kept as they are.").as(ReconciliationSummary.empty)
      case Some(desired) =>
        val byId = materials.map(material => material.info.id -> material).toMap
        DeliveryNoteReconciliation.plan(desired, DeliveryNoteReconciliation.applied(id, materials)).foldMapM { (materialId, command) =>
          byId.get(materialId) match
            case None =>
              logger
                .warn(s"Delivery note $id names material $materialId, which is not in the warehouse: skipped.")
                .as(ReconciliationSummary.oneUnknownMaterial)
            case Some(material) => send(material, command)
        }
    }

  private def send(material: MaterialRecord, command: MaterialService.Command.SetDeliveryNotePurchases): IO[ReconciliationSummary] =
    val materialId = material.info.id
    val id = commandId(materialId, command, material.ledger.movements.size)
    val step = s"purchases of material $materialId from delivery note ${command.ref.id} (n. ${command.ref.number})"
    gateway.setDeliveryNotePurchases(materialId, command, id).attempt.flatMap {
      case Right(Right(())) => logger.info(s"Set $step to ${command.lines.size} lines.").as(ReconciliationSummary.oneApplied)
      case Right(Left(error)) => logger.warn(s"Could not set $step: rejected with $error.").as(ReconciliationSummary.oneFailed)
      case Left(error) => logger.error(error)(s"Could not set $step.").as(ReconciliationSummary.oneFailed)
    }
end WarehouseDeliveryNoteReconciler

object WarehouseDeliveryNoteReconciler:
  private val logger = Slf4jLogger.getLogger[IO]

  /** A reconciler over `gateway`. */
  def make(gateway: WarehouseDeliveryNoteGateway): IO[WarehouseDeliveryNoteReconciler] =
    Mutex[IO].map(new WarehouseDeliveryNoteReconciler(gateway, _))

  /** The reconciler over the delivery note and material backends of this process. */
  def live(notes: DeliveryNoteApp.Store, materials: MaterialApp.Store): IO[WarehouseDeliveryNoteReconciler] =
    make(WarehouseDeliveryNoteGateway.fromStores(notes, materials))

  /**
   * The command id of one step: the same step on the same ledger always gets the same id. The purchases are summed up by a name-based UUID of their
   * number and lines, stable across releases and machines.
   */
  private[service] def commandId(materialId: MaterialId, command: MaterialService.Command.SetDeliveryNotePurchases, ledgerSize: Int): String =
    val content = (command.ref.number: String) :: command.lines.map(line =>
      s"${line.mpnCode}|${line.supplierId}|${line.quantity.toDecimalString}|${line.unitCost.toDecimalString}|${line.on}",
    )
    val digest = UUID.nameUUIDFromBytes(content.mkString("\n").getBytes(StandardCharsets.UTF_8)).nn
    s"warehouse-delivery-note-${command.ref.id}-$materialId-$ledgerSize-$digest"

  private def failure(scope: String)(error: Throwable): IO[ReconciliationSummary] =
    logger
      .error(error)(s"Warehouse delivery note reconciliation of $scope failed; the next reconciliation retries it.")
      .as(ReconciliationSummary.oneFailed)
end WarehouseDeliveryNoteReconciler
