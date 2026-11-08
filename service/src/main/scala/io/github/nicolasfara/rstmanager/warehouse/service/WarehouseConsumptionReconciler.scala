package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant

import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.warehouse.domain.{ BusinessCalendar, ConsumptionReconciliation }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialError, MaterialId, MaterialRecord, MaterialService }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.Monoid
import cats.effect.IO
import cats.effect.std.Mutex
import cats.syntax.all.*
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Internal port the consumption reconciliation reads orders and materials through, and sets consumptions with (in process, no HTTP loopback). */
trait WarehouseConsumptionGateway:
  /** Every order id in the order registry (cancelled orders included). */
  def orderIds: IO[List[OrderId]]

  /** The order, if it exists. */
  def loadOrder(orderId: OrderId): IO[Option[Order]]

  /** Every material, archived ones included. */
  def listMaterials: IO[List[MaterialRecord]]

  /** Sends `command` to material `materialId` under `commandId`. */
  def setConsumption(materialId: MaterialId, command: MaterialService.Command.SetConsumption, commandId: String): IO[Either[MaterialError, Unit]]
end WarehouseConsumptionGateway

object WarehouseConsumptionGateway:
  /** The gateway over the order and material backends of this process. */
  def fromStores(orders: OrderApp.Store, materials: MaterialApp.Store): WarehouseConsumptionGateway =
    new WarehouseConsumptionGateway:
      override def orderIds: IO[List[OrderId]] = RegistryBackend.ids(orders.registry).map(_.toList.sortBy(_.toString).map(OrderId(_)))

      override def loadOrder(orderId: OrderId): IO[Option[Order]] = OrderApp.get(orders, orderId)

      override def listMaterials: IO[List[MaterialRecord]] = MaterialApp.listAll(materials).map(_.map(_.material))

      override def setConsumption(
          materialId: MaterialId,
          command: MaterialService.Command.SetConsumption,
          commandId: String,
      ): IO[Either[MaterialError, Unit]] =
        MaterialApp.setConsumption(materials, materialId, command, commandId)
end WarehouseConsumptionGateway

/**
 * What a reconciliation did.
 *
 * @param applied
 *   Consumptions set (or reversed) on a material.
 * @param unknownMaterials
 *   Consumptions skipped because their material does not exist in the warehouse.
 * @param failed
 *   Steps (or whole orders) that failed: rejected by the material, or an unexpected error. They are retried by the next reconciliation.
 */
final case class ReconciliationSummary(applied: Int, unknownMaterials: Int, failed: Int) derives CanEqual

object ReconciliationSummary:
  val empty: ReconciliationSummary = ReconciliationSummary(0, 0, 0)
  val oneApplied: ReconciliationSummary = ReconciliationSummary(1, 0, 0)
  val oneUnknownMaterial: ReconciliationSummary = ReconciliationSummary(0, 1, 0)
  val oneFailed: ReconciliationSummary = ReconciliationSummary(0, 0, 1)

  given Monoid[ReconciliationSummary] = Monoid.instance(
    empty,
    (left, right) => ReconciliationSummary(left.applied + right.applied, left.unknownMaterials + right.unknownMaterials, left.failed + right.failed),
  )

/**
 * Keeps the automatic consumptions of the warehouse in line with the orders: the materials of every completed order manufacturing are consumed from
 * the day it was completed, and the consumption is reversed when the manufacturing is reopened or removed (see the pure
 * [[ConsumptionReconciliation]]).
 *
 * It runs after every order notification ([[io.github.nicolasfara.rstmanager.planning.service.PlanningDependencyConsumer]]), once at startup and once
 * a day ([[WarehouseDailyReconciler]]). Reconciliations run one at a time (a mutex), so two of them never plan from the same stale materials. They
 * never fail: an unknown material is skipped with a warning, a rejected or failing step is logged and left to the next reconciliation, and the other
 * steps go on.
 *
 * Each step is sent under a deterministic command id (order, material, manufacturing, ledger size, quantity, day), so a step sent twice for the same
 * ledger is handled once by the material backend, while a later step that sets the same quantity again (completed, reopened, and completed again the
 * same day) gets a new id because the ledger has grown.
 */
final class WarehouseConsumptionReconciler private (gateway: WarehouseConsumptionGateway, now: IO[Instant], mutex: Mutex[IO]):
  import WarehouseConsumptionReconciler.*

  /** Reconciles the consumptions of `orderId`; an order that cannot be loaded keeps its consumptions. */
  def reconcileOrder(orderId: OrderId): IO[ReconciliationSummary] =
    mutex.lock.surround(gateway.listMaterials.flatMap(reconcileWith(orderId, _))).handleErrorWith(failure(s"order $orderId"))

  /** Reconciles every order of the registry against one read of the materials. */
  def reconcileAll: IO[ReconciliationSummary] =
    mutex.lock
      .surround(
        for
          ids <- gateway.orderIds
          materials <- gateway.listMaterials
          summaries <- ids.traverse(id => reconcileWith(id, materials).handleErrorWith(failure(s"order $id")))
          summary = summaries.combineAll
          _ <- logger.info(
            s"Warehouse consumption reconciliation of ${ids.size} orders: ${summary.applied} applied, ${summary.unknownMaterials} unknown materials, " +
              s"${summary.failed} failed.",
          )
        yield summary,
      )
      .handleErrorWith(failure("every order"))

  private def reconcileWith(orderId: OrderId, materials: List[MaterialRecord]): IO[ReconciliationSummary] =
    gateway.loadOrder(orderId).flatMap {
      case None => logger.warn(s"Order $orderId not found: its warehouse consumptions are kept as they are.").as(ReconciliationSummary.empty)
      case Some(order) =>
        now.flatMap { instant =>
          val byId = materials.map(material => material.info.id -> material).toMap
          val steps = ConsumptionReconciliation.plan(
            ConsumptionReconciliation.desired(orderId, order),
            ConsumptionReconciliation.applied(orderId, materials),
            BusinessCalendar.day(instant),
          )
          steps.foldMapM { (materialId, command) =>
            byId.get(materialId) match
              case None =>
                logger
                  .warn(
                    s"Order $orderId manufacturing ${command.ref.manufacturingId} uses material $materialId, which is not in the warehouse: skipped.",
                  )
                  .as(ReconciliationSummary.oneUnknownMaterial)
              case Some(material) => send(material, command)
          }
        }
    }

  private def send(material: MaterialRecord, command: MaterialService.Command.SetConsumption): IO[ReconciliationSummary] =
    val materialId = material.info.id
    val id = commandId(materialId, command, material.ledger.movements.size)
    val step = s"consumption of material $materialId by order ${command.ref.orderId} manufacturing ${command.ref.manufacturingId}"
    gateway.setConsumption(materialId, command, id).attempt.flatMap {
      case Right(Right(())) =>
        logger.info(s"Set $step to ${command.quantity.toDecimalString} from ${command.on}.").as(ReconciliationSummary.oneApplied)
      case Right(Left(error)) => logger.warn(s"Could not set $step: rejected with $error.").as(ReconciliationSummary.oneFailed)
      case Left(error) => logger.error(error)(s"Could not set $step.").as(ReconciliationSummary.oneFailed)
    }
end WarehouseConsumptionReconciler

object WarehouseConsumptionReconciler:
  private val logger = Slf4jLogger.getLogger[IO]

  /** A reconciler over `gateway`, taking the business day of a reversal from `now`. */
  def make(gateway: WarehouseConsumptionGateway, now: IO[Instant]): IO[WarehouseConsumptionReconciler] =
    Mutex[IO].map(new WarehouseConsumptionReconciler(gateway, now, _))

  /** The reconciler over the order and material backends of this process, on the real clock. */
  def live(orders: OrderApp.Store, materials: MaterialApp.Store): IO[WarehouseConsumptionReconciler] =
    make(WarehouseConsumptionGateway.fromStores(orders, materials), IO.realTimeInstant)

  /** The command id of one step: the same step on the same ledger always gets the same id. */
  private[service] def commandId(materialId: MaterialId, command: MaterialService.Command.SetConsumption, ledgerSize: Int): String =
    s"warehouse-consumption-${command.ref.orderId}-$materialId-${command.ref.manufacturingId}-$ledgerSize-${command.quantity.toDecimalString}-${command.on}"

  private def failure(scope: String)(error: Throwable): IO[ReconciliationSummary] =
    logger
      .error(error)(s"Warehouse consumption reconciliation of $scope failed; the next reconciliation retries it.")
      .as(ReconciliationSummary.oneFailed)
end WarehouseConsumptionReconciler
