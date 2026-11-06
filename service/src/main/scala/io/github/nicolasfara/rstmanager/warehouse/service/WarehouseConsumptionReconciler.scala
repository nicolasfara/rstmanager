package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant

import io.github.nicolasfara.rstmanager.service.registry.RegistryBackend
import io.github.nicolasfara.rstmanager.warehouse.domain.{ BusinessCalendar, ConsumptionReconciliation }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ ProductError, ProductId, ProductRecord, ProductService }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId }
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.Monoid
import cats.effect.IO
import cats.effect.std.Mutex
import cats.syntax.all.*
import org.typelevel.log4cats.slf4j.Slf4jLogger

/** Internal port the consumption reconciliation reads orders and products through, and sets consumptions with (in process, no HTTP loopback). */
trait WarehouseConsumptionGateway:
  /** Every order id in the order registry (cancelled orders included). */
  def orderIds: IO[List[OrderId]]

  /** The order, if it exists. */
  def loadOrder(orderId: OrderId): IO[Option[Order]]

  /** Every product, archived ones included. */
  def listProducts: IO[List[ProductRecord]]

  /** Sends `command` to product `productId` under `commandId`. */
  def setConsumption(productId: ProductId, command: ProductService.Command.SetConsumption, commandId: String): IO[Either[ProductError, Unit]]
end WarehouseConsumptionGateway

object WarehouseConsumptionGateway:
  /** The gateway over the order and product backends of this process. */
  def fromStores(orders: OrderApp.Store, products: ProductApp.Store): WarehouseConsumptionGateway =
    new WarehouseConsumptionGateway:
      override def orderIds: IO[List[OrderId]] = RegistryBackend.ids(orders.registry).map(_.toList.sortBy(_.toString).map(OrderId(_)))

      override def loadOrder(orderId: OrderId): IO[Option[Order]] = OrderApp.get(orders, orderId)

      override def listProducts: IO[List[ProductRecord]] = ProductApp.listAll(products).map(_.map(_.product))

      override def setConsumption(
          productId: ProductId,
          command: ProductService.Command.SetConsumption,
          commandId: String,
      ): IO[Either[ProductError, Unit]] =
        ProductApp.setConsumption(products, productId, command, commandId)
end WarehouseConsumptionGateway

/**
 * What a reconciliation did.
 *
 * @param applied
 *   Consumptions set (or reversed) on a product.
 * @param unknownProducts
 *   Consumptions skipped because their product does not exist in the warehouse.
 * @param failed
 *   Steps (or whole orders) that failed: rejected by the product, or an unexpected error. They are retried by the next reconciliation.
 */
final case class ReconciliationSummary(applied: Int, unknownProducts: Int, failed: Int) derives CanEqual

object ReconciliationSummary:
  val empty: ReconciliationSummary = ReconciliationSummary(0, 0, 0)
  val oneApplied: ReconciliationSummary = ReconciliationSummary(1, 0, 0)
  val oneUnknownProduct: ReconciliationSummary = ReconciliationSummary(0, 1, 0)
  val oneFailed: ReconciliationSummary = ReconciliationSummary(0, 0, 1)

  given Monoid[ReconciliationSummary] = Monoid.instance(
    empty,
    (left, right) => ReconciliationSummary(left.applied + right.applied, left.unknownProducts + right.unknownProducts, left.failed + right.failed),
  )

/**
 * Keeps the automatic consumptions of the warehouse in line with the orders: the materials of every completed order manufacturing are consumed from
 * the day it was completed, and the consumption is reversed when the manufacturing is reopened or removed (see the pure
 * [[ConsumptionReconciliation]]).
 *
 * It runs after every order notification ([[io.github.nicolasfara.rstmanager.planning.service.PlanningDependencyConsumer]]), once at startup and once
 * a day ([[WarehouseDailyReconciler]]). Reconciliations run one at a time (a mutex), so two of them never plan from the same stale products. They
 * never fail: an unknown product is skipped with a warning, a rejected or failing step is logged and left to the next reconciliation, and the other
 * steps go on.
 *
 * Each step is sent under a deterministic command id (order, product, manufacturing, ledger size, quantity, day), so a step sent twice for the same
 * ledger is handled once by the product backend, while a later step that sets the same quantity again (completed, reopened, and completed again the
 * same day) gets a new id because the ledger has grown.
 */
final class WarehouseConsumptionReconciler private (gateway: WarehouseConsumptionGateway, now: IO[Instant], mutex: Mutex[IO]):
  import WarehouseConsumptionReconciler.*

  /** Reconciles the consumptions of `orderId`; an order that cannot be loaded keeps its consumptions. */
  def reconcileOrder(orderId: OrderId): IO[ReconciliationSummary] =
    mutex.lock.surround(gateway.listProducts.flatMap(reconcileWith(orderId, _))).handleErrorWith(failure(s"order $orderId"))

  /** Reconciles every order of the registry against one read of the products. */
  def reconcileAll: IO[ReconciliationSummary] =
    mutex.lock
      .surround(
        for
          ids <- gateway.orderIds
          products <- gateway.listProducts
          summaries <- ids.traverse(id => reconcileWith(id, products).handleErrorWith(failure(s"order $id")))
          summary = summaries.combineAll
          _ <- logger.info(
            s"Warehouse consumption reconciliation of ${ids.size} orders: ${summary.applied} applied, ${summary.unknownProducts} unknown products, " +
              s"${summary.failed} failed.",
          )
        yield summary,
      )
      .handleErrorWith(failure("every order"))

  private def reconcileWith(orderId: OrderId, products: List[ProductRecord]): IO[ReconciliationSummary] =
    gateway.loadOrder(orderId).flatMap {
      case None => logger.warn(s"Order $orderId not found: its warehouse consumptions are kept as they are.").as(ReconciliationSummary.empty)
      case Some(order) =>
        now.flatMap { instant =>
          val byId = products.map(product => product.info.id -> product).toMap
          val steps = ConsumptionReconciliation.plan(
            ConsumptionReconciliation.desired(orderId, order),
            ConsumptionReconciliation.applied(orderId, products),
            BusinessCalendar.day(instant),
          )
          steps.foldMapM { (productId, command) =>
            byId.get(productId) match
              case None =>
                logger
                  .warn(
                    s"Order $orderId manufacturing ${command.ref.manufacturingId} uses product $productId, which is not in the warehouse: skipped.",
                  )
                  .as(ReconciliationSummary.oneUnknownProduct)
              case Some(product) => send(product, command)
          }
        }
    }

  private def send(product: ProductRecord, command: ProductService.Command.SetConsumption): IO[ReconciliationSummary] =
    val productId = product.info.id
    val id = commandId(productId, command, product.ledger.movements.size)
    val step = s"consumption of product $productId by order ${command.ref.orderId} manufacturing ${command.ref.manufacturingId}"
    gateway.setConsumption(productId, command, id).attempt.flatMap {
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

  /** The reconciler over the order and product backends of this process, on the real clock. */
  def live(orders: OrderApp.Store, products: ProductApp.Store): IO[WarehouseConsumptionReconciler] =
    make(WarehouseConsumptionGateway.fromStores(orders, products), IO.realTimeInstant)

  /** The command id of one step: the same step on the same ledger always gets the same id. */
  private[service] def commandId(productId: ProductId, command: ProductService.Command.SetConsumption, ledgerSize: Int): String =
    s"warehouse-consumption-${command.ref.orderId}-$productId-${command.ref.manufacturingId}-$ledgerSize-${command.quantity.toDecimalString}-${command.on}"

  private def failure(scope: String)(error: Throwable): IO[ReconciliationSummary] =
    logger
      .error(error)(s"Warehouse consumption reconciliation of $scope failed; the next reconciliation retries it.")
      .as(ReconciliationSummary.oneFailed)
end WarehouseConsumptionReconciler
