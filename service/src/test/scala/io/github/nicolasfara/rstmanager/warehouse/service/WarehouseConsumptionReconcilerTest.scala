package io.github.nicolasfara.rstmanager.warehouse.service

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.planning.service.PlanningServerConfig
import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.product.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing.{ CompletedManufacturing, InProgressManufacturing }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderData, OrderId, OrderNumber, OrderPriority }
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask.CompletedTask
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.data.{ NonEmptyList, ValidatedNec }
import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class WarehouseConsumptionReconcilerTest extends AnyFlatSpecLike:
  private val now: Instant = Instant.parse("2026-10-30T10:00:00Z").nn
  private val today: LocalDate = LocalDate.parse("2026-10-30")
  private val completedAt: DateTime = DateTime.parse("2026-10-22T09:00:00Z")
  private val orderId: OrderId = OrderId(UUID.fromString("00000000-0000-0000-0000-000000000801").nn)
  private val manufacturingId: ScheduledManufacturingId = ScheduledManufacturingId(UUID.fromString("00000000-0000-0000-0000-000000000802").nn)
  private val ref: ConsumptionRef = ConsumptionRef(orderId, manufacturingId)
  private val profile: ProductId = ProductId(UUID.fromString("00000000-0000-0000-0000-000000000901").nn)
  private val glass: ProductId = ProductId(UUID.fromString("00000000-0000-0000-0000-000000000902").nn)
  private val missing: ProductId = ProductId(UUID.fromString("00000000-0000-0000-0000-000000000903").nn)

  private def valid[A](value: ValidatedNec[String, A]): A = value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def product(id: ProductId, code: String): ProductRecord =
    ProductRecord.created(
      valid(ProductInfo.createProductInfo(id, code, s"Prodotto $code", None, None, UnitOfMeasure.LinearMeters, UnitPrice.applyUnsafe(10000L), Nil)),
      LocalDate.parse("2026-01-01"),
    )

  private def material(productId: ProductId, quantity: Long): ScheduledMaterial =
    ScheduledMaterial(productId, Quantity.applyUnsafe(quantity), UnitPrice.applyUnsafe(10000L))

  private def order(materials: List[ScheduledMaterial], done: Boolean): Order =
    val taskId = ScheduledTaskId(UUID.fromString("00000000-0000-0000-0000-000000000803").nn)
    val info = ScheduledManufacturingInfo(
      manufacturingId,
      "TND-01".refineUnsafe[ManufacturingCode],
      completedAt,
      NonEmptyList.one(
        CompletedTask(taskId, TaskId(UUID.fromString("00000000-0000-0000-0000-000000000804").nn), TaskDuration(8), TaskDuration(8), completedAt),
      ),
      ManufacturingDependencies(),
      materials = materials,
    )
    val manufacturing = if done then CompletedManufacturing(info, completedAt, completedAt) else InProgressManufacturing(info, completedAt)
    val data = OrderData(
      orderId,
      "2026-014".refineUnsafe[OrderNumber],
      CustomerId(UUID.fromString("00000000-0000-0000-0000-000000000805").nn),
      completedAt,
      completedAt.plusDays(10).nn,
      OrderPriority.Normal,
      NonEmptyList.one(manufacturing),
    )
    Order.InProgressOrder(data, completedAt.plusDays(10).nn)
  end order

  /** A gateway over in-memory products that applies commands like the product backend, failing on `broken`. */
  private final class FakeGateway(
      orders: Ref[IO, Map[OrderId, Order]],
      products: Ref[IO, Map[ProductId, ProductRecord]],
      sent: Ref[IO, List[String]],
      broken: Set[ProductId],
  ) extends WarehouseConsumptionGateway:
    override def orderIds: IO[List[OrderId]] = orders.get.map(_.keys.toList)

    override def loadOrder(id: OrderId): IO[Option[Order]] = orders.get.map(_.get(id))

    override def listProducts: IO[List[ProductRecord]] = products.get.map(_.values.toList.sortBy(_.info.id.value.toString))

    override def setConsumption(
        productId: ProductId,
        command: ProductService.Command.SetConsumption,
        commandId: String,
    ): IO[Either[ProductError, Unit]] =
      if broken.contains(productId) then IO.raiseError(new RuntimeException(s"database down for $productId"))
      else
        sent.update(_ :+ commandId) >> products.modify { current =>
          current.get(productId) match
            case None => (current, ProductError.ProductNotFound.asLeft)
            case Some(record) =>
              ProductAggregate.Active(record).setConsumption(command.ref, command.quantity, command.on, today).toEither match
                case Right(ProductAggregate.Active(updated)) => (current.updated(productId, updated), ().asRight)
                case Right(_) => (current, ProductError.ProductNotFound.asLeft)
                case Left(errors) => (current, errors.head.asLeft)
        }
  end FakeGateway

  private final case class Fixture(
      reconciler: WarehouseConsumptionReconciler,
      orders: Ref[IO, Map[OrderId, Order]],
      products: Ref[IO, Map[ProductId, ProductRecord]],
      sent: Ref[IO, List[String]],
  ):
    def consumed(productId: ProductId): Long = products.get.unsafeRunSync().get(productId).fold(-1L)(_.ledger.consumedBy(ref).value)

  private def fixture(initial: Order, broken: Set[ProductId]): Fixture =
    (
      for
        orders <- Ref.of[IO, Map[OrderId, Order]](Map(orderId -> initial))
        products <- Ref.of[IO, Map[ProductId, ProductRecord]](Map(profile -> product(profile, "PRF-1"), glass -> product(glass, "VET-1")))
        sent <- Ref.of[IO, List[String]](Nil)
        reconciler <- WarehouseConsumptionReconciler.make(FakeGateway(orders, products, sent, broken), IO.pure(now))
      yield Fixture(reconciler, orders, products, sent)
    ).unsafeRunSync()

  private val materials = List(material(profile, 2500L), material(glass, 2000L), material(missing, 1000L))

  "WarehouseConsumptionReconciler" should "consume the materials of a completed manufacturing, skipping unknown products" in:
    val f = fixture(order(materials, done = true), Set.empty)
    f.reconciler.reconcileOrder(orderId).unsafeRunSync() shouldEqual ReconciliationSummary(2, 1, 0)
    f.consumed(profile) shouldEqual 2500L
    f.consumed(glass) shouldEqual 2000L
    f.products.get.unsafeRunSync().get(profile).flatMap(_.ledger.movements.lastOption).map(_.on) shouldEqual Some(LocalDate.parse("2026-10-22"))

  it should "do nothing the second time, and reverse the consumption today once the manufacturing is reopened" in:
    val f = fixture(order(materials, done = true), Set.empty)
    f.reconciler.reconcileOrder(orderId).unsafeRunSync()
    f.reconciler.reconcileOrder(orderId).unsafeRunSync() shouldEqual ReconciliationSummary(0, 1, 0)
    f.orders.set(Map(orderId -> order(materials, done = false))).unsafeRunSync()
    f.reconciler.reconcileAll.unsafeRunSync() shouldEqual ReconciliationSummary(2, 0, 0)
    f.consumed(profile) shouldEqual 0L
    f.products.get
      .unsafeRunSync()
      .get(profile)
      .flatMap(_.ledger.movements.lastOption)
      .map(movement => (movement.on, movement.delta.value: Long)) shouldEqual
      Some((today, 2500L))

  it should "go on with the other products when one fails, and catch up on the next run" in:
    val f = fixture(order(materials, done = true), Set(profile))
    f.reconciler.reconcileOrder(orderId).unsafeRunSync() shouldEqual ReconciliationSummary(1, 1, 1)
    f.consumed(profile) shouldEqual 0L
    f.consumed(glass) shouldEqual 2000L

  it should "keep the consumptions of an order that cannot be loaded" in:
    val f = fixture(order(materials, done = true), Set.empty)
    f.reconciler.reconcileOrder(orderId).unsafeRunSync()
    f.orders.set(Map.empty).unsafeRunSync()
    f.reconciler.reconcileOrder(orderId).unsafeRunSync() shouldEqual ReconciliationSummary.empty
    f.consumed(profile) shouldEqual 2500L

  it should "send each step under a deterministic id that changes once the ledger grew" in:
    val f = fixture(order(List(material(profile, 2500L)), done = true), Set.empty)
    f.reconciler.reconcileOrder(orderId).unsafeRunSync()
    f.orders.set(Map(orderId -> order(List(material(profile, 2500L)), done = false))).unsafeRunSync()
    f.reconciler.reconcileOrder(orderId).unsafeRunSync()
    f.orders.set(Map(orderId -> order(List(material(profile, 2500L)), done = true))).unsafeRunSync()
    f.reconciler.reconcileOrder(orderId).unsafeRunSync()
    f.sent.get.unsafeRunSync() shouldEqual List(
      s"warehouse-consumption-$orderId-$profile-$manufacturingId-0-2.500-2026-10-22",
      s"warehouse-consumption-$orderId-$profile-$manufacturingId-1-0.000-2026-10-30",
      s"warehouse-consumption-$orderId-$profile-$manufacturingId-2-2.500-2026-10-22",
    )
    f.consumed(profile) shouldEqual 2500L

  "PlanningServerConfig" should "read the hour of the daily consumption reconciliation" in:
    PlanningServerConfig.fromEnv(Map.empty).map(_.dailyConsumptionReconcileHour) shouldEqual Right(WarehouseDailyReconciler.defaultReconcileHour)
    PlanningServerConfig.fromEnv(Map("RSTMANAGER_WAREHOUSE_DAILY_RECONCILE_HOUR" -> "3")).map(_.dailyConsumptionReconcileHour) shouldEqual Right(3)
    PlanningServerConfig.fromEnv(Map("RSTMANAGER_WAREHOUSE_DAILY_RECONCILE_HOUR" -> "24")).isLeft shouldEqual true
end WarehouseConsumptionReconcilerTest
