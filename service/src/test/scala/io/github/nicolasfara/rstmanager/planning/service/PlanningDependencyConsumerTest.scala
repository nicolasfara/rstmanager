package io.github.nicolasfara.rstmanager.planning.service

import java.time.OffsetDateTime
import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.{ EmployeeId, EmployeeService }
import io.github.nicolasfara.rstmanager.planning.PlanningTrigger
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialError, MaterialId, MaterialRecord, MaterialService }
import io.github.nicolasfara.rstmanager.warehouse.service.{ WarehouseConsumptionGateway, WarehouseConsumptionReconciler }
import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderId, OrderService }

import cats.data.NonEmptyChain
import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import edomata.backend.{ OutboxConsumer, OutboxItem, OutboxReader }
import edomata.core.MessageMetadata
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningDependencyConsumerTest extends AnyFlatSpecLike:
  private val orderId: OrderId = OrderId(UUID.fromString("00000000-0000-0000-0000-000000000021").nn)
  private val employeeId: EmployeeId = EmployeeId(UUID.fromString("00000000-0000-0000-0000-000000000022").nn)
  private val time: OffsetDateTime = OffsetDateTime.parse("2026-06-15T10:15:30Z").nn

  "PlanningDependencyConsumer" should "derive a stable order recalculation cause from an outbox item" in:
    val item = OutboxItem(
      12L,
      orderId.toString,
      time,
      OrderService.Notification.SchedulingRecalculationRequested(orderId),
      MessageMetadata("order-command"),
    )

    val first = PlanningDependencyConsumer.orderCause(item)
    val second = PlanningDependencyConsumer.orderCause(item)

    first shouldEqual second
    first.trigger shouldEqual PlanningTrigger.OrderChanged(orderId)
    first.commandId shouldEqual Some(s"planning-recalc-orders-$orderId-12")
    first.requestId should not be empty

  it should "map employee changes to workforce recalculation" in:
    val item = OutboxItem(
      9L,
      employeeId.toString,
      time,
      EmployeeService.Notification.EmployeeChanged(employeeId),
      MessageMetadata("employee-command"),
    )

    val cause = PlanningDependencyConsumer.employeeCause(item)

    cause.trigger shouldEqual PlanningTrigger.WorkforceCapacityChanged
    cause.commandId shouldEqual Some(s"planning-recalc-employees-$employeeId-9")
    cause.requestId should not be empty

  private def orderItem(seqNr: Long): OutboxItem[OrderService.Notification] =
    OutboxItem(seqNr, orderId.toString, time, OrderService.Notification.SchedulingRecalculationRequested(orderId), MessageMetadata("order-command"))

  /** An outbox holding `items` once, recording what the consumer marks as sent. */
  private final class FakeOutbox(items: List[OutboxItem[OrderService.Notification]], sent: Ref[IO, List[Long]])
      extends OutboxReader[IO, OrderService.Notification]:
    override def read: Stream[IO, OutboxItem[OrderService.Notification]] = Stream.emits(items)
    override def markAllAsSent(marked: NonEmptyChain[OutboxItem[OrderService.Notification]]): IO[Unit] =
      sent.update(_ ++ marked.toChain.toList.map(_.seqNr))

  /** A recalculator recording the triggers it receives, failing on every one when `failing`. */
  private final class RecordingRecalculator(seen: Ref[IO, List[PlanningTrigger]], failing: Boolean) extends PlanningRecalculator:
    override def recalculate(trigger: PlanningTrigger): IO[PlanningRecalculationResult] =
      recalculate(PlanningRecalculationCause.manual(trigger))
    override def recalculate(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult] =
      seen.update(_ :+ cause.trigger) >>
        (if failing then IO.raiseError(new RuntimeException("planning down")) else IO.pure(PlanningRecalculationResult.skipped))

  /** A warehouse gateway without orders or materials, recording the orders the reconciliation loads. */
  private final class RecordingGateway(loaded: Ref[IO, List[OrderId]]) extends WarehouseConsumptionGateway:
    override def orderIds: IO[List[OrderId]] = IO.pure(Nil)
    override def loadOrder(id: OrderId): IO[Option[Order]] = loaded.update(_ :+ id).as(None)
    override def listMaterials: IO[List[MaterialRecord]] = IO.pure(Nil)
    override def setConsumption(
        materialId: MaterialId,
        command: MaterialService.Command.SetConsumption,
        commandId: String,
    ): IO[Either[MaterialError, Unit]] =
      IO.pure(Right(()))

  it should "send an order notification both to the planning recalculation and to the warehouse reconciliation" in:
    List(false, true).foreach { planningFails =>
      val (triggers, loaded, sent) =
        (Ref.of[IO, List[PlanningTrigger]](Nil), Ref.of[IO, List[OrderId]](Nil), Ref.of[IO, List[Long]](Nil)).tupled.unsafeRunSync()
      val reconciler = WarehouseConsumptionReconciler.make(RecordingGateway(loaded), IO.realTimeInstant).unsafeRunSync()
      val handler =
        PlanningDependencyConsumer.fanOut(PlanningDependencyConsumer.orderHandlers(RecordingRecalculator(triggers, planningFails), reconciler))
      OutboxConsumer.from(FakeOutbox(List(orderItem(4L)), sent), Stream.empty)(handler).compile.drain.unsafeRunSync()
      withClue(s"planning fails: $planningFails") {
        triggers.get.unsafeRunSync() shouldEqual List(PlanningTrigger.OrderChanged(orderId))
        loaded.get.unsafeRunSync() shouldEqual List(orderId)
        sent.get.unsafeRunSync() shouldEqual List(4L)
      }
    }

  it should "fan every order notification out to planning and warehouse, a failing reaction stopping neither the other nor the consumer" in:
    val items = List(orderItem(1L), orderItem(2L), orderItem(3L))
    val (planning, warehouse, sent) =
      (Ref.of[IO, List[Long]](Nil), Ref.of[IO, List[Long]](Nil), Ref.of[IO, List[Long]](Nil)).tupled.unsafeRunSync()
    val handlers: List[PlanningDependencyConsumer.Handler[OrderService.Notification]] = List(
      // Planning fails on the second item: the warehouse still sees it, and the third item is still consumed.
      ("planning", item => if item.seqNr == 2L then IO.raiseError(new RuntimeException("planning down")) else planning.update(_ :+ item.seqNr)),
      ("warehouse", item => warehouse.update(_ :+ item.seqNr)),
    )
    OutboxConsumer.from(FakeOutbox(items, sent), Stream.empty)(PlanningDependencyConsumer.fanOut(handlers)).compile.drain.unsafeRunSync()
    planning.get.unsafeRunSync() shouldEqual List(1L, 3L)
    warehouse.get.unsafeRunSync() shouldEqual List(1L, 2L, 3L)
    sent.get.unsafeRunSync() shouldEqual List(1L, 2L, 3L)

  it should "keep running the warehouse reaction when it is the one that fails first" in:
    val (planning, sent) = (Ref.of[IO, List[Long]](Nil), Ref.of[IO, List[Long]](Nil)).tupled.unsafeRunSync()
    val handlers: List[PlanningDependencyConsumer.Handler[OrderService.Notification]] = List(
      ("warehouse", _ => IO.raiseError(new RuntimeException("warehouse down"))),
      ("planning", item => planning.update(_ :+ item.seqNr)),
    )
    OutboxConsumer
      .from(FakeOutbox(List(orderItem(7L)), sent), Stream.empty)(PlanningDependencyConsumer.fanOut(handlers))
      .compile
      .drain
      .unsafeRunSync()
    planning.get.unsafeRunSync() shouldEqual List(7L)
    sent.get.unsafeRunSync() shouldEqual List(7L)
end PlanningDependencyConsumerTest
