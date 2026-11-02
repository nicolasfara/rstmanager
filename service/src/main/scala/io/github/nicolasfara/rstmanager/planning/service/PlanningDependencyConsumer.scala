package io.github.nicolasfara.rstmanager.planning.service

import java.nio.charset.StandardCharsets
import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeService
import io.github.nicolasfara.rstmanager.hr.service.EmployeeApp
import io.github.nicolasfara.rstmanager.planning.PlanningTrigger
import io.github.nicolasfara.rstmanager.warehouse.service.WarehouseConsumptionReconciler
import io.github.nicolasfara.rstmanager.work.domain.order.OrderService
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.{ OutboxConsumer, OutboxItem }
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Bridges edomata outbox notifications from planning dependencies into planning recalculation commands, and order notifications into the warehouse
 * consumption reconciliation.
 *
 * An edomata `OutboxConsumer` reads the outbox of a backend and then marks every item it read as sent, so a backend can have one consumer only: a
 * second consumer on the orders would steal notifications from the first. The order notifications are therefore fanned out here to every reaction
 * (planning, then warehouse). Each reaction is isolated: a failure is logged and neither stops the other reactions nor ends the consumer fiber (the
 * item is still marked as sent; the daily consolidations recover what a failed reaction missed).
 */
object PlanningDependencyConsumer:
  private val logger = Slf4jLogger.getLogger[IO]

  /** A named reaction to an outbox item. */
  type Handler[N] = (String, OutboxItem[N] => IO[Unit])

  def resource(
      orders: OrderApp.Store,
      employees: EmployeeApp.Store,
      recalculator: PlanningRecalculator,
      warehouse: WarehouseConsumptionReconciler,
  ): Resource[IO, Unit] =
    val orderStream = OutboxConsumer(orders.entity)(fanOut(orderHandlers(recalculator, warehouse)))
    val employeeStream = OutboxConsumer(employees.entity)(fanOut(List(("planning", item => handleEmployeeNotification(recalculator, item)))))

    orderStream.merge(employeeStream).compile.drain.background.void

  /** The reactions to an order notification: the planning recalculation, then the warehouse consumption reconciliation of the order. */
  private[service] def orderHandlers(
      recalculator: PlanningRecalculator,
      warehouse: WarehouseConsumptionReconciler,
  ): List[Handler[OrderService.Notification]] =
    List(
      ("planning", item => handleOrderNotification(recalculator, item)),
      ("warehouse", item => handleWarehouseNotification(warehouse, item)),
    )

  /** Runs every handler on the item, one after the other; a failing handler is logged and the next ones still run. Never fails. */
  private[service] def fanOut[N](handlers: List[Handler[N]]): OutboxItem[N] => IO[Unit] =
    item =>
      handlers.traverse_ { (name, handler) =>
        handler(item).attempt.flatMap {
          case Right(()) => IO.unit
          case Left(error) => logger.error(error)(s"The $name reaction to outbox item ${item.streamId}#${item.seqNr} failed; the consumer goes on.")
        }
      }

  private[service] def orderCause(item: OutboxItem[OrderService.Notification]): PlanningRecalculationCause =
    item.data match
      case OrderService.Notification.SchedulingRecalculationRequested(orderId) =>
        outboxCause("orders", item, PlanningTrigger.OrderChanged(orderId))

  private[service] def employeeCause(item: OutboxItem[EmployeeService.Notification]): PlanningRecalculationCause =
    item.data match
      case EmployeeService.Notification.EmployeeChanged(_) =>
        outboxCause("employees", item, PlanningTrigger.WorkforceCapacityChanged)

  private def handleOrderNotification(recalculator: PlanningRecalculator, item: OutboxItem[OrderService.Notification]): IO[Unit] =
    recalculator.recalculate(orderCause(item)).void

  private def handleWarehouseNotification(warehouse: WarehouseConsumptionReconciler, item: OutboxItem[OrderService.Notification]): IO[Unit] =
    item.data match
      case OrderService.Notification.SchedulingRecalculationRequested(orderId) => warehouse.reconcileOrder(orderId).void

  private def handleEmployeeNotification(recalculator: PlanningRecalculator, item: OutboxItem[EmployeeService.Notification]): IO[Unit] =
    recalculator.recalculate(employeeCause(item)).void

  private def outboxCause[N](namespace: String, item: OutboxItem[N], trigger: PlanningTrigger): PlanningRecalculationCause =
    val stableId = s"$namespace-${item.streamId}-${item.seqNr}"
    val requestId = UUID.nameUUIDFromBytes(stableId.getBytes(StandardCharsets.UTF_8)).nn
    PlanningRecalculationCause(trigger, Some(s"planning-recalc-$stableId"), Some(requestId))
end PlanningDependencyConsumer
