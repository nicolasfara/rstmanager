package io.github.nicolasfara.rstmanager.planning.service

import java.nio.charset.StandardCharsets
import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeService
import io.github.nicolasfara.rstmanager.hr.service.EmployeeApp
import io.github.nicolasfara.rstmanager.planning.PlanningTrigger
import io.github.nicolasfara.rstmanager.work.domain.order.OrderService
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.{ OutboxConsumer, OutboxItem }

/** Bridges edomata outbox notifications from planning dependencies into planning recalculation commands. */
object PlanningDependencyConsumer:
  def resource(
      orders: OrderApp.Store,
      employees: EmployeeApp.Store,
      recalculator: PlanningRecalculator,
  ): Resource[IO, Unit] =
    val orderStream = OutboxConsumer(orders.entity)(item => handleOrderNotification(recalculator, item))
    val employeeStream = OutboxConsumer(employees.entity)(item => handleEmployeeNotification(recalculator, item))

    orderStream.merge(employeeStream).compile.drain.background.void

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

  private def handleEmployeeNotification(recalculator: PlanningRecalculator, item: OutboxItem[EmployeeService.Notification]): IO[Unit] =
    recalculator.recalculate(employeeCause(item)).void

  private def outboxCause[N](namespace: String, item: OutboxItem[N], trigger: PlanningTrigger): PlanningRecalculationCause =
    val stableId = s"$namespace-${item.streamId}-${item.seqNr}"
    val requestId = UUID.nameUUIDFromBytes(stableId.getBytes(StandardCharsets.UTF_8)).nn
    PlanningRecalculationCause(trigger, Some(s"planning-recalc-$stableId"), Some(requestId))
end PlanningDependencyConsumer
