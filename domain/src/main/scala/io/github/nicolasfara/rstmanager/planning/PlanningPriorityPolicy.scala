package io.github.nicolasfara.rstmanager.planning

import io.github.nicolasfara.rstmanager.work.domain.order.{ Order, OrderPriority }
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder

/**
 * Priority policy used to order open orders before planning.
 *
 * Only `InProgressOrder` values are schedulable. The policy gives precedence to urgent orders, then earlier planned delivery dates, then earlier
 * creation dates, and finally stable order ids to keep the result deterministic.
 */
object PlanningPriorityPolicy:
  /** Returns only open orders, ordered by priority, delivery date, creation date, then stable id. */
  def sortOpenOrders(orders: List[Order]): List[InProgressOrder] =
    orders.collect { case order: InProgressOrder => order }.sortBy { order =>
      (
        priorityRank(order.data.priority),
        order.plannedDelivery.getMillis,
        order.data.creationDate.getMillis,
        order.data.id.toString,
      )
    }

  private def priorityRank(priority: OrderPriority): Int = priority match
    case OrderPriority.Urgent => 0
    case OrderPriority.Normal => 1
end PlanningPriorityPolicy
