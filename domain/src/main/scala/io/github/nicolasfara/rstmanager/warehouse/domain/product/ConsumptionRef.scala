package io.github.nicolasfara.rstmanager.warehouse.domain.product

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

/**
 * The order manufacturing an automatic consumption belongs to. A product holds at most one consumption per reference, set to an absolute quantity by
 * [[io.github.nicolasfara.rstmanager.warehouse.domain.product.events.ProductEvent.ConsumptionSet]] and reconciled from the order by
 * [[io.github.nicolasfara.rstmanager.warehouse.domain.ConsumptionReconciliation]].
 */
final case class ConsumptionRef(orderId: OrderId, manufacturingId: ScheduledManufacturingId) derives CanEqual
