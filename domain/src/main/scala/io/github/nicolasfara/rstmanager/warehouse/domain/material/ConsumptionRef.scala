package io.github.nicolasfara.rstmanager.warehouse.domain.material

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId

/**
 * The order manufacturing an automatic consumption belongs to. A material holds at most one consumption per reference, set to an absolute quantity by
 * [[io.github.nicolasfara.rstmanager.warehouse.domain.material.events.MaterialEvent.ConsumptionSet]] and reconciled from the order by
 * [[io.github.nicolasfara.rstmanager.warehouse.domain.ConsumptionReconciliation]].
 */
final case class ConsumptionRef(orderId: OrderId, manufacturingId: ScheduledManufacturingId) derives CanEqual
