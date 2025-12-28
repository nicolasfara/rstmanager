package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.iltotore.iron.*
import com.github.nscala_time.time.Imports.*
import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{Manufacturing, ManufacturingCode}
import io.github.nicolasfara.rstmanager.work.domain.task.Hours
import org.scalactic.anyvals.NonEmptySet

final case class Order(
    id: OrderId,
    number: OrderNumber,
    customerId: CustomerId,
    creationDate: DateTime,
    deliveryDate: DateTime,
    priority: OrderPriority,
    setOfManufacturing: NonEmptySet[Manufacturing]
):
  def totalHours: Hours = setOfManufacturing.foldLeft[Hours](Hours(0)) { (acc, manufacturing) =>
    acc + manufacturing.totalHours
  }

  def remainingHours: Hours = setOfManufacturing.foldLeft[Hours](Hours(0)) { (acc, manufacturing) =>
    acc + manufacturing.tasks.foldLeft[Hours](Hours(0)) { (taskAcc, task) =>
      taskAcc + task.remainingHours
    }
  }

  def addManufacturing(manufacturing: Manufacturing): Order =
    copy(setOfManufacturing = setOfManufacturing + manufacturing)

  def removeManufacturing(manufacturingCode: ManufacturingCode): Either[OrderError, Order] =
    val updatedSet = setOfManufacturing.filterNot(_.code == manufacturingCode).toList
    updatedSet match {
      case head :: tail => Right(copy(setOfManufacturing = NonEmptySet(head, tail: _*)))
      case Nil          => Left(OrderError.OrderWithNoManufacturing)
    }

  def updateManufacturing(updatedManufacturing: Manufacturing): Order =
    val updatedSet = setOfManufacturing.map { manufacturing =>
      if manufacturing.code == updatedManufacturing.code then updatedManufacturing
      else manufacturing
    }
    copy(setOfManufacturing = updatedSet)
