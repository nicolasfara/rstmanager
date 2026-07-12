package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.Employee
import io.github.nicolasfara.rstmanager.hr.service.EmployeeApp
import io.github.nicolasfara.rstmanager.work.domain.order.Order
import io.github.nicolasfara.rstmanager.work.domain.order.Order.InProgressOrder
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.effect.IO
import cats.syntax.all.*

/** Internal application port used by planning to read fresh entity snapshots without HTTP loopback calls. */
trait PlanningEntityGateway:
  import PlanningEntityGateway.*

  def loadOpenOrders(selection: Option[List[UUID]]): IO[Either[LoadError, List[InProgressOrder]]]

  def loadEmployees(selection: Option[List[UUID]]): IO[Either[LoadError, List[Employee]]]

  def snapshot(orderIds: Option[List[UUID]], employeeIds: Option[List[UUID]]): IO[Either[LoadError, Snapshot]] =
    loadOpenOrders(orderIds).flatMap {
      case Left(error) => IO.pure(error.asLeft)
      case Right(orders) =>
        loadEmployees(employeeIds).map(_.map(employees => Snapshot(orders, employees)))
    }
end PlanningEntityGateway

object PlanningEntityGateway:
  final case class Snapshot(orders: List[InProgressOrder], employees: List[Employee])

  enum LoadError derives CanEqual:
    case UnknownOrders(ids: List[UUID])
    case UnknownEmployees(ids: List[UUID])

  def fromStores(orders: OrderApp.Store, employees: EmployeeApp.Store): PlanningEntityGateway =
    new PlanningEntityGateway:
      override def loadOpenOrders(selection: Option[List[UUID]]): IO[Either[LoadError, List[InProgressOrder]]] =
        selection match
          case None => OrderApp.list(orders).map(_.collect { case open: InProgressOrder => open }.asRight)
          case Some(ids) => loadSelectedOpenOrders(orders, ids)

      override def loadEmployees(selection: Option[List[UUID]]): IO[Either[LoadError, List[Employee]]] =
        selection match
          case None => EmployeeApp.list(employees).map(_.asRight)
          case Some(ids) => loadSelectedEmployees(employees, ids)

  private def loadSelectedOpenOrders(store: OrderApp.Store, ids: List[UUID]): IO[Either[LoadError, List[InProgressOrder]]] =
    ids.distinct.traverse(id => OrderApp.get(store, id).map(id -> _)).map { loaded =>
      val invalid = loaded.collect { case (id, order) if !order.exists(isOpen) => id }
      if invalid.nonEmpty then LoadError.UnknownOrders(invalid).asLeft
      else loaded.flatMap { case (_, order) => order.collect { case open: InProgressOrder => open } }.asRight
    }

  private def loadSelectedEmployees(store: EmployeeApp.Store, ids: List[UUID]): IO[Either[LoadError, List[Employee]]] =
    ids.distinct.traverse(id => EmployeeApp.get(store, id).map(id -> _)).map { loaded =>
      val missing = loaded.collect { case (id, None) => id }
      if missing.nonEmpty then LoadError.UnknownEmployees(missing).asLeft
      else loaded.collect { case (_, Some(employee)) => employee }.asRight
    }

  private def isOpen(order: Order): Boolean = order match
    case _: InProgressOrder => true
    case _ => false
end PlanningEntityGateway
