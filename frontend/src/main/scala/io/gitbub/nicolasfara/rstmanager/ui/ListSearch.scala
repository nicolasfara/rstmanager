package io.gitbub.nicolasfara.rstmanager.ui

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*

/**
 * Pure (DOM-free) search fields of the list pages: for each listed resource, the texts its search box matches the typed terms against (see
 * [[SearchTerms.matchesAny]]). Names resolved from other resources (the customer of an order, the default employee of a task) are passed in.
 */
object ListSearch:

  def customer(c: CustomerResponse): List[String] =
    List(c.name, c.surname) ++
      List(c.email, c.phone, c.city, c.fiscalCode, c.businessName, c.pec, c.boatModel, c.boatName, c.boatBerth, c.port).flatten

  def employee(e: EmployeeResponse): List[String] = List(e.name, e.surname)

  def task(t: TaskResponse, defaultEmployee: Option[String]): List[String] = t.name :: (t.description.toList ++ defaultEmployee.toList)

  def manufacturing(m: ManufacturingCatalogResponse): List[String] = List(m.code, m.name) ++ m.description.toList ++ m.tasks.map(_.name)

  def supplier(s: SupplierResponse): List[String] = s.name :: List(s.vatNumber, s.email, s.phone, s.address).flatten

  def material(m: MaterialResponse): List[String] =
    List(m.internalCode, m.description) ++ m.materialType.toList ++ m.mpns.map(_.code) ++ m.suppliers.map(_.name)

  def order(o: OrderResponse, customer: Option[String]): List[String] =
    o.number :: (customer.toList ++ o.description.toList ++ o.manufacturings.flatMap(m => m.code :: m.description.toList))
end ListSearch
