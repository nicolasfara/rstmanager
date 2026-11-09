package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the search fields of the list pages. */
final class ListSearchSpec extends AnyFunSuite with Matchers:

  private val customer = CustomerResponse(
    id = UUID.fromString("00000000-0000-0000-0000-000000000001").nn,
    name = "Mario",
    surname = "Rossi",
    email = "mario@example.com",
    phone = "333 1234567",
    street = "Via Roma 1",
    city = "Rimini",
    postalCode = "47921",
    country = "IT",
    fiscalCode = "RSSMRA80A01H294X",
    customerType = "company",
    businessName = Some("Nautica Rossi"),
    pec = None,
    notes = None,
    boatModel = Some("Azimut 50"),
    boatName = Some("Libeccio"),
    boatBerth = None,
    port = Some("Cattolica"),
  )

  private def manufacturing(code: String, description: Option[String]) =
    ManufacturingResponse(
      id = UUID.fromString("00000000-0000-0000-0000-000000000002").nn,
      code = code,
      completionDate = "2026-11-20T00:00:00Z",
      status = "pending",
      startedAt = None,
      endedAt = None,
      pauseReason = None,
      tasks = Nil,
      dependencies = Nil,
      description = description,
    )

  private val order = OrderResponse(
    id = UUID.fromString("00000000-0000-0000-0000-000000000003").nn,
    number = "ORD-2026-042",
    customerId = customer.id,
    status = "in_progress",
    priority = "normal",
    creationDate = "2026-11-01T00:00:00Z",
    deliveryDate = "2026-12-01T00:00:00Z",
    promisedDeliveryDate = None,
    manufacturings = List(manufacturing("TEND-01", Some("Tendalino prua"))),
    description = Some("Rifacimento capottina"),
    dependencies = Nil,
  )

  test("a customer is found by name, business name, contacts, boat and port"):
    val fields = ListSearch.customer(customer)
    List("rossi mario", "nautica", "333", "libeccio azimut", "cattolica", "rimini").foreach { query =>
      SearchTerms.matchesAny(query, fields) shouldBe true
    }
    SearchTerms.matchesAny("bianchi", fields) shouldBe false

  test("an order is found by number, customer name, description and manufacturings"):
    val fields = ListSearch.order(order, Some("Nautica Rossi"))
    List("ord-2026-042", "nautica", "capottina", "tend-01", "tendalino prua").foreach { query =>
      SearchTerms.matchesAny(query, fields) shouldBe true
    }

  test("an order whose customer is not loaded yet is still found by its own fields"):
    val fields = ListSearch.order(order, None)
    SearchTerms.matchesAny("042", fields) shouldBe true
    SearchTerms.matchesAny("nautica", fields) shouldBe false
end ListSearchSpec
