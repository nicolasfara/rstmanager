package io.github.nicolasfara.rstmanager.service.codecs

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.*
import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent
import io.github.nicolasfara.rstmanager.service.codecs.CustomerCodecs.given

import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class CustomerCodecsTest extends AnyFlatSpecLike:
  private def customer(): Customer =
    Customer(
      id = UUID.fromString("00000000-0000-0000-0000-000000000801").nn,
      contactInfo = ContactInfo(
        name = "Giulia".refineUnsafe[Name],
        surname = "Bianchi".refineUnsafe[Surname],
        email = "giulia.bianchi@example.com".refineUnsafe[Email],
        phone = "+393331234567".refineUnsafe[PhoneNumber],
      ),
      address = Address(
        street = "Via Roma 10".refineUnsafe[Street],
        city = "Bologna".refineUnsafe[City],
        postalCode = "40121".refineUnsafe[PostalCode],
        country = "IT".refineUnsafe[Country],
      ),
      fiscalCode = "RSSMRA85T10A562S".refineUnsafe[TaxCodeRule],
      customerType = CustomerType.Individual,
      pec = Some[String :| Email]("giulia.bianchi@pec.example.com".refineUnsafe[Email]),
      notes = Some("Cliente storico"),
      boat = BoatInfo(Some("Sun Odyssey 410"), Some("Aurora"), Some("B12"), Some("Marina di Rimini")),
    )

  "Customer codec" should "round-trip the extended master-data fields" in:
    val data = customer()
    val decoded = data.asJson.as[Customer].fold(err => fail(s"Decoding failed: $err"), identity)
    decoded shouldEqual data

  it should "decode customers persisted before the master-data extension" in:
    val legacyJson = customer().asJson.mapObject(_.remove("businessName").remove("pec").remove("notes").remove("boat"))
    val decoded = legacyJson.as[Customer].fold(err => fail(s"Decoding failed: $err"), identity)

    decoded.businessName shouldEqual None
    decoded.pec shouldEqual None
    decoded.notes shouldEqual None
    decoded.boat shouldEqual BoatInfo.empty

  "CustomerEvent codec" should "round-trip a creation event with a VAT-number company" in:
    val company = Customer
      .createCustomer(
        UUID.fromString("00000000-0000-0000-0000-000000000802").nn,
        "Marco",
        "Rossi",
        "marco.rossi@example.com",
        "+393339876543",
        "Via del Porto 1",
        "Rimini",
        "47921",
        "IT",
        "12345678903",
        CustomerType.Company,
        businessName = Some("Cantiere Navale Srl"),
      )
      .fold(errors => fail(s"Invalid company customer: $errors"), identity)

    val event: CustomerEvent = CustomerEvent.CustomerCreated(company)
    val decoded = event.asJson.as[CustomerEvent].fold(err => fail(s"Decoding failed: $err"), identity)
    decoded shouldEqual event
end CustomerCodecsTest
