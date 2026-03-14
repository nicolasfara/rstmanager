package io.github.nicolasfara.rstmanager.customer.domain

import java.util.UUID

import io.github.iltotore.iron.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CustomerTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  private final case class ValidCustomerInput(
      id: UUID,
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      postalCode: String,
      country: String,
      fiscalCode: String,
  )

  private val genUUID: Gen[UUID] = Gen.delay(UUID.randomUUID().nn)
  private val genNonEmptyString: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)
  private val genEmail: Gen[String] =
    for
      user <- genNonEmptyString
      domain <- genNonEmptyString
    yield s"$user@$domain.com"
  private val genPhoneNumber: Gen[String] =
    for
      prefix <- Gen.oneOf("", "+")
      digits <- Gen.listOfN(10, Gen.numChar).map(_.mkString)
    yield s"$prefix$digits"
  private val genPostalCode: Gen[String] = Gen.listOfN(5, Gen.numChar).map(_.mkString)
  private val genFiscalCode: Gen[String] =
    for
      letters1 <- Gen.listOfN(6, Gen.choose('A', 'Z')).map(_.mkString)
      digits1 <- Gen.listOfN(2, Gen.numChar).map(_.mkString)
      letter2 <- Gen.choose('A', 'Z').map(_.toString)
      digits2 <- Gen.listOfN(2, Gen.numChar).map(_.mkString)
      letter3 <- Gen.choose('A', 'Z').map(_.toString)
      digits3 <- Gen.listOfN(3, Gen.numChar).map(_.mkString)
      letter4 <- Gen.choose('A', 'Z').map(_.toString)
    yield s"$letters1$digits1$letter2$digits2$letter3$digits3$letter4"
  private val genValidCustomerInput: Gen[ValidCustomerInput] =
    for
      id <- genUUID
      name <- genNonEmptyString
      surname <- genNonEmptyString
      email <- genEmail
      phone <- genPhoneNumber
      street <- genNonEmptyString
      city <- genNonEmptyString
      postalCode <- genPostalCode
      country <- genNonEmptyString
      fiscalCode <- genFiscalCode
    yield ValidCustomerInput(id, name, surname, email, phone, street, city, postalCode, country, fiscalCode)

  "FiscalCode.createFiscalCode" should "normalize lowercase input before validating it" in:
    FiscalCode
      .createFiscalCode("rssmra85t10a562s")
      .foreach(_.toString shouldEqual "RSSMRA85T10A562S")

  "Customer.createCustomer" should "succeed for valid customer data" in:
    forAll(genValidCustomerInput): input =>
      Customer
        .createCustomer(
          input.id,
          input.name,
          input.surname,
          input.email,
          input.phone,
          input.street,
          input.city,
          input.postalCode,
          input.country,
          input.fiscalCode,
          CustomerType.Individual,
        )
        .isValid shouldEqual true

  it should "preserve all nested values when creation succeeds" in:
    forAll(genValidCustomerInput): input =>
      Customer
        .createCustomer(
          input.id,
          input.name,
          input.surname,
          input.email,
          input.phone,
          input.street,
          input.city,
          input.postalCode,
          input.country,
          input.fiscalCode,
          CustomerType.Company,
        )
        .foreach: customer =>
          customer.id shouldEqual input.id
          customer.contactInfo.name.toString shouldEqual input.name
          customer.contactInfo.surname.toString shouldEqual input.surname
          customer.contactInfo.email.toString shouldEqual input.email
          customer.contactInfo.phone.toString shouldEqual input.phone
          customer.address.street.toString shouldEqual input.street
          customer.address.city.toString shouldEqual input.city
          customer.address.postalCode.toString shouldEqual input.postalCode
          customer.address.country.toString shouldEqual input.country
          customer.fiscalCode.toString shouldEqual input.fiscalCode
          customer.customerType shouldEqual CustomerType.Company

  it should "accumulate errors coming from nested validation" in:
    forAll(genUUID): id =>
      val result = Customer.createCustomer(
        id = id,
        name = "",
        surname = "Doe",
        email = "invalid-email",
        phone = "+12345678901",
        street = "MainStreet",
        city = "Rome",
        postalCode = "ABCDE",
        country = "Italy",
        fiscalCode = "invalid",
        customerType = CustomerType.Individual,
      )

      result.isValid shouldEqual false
      result.swap.foreach(_.length shouldEqual 4L)

  "updateEmail" should "replace only the nested email field" in:
    val customer = Customer(
      id = UUID.randomUUID().nn,
      contactInfo = ContactInfo(
        name = "John".refineUnsafe[Name],
        surname = "Doe".refineUnsafe[Surname],
        email = "john@doe.com".refineUnsafe[Email],
        phone = "+12345678901".refineUnsafe[PhoneNumber],
      ),
      address = Address(
        street = "123MainStreet".refineUnsafe[Street],
        city = "Springfield".refineUnsafe[City],
        postalCode = "12345".refineUnsafe[PostalCode],
        country = "USA".refineUnsafe[Country],
      ),
      fiscalCode = "RSSMRA85T10A562S".refineUnsafe[FiscalCodeRule],
      customerType = CustomerType.Individual,
    )

    val updated = customer.updateEmail("jane@doe.com".refineUnsafe[Email])

    updated.contactInfo.email.toString shouldEqual "jane@doe.com"
    updated.contactInfo.name shouldEqual customer.contactInfo.name
    updated.contactInfo.surname shouldEqual customer.contactInfo.surname
    updated.contactInfo.phone shouldEqual customer.contactInfo.phone
    updated.address shouldEqual customer.address
    updated.fiscalCode shouldEqual customer.fiscalCode
    updated.customerType shouldEqual customer.customerType
end CustomerTest
