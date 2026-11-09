package io.github.nicolasfara.rstmanager.customer.domain

import java.util.UUID

import io.github.iltotore.iron.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CustomerTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  private final case class ValidCustomerInput(
      id: CustomerId,
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
  private val genVatNumber: Gen[String] =
    Gen.listOfN(10, Gen.numChar).map(_.mkString).map { digits =>
      val total = digits
        .map(_.asDigit)
        .zipWithIndex
        .map { (digit, index) =>
          if index % 2 == 0 then digit
          else
            val doubled = digit * 2
            if doubled > 9 then doubled - 9 else doubled
        }
        .sum
      s"$digits${(10 - total % 10) % 10}"
    }
  private val genValidCustomerInput: Gen[ValidCustomerInput] =
    for
      id <- genUUID.map(CustomerId(_))
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
          Some(input.email),
          Some(input.phone),
          Some(input.street),
          Some(input.city),
          Some(input.postalCode),
          Some(input.country),
          Some(input.fiscalCode),
          CustomerType.Individual,
          businessName = None,
          pec = None,
          notes = None,
          boatModel = None,
          boatName = None,
          boatBerth = None,
          port = None,
        )
        .isValid shouldEqual true

  it should "preserve all nested values when creation succeeds" in:
    forAll(genValidCustomerInput): input =>
      Customer
        .createCustomer(
          input.id,
          input.name,
          input.surname,
          Some(input.email),
          Some(input.phone),
          Some(input.street),
          Some(input.city),
          Some(input.postalCode),
          Some(input.country),
          Some(input.fiscalCode),
          CustomerType.Individual,
          businessName = Some("Nautica Srl"),
          pec = Some("nautica@pec.example.com"),
          notes = Some("Cliente storico"),
          boatModel = Some("Sun Odyssey 410"),
          boatName = Some("Aurora"),
          boatBerth = Some("B12"),
          port = Some("Marina di Rimini"),
        )
        .foreach: customer =>
          customer.id shouldEqual input.id
          customer.contactInfo.name.toString shouldEqual input.name
          customer.contactInfo.surname.toString shouldEqual input.surname
          customer.contactInfo.email.map(_.toString) shouldEqual Some(input.email)
          customer.contactInfo.phone.map(_.toString) shouldEqual Some(input.phone)
          customer.address.street.map(_.toString) shouldEqual Some(input.street)
          customer.address.city.map(_.toString) shouldEqual Some(input.city)
          customer.address.postalCode.map(_.toString) shouldEqual Some(input.postalCode)
          customer.address.country.map(_.toString) shouldEqual Some(input.country)
          customer.fiscalCode.map(_.toString) shouldEqual Some(input.fiscalCode)
          customer.customerType shouldEqual CustomerType.Individual
          customer.businessName.map(_.toString) shouldEqual Some("Nautica Srl")
          customer.pec.map(_.toString) shouldEqual Some("nautica@pec.example.com")
          customer.notes shouldEqual Some("Cliente storico")
          customer.boat shouldEqual BoatInfo(Some("Sun Odyssey 410"), Some("Aurora"), Some("B12"), Some("Marina di Rimini"))

  it should "validate the VAT number of companies" in:
    forAll(genValidCustomerInput, genVatNumber): (input, vatNumber) =>
      val result = Customer.createCustomer(
        input.id,
        input.name,
        input.surname,
        Some(input.email),
        Some(input.phone),
        Some(input.street),
        Some(input.city),
        Some(input.postalCode),
        Some(input.country),
        Some(vatNumber),
        CustomerType.Company,
        businessName = Some("Cantiere Navale Srl"),
        pec = None,
        notes = None,
        boatModel = None,
        boatName = None,
        boatBerth = None,
        port = None,
      )
      result.isValid shouldEqual true
      result.foreach: customer =>
        customer.fiscalCode.map(_.toString) shouldEqual Some(vatNumber)
        customer.businessName.map(_.toString) shouldEqual Some("Cantiere Navale Srl")

  it should "accept a customer with only name and surname, treating blank values as absent" in:
    forAll(genValidCustomerInput, Gen.oneOf(CustomerType.Individual, CustomerType.Company)): (input, customerType) =>
      val result = Customer.createCustomer(
        input.id,
        input.name,
        input.surname,
        email = None,
        phone = Some(""),
        street = None,
        city = Some("  "),
        postalCode = None,
        country = None,
        fiscalCode = Some(" "),
        customerType = customerType,
        businessName = None,
        pec = None,
        notes = None,
        boatModel = None,
        boatName = None,
        boatBerth = None,
        port = None,
      )
      result.toOption.map(c => (c.contactInfo.email, c.contactInfo.phone, c.address, c.fiscalCode, c.businessName)) shouldEqual
        Some((None, None, Address.empty, None, None))
      result.foreach(_.checkInvariants.isValid shouldEqual true)

  it should "reject a company VAT number with an invalid check digit" in:
    forAll(genValidCustomerInput, genVatNumber): (input, vatNumber) =>
      val corrupted = vatNumber.init + ((vatNumber.last.asDigit + 1) % 10).toString
      Customer
        .createCustomer(
          input.id,
          input.name,
          input.surname,
          Some(input.email),
          Some(input.phone),
          Some(input.street),
          Some(input.city),
          Some(input.postalCode),
          Some(input.country),
          Some(corrupted),
          CustomerType.Company,
          businessName = Some("Cantiere Navale Srl"),
          pec = None,
          notes = None,
          boatModel = None,
          boatName = None,
          boatBerth = None,
          port = None,
        )
        .isValid shouldEqual false

  it should "reject an invalid PEC address" in:
    forAll(genValidCustomerInput): input =>
      Customer
        .createCustomer(
          input.id,
          input.name,
          input.surname,
          Some(input.email),
          Some(input.phone),
          Some(input.street),
          Some(input.city),
          Some(input.postalCode),
          Some(input.country),
          Some(input.fiscalCode),
          CustomerType.Individual,
          businessName = None,
          pec = Some("not-a-pec"),
          notes = None,
          boatModel = None,
          boatName = None,
          boatBerth = None,
          port = None,
        )
        .isValid shouldEqual false

  "VatNumber.createVatNumber" should "accept VAT numbers with a valid check digit" in:
    forAll(genVatNumber): vatNumber =>
      VatNumber.createVatNumber(vatNumber).isValid shouldEqual true

  it should "accumulate errors coming from nested validation" in:
    forAll(genUUID.map(CustomerId(_))): id =>
      val result = Customer.createCustomer(
        id = id,
        name = "",
        surname = "Doe",
        email = Some("invalid-email"),
        phone = Some("+12345678901"),
        street = Some("MainStreet"),
        city = Some("Rome"),
        postalCode = Some("ABCDE"),
        country = Some("Italy"),
        fiscalCode = Some("invalid"),
        customerType = CustomerType.Individual,
        businessName = None,
        pec = None,
        notes = None,
        boatModel = None,
        boatName = None,
        boatBerth = None,
        port = None,
      )

      result.isValid shouldEqual false
      result.swap.foreach(_.length shouldEqual 4L)

  "updateEmail" should "replace only the nested email field" in:
    val customer = Customer(
      id = CustomerId(UUID.randomUUID().nn),
      contactInfo = ContactInfo(
        name = "John".refineUnsafe[Name],
        surname = "Doe".refineUnsafe[Surname],
        email = Some("john@doe.com".refineUnsafe[Email]),
        phone = Some("+12345678901".refineUnsafe[PhoneNumber]),
      ),
      address = Address(
        street = Some("123MainStreet".refineUnsafe[Street]),
        city = Some("Springfield".refineUnsafe[City]),
        postalCode = Some("12345".refineUnsafe[PostalCode]),
        country = Some("USA".refineUnsafe[Country]),
      ),
      fiscalCode = Some("RSSMRA85T10A562S".refineUnsafe[TaxCodeRule]),
      customerType = CustomerType.Individual,
    )

    val updated = customer.updateEmail(Some("jane@doe.com".refineUnsafe[Email]))

    updated.contactInfo.email.map(_.toString) shouldEqual Some("jane@doe.com")
    updated.contactInfo.name shouldEqual customer.contactInfo.name
    updated.contactInfo.surname shouldEqual customer.contactInfo.surname
    updated.contactInfo.phone shouldEqual customer.contactInfo.phone
    updated.address shouldEqual customer.address
    updated.fiscalCode shouldEqual customer.fiscalCode
    updated.customerType shouldEqual customer.customerType
end CustomerTest
