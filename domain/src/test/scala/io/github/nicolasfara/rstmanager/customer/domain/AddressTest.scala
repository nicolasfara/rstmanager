package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AddressTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

  private val genNonEmptyString: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)
  private val genPostalCode: Gen[String] = Gen.listOfN(5, Gen.numChar).map(_.mkString)

  "Address.createAddress" should "succeed for non-empty street, city, country, and a 5-digit postal code" in:
    forAll(genNonEmptyString, genNonEmptyString, genPostalCode, genNonEmptyString): (street, city, postalCode, country) =>
      Address.createAddress(Some(street), Some(city), Some(postalCode), Some(country)).isValid shouldEqual true

  it should "preserve all field values when creation succeeds" in:
    forAll(genNonEmptyString, genNonEmptyString, genPostalCode, genNonEmptyString): (street, city, postalCode, country) =>
      Address
        .createAddress(Some(street), Some(city), Some(postalCode), Some(country))
        .foreach: address =>
          address.street.map(_.toString) shouldEqual Some(street)
          address.city.map(_.toString) shouldEqual Some(city)
          address.postalCode.map(_.toString) shouldEqual Some(postalCode)
          address.country.map(_.toString) shouldEqual Some(country)

  it should "treat missing and blank fields as absent" in:
    Address.createAddress(None, Some("  "), None, Some("")).toOption shouldEqual Some(Address.empty)

  it should "fail when the postal code is not made of exactly 5 digits" in:
    forAll(genNonEmptyString, genNonEmptyString, genNonEmptyString): (street, city, country) =>
      Address.createAddress(Some(street), Some(city), Some("ABCDE"), Some(country)).isValid shouldEqual false

  "updatePostalCode" should "replace only the postal code" in:
    val address = Address(
      street = Some("123MainStreet".refineUnsafe[Street]),
      city = Some("Springfield".refineUnsafe[City]),
      postalCode = Some("12345".refineUnsafe[PostalCode]),
      country = Some("USA".refineUnsafe[Country]),
    )

    val updated = address.updatePostalCode(Some("54321".refineUnsafe[PostalCode]))

    updated.postalCode.map(_.toString) shouldEqual Some("54321")
    updated.street shouldEqual address.street
    updated.city shouldEqual address.city
    updated.country shouldEqual address.country
end AddressTest
