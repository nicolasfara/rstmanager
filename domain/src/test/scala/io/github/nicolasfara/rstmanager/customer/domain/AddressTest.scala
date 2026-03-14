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
      Address.createAddress(street, city, postalCode, country).isValid shouldEqual true

  it should "preserve all field values when creation succeeds" in:
    forAll(genNonEmptyString, genNonEmptyString, genPostalCode, genNonEmptyString): (street, city, postalCode, country) =>
      Address
        .createAddress(street, city, postalCode, country)
        .foreach: address =>
          address.street.toString shouldEqual street
          address.city.toString shouldEqual city
          address.postalCode.toString shouldEqual postalCode
          address.country.toString shouldEqual country

  it should "fail when the street is empty" in:
    forAll(genNonEmptyString, genPostalCode, genNonEmptyString): (city, postalCode, country) =>
      Address.createAddress("", city, postalCode, country).isValid shouldEqual false

  it should "fail when the postal code is not made of exactly 5 digits" in:
    forAll(genNonEmptyString, genNonEmptyString, genNonEmptyString): (street, city, country) =>
      Address.createAddress(street, city, "ABCDE", country).isValid shouldEqual false

  "updatePostalCode" should "replace only the postal code" in:
    val address = Address(
      street = "123MainStreet".refineUnsafe[Street],
      city = "Springfield".refineUnsafe[City],
      postalCode = "12345".refineUnsafe[PostalCode],
      country = "USA".refineUnsafe[Country],
    )

    val updated = address.updatePostalCode("54321".refineUnsafe[PostalCode])

    updated.postalCode.toString shouldEqual "54321"
    updated.street shouldEqual address.street
    updated.city shouldEqual address.city
    updated.country shouldEqual address.country
end AddressTest
