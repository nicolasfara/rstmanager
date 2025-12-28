package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.Validated.Invalid
import io.github.iltotore.iron.*
import org.scalatest.flatspec.{AnyFlatSpec, AnyFlatSpecLike}
import org.scalatest.matchers.should.Matchers.*

class AddressTest extends AnyFlatSpecLike:
  "A Street built from an empty value" must "return a validation error" in:
    val result = Street("")
    result shouldBe a[Invalid[String]]
    result.swap.getOrElse("") should include("empty")
  "A Street built from a valid value" must "return a valid Street" in:
    val result = Street("123 Main St")
    result.isValid shouldBe true
  "A Street" should "be compared" in:
    val street1: Street = Street("123 Main St")
    val street2: Street = Street("123 Main St")
    street1 shouldEqual street2
  "A City built from an empty value" must "return a validation error" in:
    val result = City("")
    result shouldBe a[Invalid[String]]
    result.swap.getOrElse("") should include("empty")
  "A City built from a valid value" must "return a valid City" in:
    val result = City("Springfield")
    result.isValid shouldBe true
  "A PostalCode built from an invalid value" must "return a validation error" in:
    val result = PostalCode("ABCDE")
    result shouldBe a[Invalid[String]]
    result.swap.getOrElse("") should include("match")
  "A PostalCode built from a valid value" must "return a valid PostalCode" in:
    val result = PostalCode("12345")
    result.isValid shouldBe true
  "A Country built from an empty value" must "return a validation error" in:
    val result = Country("")
    result shouldBe a[Invalid[String]]
    result.swap.getOrElse("") should include("empty")
  "A Country built from a valid value" must "return a valid Country" in:
    val result = Country("USA")
    result.isValid shouldBe true
  "Two Address instances with the same values" must "be equal" in:
    val address1 = Address(
      street = Street("123 Main St"),
      city = City("Springfield"),
      cap = PostalCode("12345"),
      nation = Country("USA")
    )
    val address2 = Address(
      street = Street("123 Main St"),
      city = City("Springfield"),
      cap = PostalCode("12345"),
      nation = Country("USA")
    )
    address1 shouldEqual address2
