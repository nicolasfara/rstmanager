package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.Validated.Invalid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class AddressTest extends AnyFlatSpec:
  "A Street built from an empty value" must "return a validation error" in:
    val result = Street("")
    result shouldBe a [Invalid[String]]
    result.swap.getOrElse("") should include ("empty")
  "A Street built from a valid value" must "return a valid Street" in:
    val result = Street("123 Main St")
    result.isValid shouldBe true
  "A City built from an empty value" must "return a validation error" in:
    val result = City("")
    result shouldBe a [Invalid[String]]
    result.swap.getOrElse("") should include ("empty")
  "A City built from a valid value" must "return a valid City" in:
    val result = City("Springfield")
    result.isValid shouldBe true
  "A PostalCode built from an invalid value" must "return a validation error" in:
    val result = PostalCode("ABCDE")
    result shouldBe a [Invalid[String]]
    result.swap.getOrElse("") should include ("match")
  "A PostalCode built from a valid value" must "return a valid PostalCode" in:
      val result = PostalCode("12345")
      result.isValid shouldBe true
  "A Country built from an empty value" must "return a validation error" in:
    val result = Country("")
    result shouldBe a [Invalid[String]]
    result.swap.getOrElse("") should include ("empty")
  "A Country built from a valid value" must "return a valid Country" in:
    val result = Country("USA")
    result.isValid shouldBe true
  "Two Address instances with the same values" must "be equal" in:
    val address1 = Address(
      street = Street("123 Main St").getOrElse(throw new Exception("Invalid Street")),
      city = City("Springfield").getOrElse(throw new Exception("Invalid City")),
      cap = PostalCode("12345").getOrElse(throw new Exception("Invalid PostalCode")),
      nation = Country("USA").getOrElse(throw new Exception("Invalid Country"))
    )
    val address2 = Address(
      street = Street("123 Main St").getOrElse(throw new Exception("Invalid Street")),
      city = City("Springfield").getOrElse(throw new Exception("Invalid City")),
      cap = PostalCode("12345").getOrElse(throw new Exception("Invalid PostalCode")),
      nation = Country("USA").getOrElse(throw new Exception("Invalid Country"))
    )
    address1 shouldEqual address2