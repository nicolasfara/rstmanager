package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class ContactInfoTest extends AnyFlatSpec:
  "An Email built from an invalid value" must "return a validation error" in:
    val result = Email("invalid-email")
    result.isValid shouldBe false
    result.swap.getOrElse("") should include("match")
  "An Email built from a valid value" must "return a valid Email" in:
    val result = Email("foo@bar.com")
    result.isValid shouldBe true
  "A PhoneNumber built from an invalid value" must "return a validation error" in:
    val result = PhoneNumber("12345")
    result.isValid shouldBe false
    result.swap.getOrElse("") should include("match")
  "A PhoneNumber built from a valid value" must "return a valid PhoneNumber" in:
    val result = PhoneNumber("+12345678901")
    result.isValid shouldBe true
  "Two ContactInfo instances with the same values" must "be equal" in:
    val contactInfo1 = ContactInfo(
      name = Name("John"),
      surname = Surname("Doe"),
      email = Email("foo@bar.com"),
      phone = PhoneNumber("+12345678901")
    )
    val contactInfo2 = ContactInfo(
      name = Name("John"),
      surname = Surname("Doe"),
      email = Email("foo@bar.com"),
      phone = PhoneNumber("+12345678901")
    )
    contactInfo1 shouldEqual contactInfo2
