package io.github.nicolasfara.rstmanager.customer.domain

import io.github.iltotore.iron.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ContactInfoTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:

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

  "ContactInfo.createContactInfo" should "succeed for valid contact details" in:
    forAll(genNonEmptyString, genNonEmptyString, genEmail, genPhoneNumber): (name, surname, email, phone) =>
      ContactInfo.createContactInfo(name, surname, email, phone).isValid shouldEqual true

  it should "preserve all field values when creation succeeds" in:
    forAll(genNonEmptyString, genNonEmptyString, genEmail, genPhoneNumber): (name, surname, email, phone) =>
      ContactInfo
        .createContactInfo(name, surname, email, phone)
        .foreach: contactInfo =>
          contactInfo.name.toString shouldEqual name
          contactInfo.surname.toString shouldEqual surname
          contactInfo.email.toString shouldEqual email
          contactInfo.phone.toString shouldEqual phone

  it should "fail when the email is invalid" in:
    forAll(genNonEmptyString, genNonEmptyString, genPhoneNumber): (name, surname, phone) =>
      ContactInfo.createContactInfo(name, surname, "invalid-email", phone).isValid shouldEqual false

  it should "fail when the phone number is invalid" in:
    forAll(genNonEmptyString, genNonEmptyString, genEmail): (name, surname, email) =>
      ContactInfo.createContactInfo(name, surname, email, "12345").isValid shouldEqual false

  "updateEmail" should "replace only the email" in:
    val contactInfo = ContactInfo(
      name = "John".refineUnsafe[Name],
      surname = "Doe".refineUnsafe[Surname],
      email = "john@doe.com".refineUnsafe[Email],
      phone = "+12345678901".refineUnsafe[PhoneNumber],
    )

    val updated = contactInfo.updateEmail("jane@doe.com".refineUnsafe[Email])

    updated.email.toString shouldEqual "jane@doe.com"
    updated.name shouldEqual contactInfo.name
    updated.surname shouldEqual contactInfo.surname
    updated.phone shouldEqual contactInfo.phone
end ContactInfoTest
