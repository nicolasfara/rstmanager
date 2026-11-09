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
      ContactInfo.createContactInfo(name, surname, Some(email), Some(phone)).isValid shouldEqual true

  it should "accept a contact without email and phone" in:
    forAll(genNonEmptyString, genNonEmptyString): (name, surname) =>
      ContactInfo.createContactInfo(name, surname, None, Some(" ")).map(c => (c.email, c.phone)).toOption shouldEqual Some((None, None))

  it should "preserve all field values when creation succeeds" in:
    forAll(genNonEmptyString, genNonEmptyString, genEmail, genPhoneNumber): (name, surname, email, phone) =>
      ContactInfo
        .createContactInfo(name, surname, Some(email), Some(phone))
        .foreach: contactInfo =>
          contactInfo.name.toString shouldEqual name
          contactInfo.surname.toString shouldEqual surname
          contactInfo.email.map(_.toString) shouldEqual Some(email)
          contactInfo.phone.map(_.toString) shouldEqual Some(phone)

  it should "fail when the email is invalid" in:
    forAll(genNonEmptyString, genNonEmptyString, genPhoneNumber): (name, surname, phone) =>
      ContactInfo.createContactInfo(name, surname, Some("invalid-email"), Some(phone)).isValid shouldEqual false

  it should "fail when the phone number is invalid" in:
    forAll(genNonEmptyString, genNonEmptyString, genEmail): (name, surname, email) =>
      ContactInfo.createContactInfo(name, surname, Some(email), Some("12345")).isValid shouldEqual false

  "updateEmail" should "replace only the email" in:
    val contactInfo = ContactInfo(
      name = "John".refineUnsafe[Name],
      surname = "Doe".refineUnsafe[Surname],
      email = Some("john@doe.com".refineUnsafe[Email]),
      phone = Some("+12345678901".refineUnsafe[PhoneNumber]),
    )

    val updated = contactInfo.updateEmail(Some("jane@doe.com".refineUnsafe[Email]))

    updated.email.map(_.toString) shouldEqual Some("jane@doe.com")
    updated.name shouldEqual contactInfo.name
    updated.surname shouldEqual contactInfo.surname
    updated.phone shouldEqual contactInfo.phone
end ContactInfoTest
