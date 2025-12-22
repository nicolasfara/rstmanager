package io.github.nicolasfara.rstmanager.customer.domain

import cats.data.ValidatedNec
import cats.implicits.catsSyntaxTuple9Semigroupal
import cats.syntax.all.catsSyntaxEitherBinCompat0

import java.util.UUID

final case class Customer(
    id: CustomerId,
    contactInfo: ContactInfo,
    address: Address,
    fiscalCode: FiscalCode,
    customerType: CustomerType
)

private sealed trait CustomerValidator:
  type ValidationResult[A] = ValidatedNec[String, A]

  private def validateName(name: String): ValidationResult[Name] = Name(name).toValidatedNec
  private def validateSurname(surname: String): ValidationResult[Surname] = Surname(surname).toValidatedNec
  private def validateEmail(email: String): ValidationResult[Email] = Email(email).toValidatedNec
  private def validatePhone(phone: String): ValidationResult[PhoneNumber] = PhoneNumber(phone).toValidatedNec
  private def validateStreet(street: String): ValidationResult[Street] = Street(street).toValidatedNec
  private def validateCity(city: String): ValidationResult[City] = City(city).toValidatedNec
  private def validatePostalCode(cap: String): ValidationResult[PostalCode] = PostalCode(cap).toValidatedNec
  private def validateCountry(nation: String): ValidationResult[Country] = Country(nation).toValidatedNec
  private def validateFiscalCode(fiscalCode: String): ValidationResult[FiscalCode] = FiscalCode(
    fiscalCode
  ).toValidatedNec

  def validateCustomer(
      id: UUID,
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      cap: String,
      nation: String,
      fiscalCode: String,
      customerType: CustomerType
  ): ValidationResult[Customer] =
    (
      validateName(name),
      validateSurname(surname),
      validateEmail(email),
      validatePhone(phone),
      validateStreet(street),
      validateCity(city),
      validatePostalCode(cap),
      validateCountry(nation),
      validateFiscalCode(fiscalCode)
    ).mapN {
      (
          validName,
          validSurname,
          validEmail,
          validPhone,
          validStreet,
          validCity,
          validCAP,
          validNation,
          validFiscalCode
      ) =>
        Customer(
          id = id,
          contactInfo = ContactInfo(validName, validSurname, validEmail, validPhone),
          address = Address(validStreet, validCity, validCAP, validNation),
          fiscalCode = validFiscalCode,
          customerType = customerType
        )
    }

object CustomerValidator extends CustomerValidator

object Customer:
  def apply(
      id: UUID,
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      cap: String,
      nation: String,
      fiscalCode: String,
      customerType: CustomerType
  ): ValidatedNec[String, Customer] =
    CustomerValidator.validateCustomer(
      id,
      name,
      surname,
      email,
      phone,
      street,
      city,
      cap,
      nation,
      fiscalCode,
      customerType
    )
