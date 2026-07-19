package io.github.nicolasfara.rstmanager.customer.domain

import java.util.UUID

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import monocle.syntax.all.*

/** Refined constraint for a non-empty business name. */
type BusinessName = DescribedAs[Not[Empty], "The business name cannot be empty"]

/**
 * Customer aggregate containing identity, contacts, address, and fiscal classification.
 *
 * @param id
 *   Stable customer identifier.
 * @param contactInfo
 *   Primary contact details.
 * @param address
 *   Customer postal address.
 * @param fiscalCode
 *   Fiscal identifier: fiscal code for individuals, VAT number for companies.
 * @param customerType
 *   Whether the customer is an individual or a company.
 * @param businessName
 *   Business name; required for companies, optional for individuals.
 * @param pec
 *   Certified email (PEC) address.
 * @param notes
 *   Free-form notes about the customer.
 * @param boat
 *   Optional boat details.
 */
final case class Customer(
    id: CustomerId,
    contactInfo: ContactInfo,
    address: Address,
    fiscalCode: TaxCode,
    customerType: CustomerType,
    businessName: Option[String :| BusinessName] = None,
    pec: Option[String :| Email] = None,
    notes: Option[String] = None,
    boat: BoatInfo = BoatInfo.empty,
) derives CanEqual:
  /** Returns a copy with updated contact details. */
  def updateContactInfo(contactInfo: ContactInfo): Customer = this.focus(_.contactInfo).replace(contactInfo)

  /** Returns a copy with an updated postal address. */
  def updateAddress(address: Address): Customer = this.focus(_.address).replace(address)

  /** Returns a copy with an updated fiscal identifier. */
  def updateFiscalCode(fiscalCode: TaxCode): Customer = this.focus(_.fiscalCode).replace(fiscalCode)

  /** Returns a copy with an updated customer classification. */
  def updateCustomerType(customerType: CustomerType): Customer = this.focus(_.customerType).replace(customerType)

  /** Returns a copy with an updated contact email. */
  def updateEmail(email: String :| Email): Customer = this.focus(_.contactInfo.email).replace(email)

  /** Returns a copy with an updated contact phone number. */
  def updatePhone(phone: String :| PhoneNumber): Customer = this.focus(_.contactInfo.phone).replace(phone)

  /** Returns a copy with an updated postal code. */
  def updatePostalCode(postalCode: String :| PostalCode): Customer = this.focus(_.address.postalCode).replace(postalCode)

  /** Returns a copy with updated boat details. */
  def updateBoat(boat: BoatInfo): Customer = this.focus(_.boat).replace(boat)
end Customer

object Customer:
  /**
   * Creates a `Customer` aggregate from raw input values.
   *
   * Validation is delegated to the value objects that make up the aggregate. The fiscal identifier is validated as an Italian fiscal code for
   * individuals and as a VAT number (including check digit) for companies. The business name is required for companies and optional for individuals.
   *
   * @param id
   *   Customer identifier.
   * @param name
   *   Contact first name.
   * @param surname
   *   Contact surname.
   * @param email
   *   Contact email address.
   * @param phone
   *   Contact phone number.
   * @param street
   *   Street name and house number.
   * @param city
   *   City for the postal address.
   * @param postalCode
   *   Postal code in five-digit format.
   * @param country
   *   Country for the postal address.
   * @param fiscalCode
   *   Raw fiscal code (individuals) or VAT number (companies).
   * @param customerType
   *   Customer classification.
   * @param businessName
   *   Raw business name; required for companies.
   * @param pec
   *   Raw certified email (PEC) address.
   * @param notes
   *   Raw free-form notes.
   * @param boatModel
   *   Raw boat model.
   * @param boatName
   *   Raw boat name.
   * @param boatBerth
   *   Raw berth assigned to the boat.
   * @param port
   *   Raw port where the boat is moored.
   */
  def createCustomer(
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
      customerType: CustomerType,
      businessName: Option[String],
      pec: Option[String],
      notes: Option[String],
      boatModel: Option[String],
      boatName: Option[String],
      boatBerth: Option[String],
      port: Option[String],
  ): ValidatedNec[String, Customer] =
    (
      Validated.validNec(id),
      ContactInfo.createContactInfo(name, surname, email, phone),
      Address.createAddress(street, city, postalCode, country),
      validateTaxCode(fiscalCode, customerType),
      Validated.validNec(customerType),
      validateBusinessName(businessName, customerType),
      validatePec(pec),
      Validated.validNec(normalized(notes)),
      Validated.validNec(BoatInfo.createBoatInfo(boatModel, boatName, boatBerth, port)),
    ).mapN(Customer.apply)

  private def validateTaxCode(raw: String, customerType: CustomerType): ValidatedNec[String, TaxCode] =
    val perType: ValidatedNec[String, String] = customerType match
      case CustomerType.Individual => FiscalCode.createFiscalCode(raw)
      case CustomerType.Company => VatNumber.createVatNumber(raw)
    perType.andThen(_.refineValidatedNec[TaxCodeRule])

  private def validateBusinessName(
      raw: Option[String],
      customerType: CustomerType,
  ): ValidatedNec[String, Option[String :| BusinessName]] =
    (normalized(raw), customerType) match
      case (None, CustomerType.Company) => "The business name is required for companies".invalidNec
      case (None, CustomerType.Individual) => Validated.validNec(None)
      case (Some(value), _) => value.refineValidatedNec[BusinessName].map(Some(_))

  private def validatePec(raw: Option[String]): ValidatedNec[String, Option[String :| Email]] =
    normalized(raw).traverse(_.refineValidatedNec[Email])

  private def normalized(value: Option[String]): Option[String] = value.map(_.trim.nn).filter(_.nonEmpty)
end Customer
