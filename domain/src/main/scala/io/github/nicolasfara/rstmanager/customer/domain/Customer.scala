package io.github.nicolasfara.rstmanager.customer.domain

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
 * Customer master-data record (the state held by [[CustomerAggregate]]): identity, contacts, address, and fiscal classification. Only the contact
 * name and surname are required.
 *
 * @param id
 *   Stable customer identifier.
 * @param contactInfo
 *   Primary contact details.
 * @param address
 *   Customer postal address.
 * @param fiscalCode
 *   Optional fiscal identifier: fiscal code for individuals, VAT number for companies.
 * @param customerType
 *   Whether the customer is an individual or a company.
 * @param businessName
 *   Optional business name.
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
    fiscalCode: Option[TaxCode],
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
  def updateFiscalCode(fiscalCode: Option[TaxCode]): Customer = this.focus(_.fiscalCode).replace(fiscalCode)

  /** Returns a copy with an updated customer classification. */
  def updateCustomerType(customerType: CustomerType): Customer = this.focus(_.customerType).replace(customerType)

  /** Returns a copy with an updated contact email. */
  def updateEmail(email: Option[String :| Email]): Customer = this.focus(_.contactInfo.email).replace(email)

  /** Returns a copy with an updated contact phone number. */
  def updatePhone(phone: Option[String :| PhoneNumber]): Customer = this.focus(_.contactInfo.phone).replace(phone)

  /** Returns a copy with an updated postal code. */
  def updatePostalCode(postalCode: Option[String :| PostalCode]): Customer = this.focus(_.address.postalCode).replace(postalCode)

  /** Returns a copy with updated boat details. */
  def updateBoat(boat: BoatInfo): Customer = this.focus(_.boat).replace(boat)

  /**
   * Rules every customer must satisfy however it was built (smart constructor, `copy`, update helpers): a fiscal identifier, when present, must be an
   * Italian fiscal code for an individual and a valid VAT number for a company. The aggregate enforces them on every create and update.
   */
  def checkInvariants: ValidatedNec[CustomerError, Customer] =
    val taxCodeMatchesType = fiscalCode.forall { code =>
      customerType match
        case CustomerType.Individual => FiscalCode.createFiscalCode(code).isValid
        case CustomerType.Company => VatNumber.createVatNumber(code).isValid
    }
    Validated.condNec(taxCodeMatchesType, this, CustomerError.TaxCodeDoesNotMatchCustomerType(customerType))
end Customer

object Customer:
  /**
   * Creates a `Customer` from raw input values.
   *
   * Validation is delegated to the value objects that make up the aggregate. Only the name and surname are required: blank optional values become
   * absent, present ones are validated. The fiscal identifier is validated as an Italian fiscal code for individuals and as a VAT number (including
   * check digit) for companies.
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
   *   Raw business name.
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
      id: CustomerId,
      name: String,
      surname: String,
      email: Option[String],
      phone: Option[String],
      street: Option[String],
      city: Option[String],
      postalCode: Option[String],
      country: Option[String],
      fiscalCode: Option[String],
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
      normalized(fiscalCode).traverse(validateTaxCode(_, customerType)),
      Validated.validNec(customerType),
      normalized(businessName).traverse(_.refineValidatedNec[BusinessName]),
      normalized(pec).traverse(_.refineValidatedNec[Email]),
      Validated.validNec(normalized(notes)),
      Validated.validNec(BoatInfo.createBoatInfo(boatModel, boatName, boatBerth, port)),
    ).mapN(Customer.apply)

  private def validateTaxCode(raw: String, customerType: CustomerType): ValidatedNec[String, TaxCode] =
    val perType: ValidatedNec[String, String] = customerType match
      case CustomerType.Individual => FiscalCode.createFiscalCode(raw)
      case CustomerType.Company => VatNumber.createVatNumber(raw)
    perType.andThen(_.refineValidatedNec[TaxCodeRule])

  private def normalized(value: Option[String]): Option[String] = value.map(_.trim.nn).filter(_.nonEmpty)
end Customer
