package io.github.nicolasfara.rstmanager.customer.domain

import java.util.UUID

import cats.data.{ Validated, ValidatedNec }
import cats.syntax.all.*
import io.github.iltotore.iron.*
import monocle.syntax.all.*

/** Customer aggregate containing identity, contacts, address, and fiscal classification.
  *
  * @param id
  *   Stable customer identifier.
  * @param contactInfo
  *   Primary contact details.
  * @param address
  *   Customer postal address.
  * @param fiscalCode
  *   Customer fiscal code.
  * @param customerType
  *   Whether the customer is an individual or a company.
  */
final case class Customer(
    id: CustomerId,
    contactInfo: ContactInfo,
    address: Address,
    fiscalCode: FiscalCode,
    customerType: CustomerType,
) derives CanEqual:
  /** Returns a copy with updated contact details. */
  def updateContactInfo(contactInfo: ContactInfo): Customer = this.focus(_.contactInfo).replace(contactInfo)

  /** Returns a copy with an updated postal address. */
  def updateAddress(address: Address): Customer = this.focus(_.address).replace(address)

  /** Returns a copy with an updated fiscal code. */
  def updateFiscalCode(fiscalCode: FiscalCode): Customer = this.focus(_.fiscalCode).replace(fiscalCode)

  /** Returns a copy with an updated customer classification. */
  def updateCustomerType(customerType: CustomerType): Customer = this.focus(_.customerType).replace(customerType)

  /** Returns a copy with an updated contact email. */
  def updateEmail(email: String :| Email): Customer = this.focus(_.contactInfo.email).replace(email)

  /** Returns a copy with an updated contact phone number. */
  def updatePhone(phone: String :| PhoneNumber): Customer = this.focus(_.contactInfo.phone).replace(phone)

  /** Returns a copy with an updated postal code. */
  def updatePostalCode(postalCode: String :| PostalCode): Customer = this.focus(_.address.postalCode).replace(postalCode)
end Customer

object Customer:
  /** Creates a `Customer` aggregate from raw input values.
    *
    * Validation is delegated to the value objects that make up the aggregate.
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
    *   Raw fiscal code.
    * @param customerType
    *   Customer classification.
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
  ): ValidatedNec[String, Customer] =
    (
      Validated.validNec(id),
      ContactInfo.createContactInfo(name, surname, email, phone),
      Address.createAddress(street, city, postalCode, country),
      FiscalCode.createFiscalCode(fiscalCode),
      Validated.validNec(customerType),
    ).mapN(Customer.apply)
end Customer
