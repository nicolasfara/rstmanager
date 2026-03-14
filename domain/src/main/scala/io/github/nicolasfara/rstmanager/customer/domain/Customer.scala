package io.github.nicolasfara.rstmanager.customer.domain

import java.util.UUID

import cats.data.Validated
import cats.data.ValidatedNec
import cats.syntax.all.*
import io.github.iltotore.iron.*
import monocle.syntax.all.*

final case class Customer(
    id: CustomerId,
    contactInfo: ContactInfo,
    address: Address,
    fiscalCode: FiscalCode,
    customerType: CustomerType,
) derives CanEqual:
  def updateContactInfo(contactInfo: ContactInfo): Customer = this.focus(_.contactInfo).replace(contactInfo)

  def updateAddress(address: Address): Customer = this.focus(_.address).replace(address)

  def updateFiscalCode(fiscalCode: FiscalCode): Customer = this.focus(_.fiscalCode).replace(fiscalCode)

  def updateCustomerType(customerType: CustomerType): Customer = this.focus(_.customerType).replace(customerType)

  def updateEmail(email: String :| Email): Customer = this.focus(_.contactInfo.email).replace(email)

  def updatePhone(phone: String :| PhoneNumber): Customer = this.focus(_.contactInfo.phone).replace(phone)

  def updatePostalCode(postalCode: String :| PostalCode): Customer = this.focus(_.address.postalCode).replace(postalCode)
end Customer

object Customer:
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
