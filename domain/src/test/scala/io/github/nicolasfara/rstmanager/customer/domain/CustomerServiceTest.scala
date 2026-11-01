package io.github.nicolasfara.rstmanager.customer.domain

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.customer.domain.events.CustomerEvent

import cats.Id
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class CustomerServiceTest extends AnyFlatSpecLike:
  private val customerId: CustomerId = CustomerId(UUID.fromString("00000000-0000-0000-0000-0000000000c1").nn)

  private def customerOf(customerType: CustomerType, fiscalCode: String, businessName: Option[String]): Customer =
    Customer
      .createCustomer(
        customerId,
        "Mario",
        "Rossi",
        "mario@example.com",
        "+393331234567",
        "Via Roma 1",
        "Bologna",
        "40100",
        "Italia",
        fiscalCode,
        customerType,
        businessName,
        None,
        None,
        None,
        None,
        None,
        None,
      )
      .fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private val individual = customerOf(CustomerType.Individual, "RSSMRA80A01H501U", None)
  private val company = customerOf(CustomerType.Company, "12345678903", Some("Rossi Srl"))

  private def run(command: Command, state: CustomerAggregate): EdomatonResult[CustomerAggregate, CustomerEvent, CustomerError, Notification] =
    run(command, state, customerId)

  private def run(
      command: Command,
      state: CustomerAggregate,
      address: CustomerId,
  ): EdomatonResult[CustomerAggregate, CustomerEvent, CustomerError, Notification] =
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, address.toString, command)
    CustomerService[Id].execute(RequestContext(message, state))

  private def rejection(result: EdomatonResult[CustomerAggregate, CustomerEvent, CustomerError, Notification]): List[CustomerError] =
    result match
      case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
      case other => fail(s"Expected a rejection, got: $other")

  "CustomerService" should "create valid individual and company customers" in:
    List(individual, company).foreach { customer =>
      run(Command.Create(customer), CustomerAggregate.Empty) should matchPattern { case EdomatonResult.Accepted(CustomerAggregate.Active(_), _, _) =>
      }
    }

  it should "reject a company without a business name, however the customer was built" in:
    rejection(run(Command.Create(company.copy(businessName = None)), CustomerAggregate.Empty)) shouldEqual List(CustomerError.BusinessNameRequired)

  it should "reject a fiscal identifier that does not match the customer type" in:
    rejection(
      run(Command.Create(individual.updateCustomerType(CustomerType.Company).copy(businessName = company.businessName)), CustomerAggregate.Empty),
    ) shouldEqual
      List(CustomerError.TaxCodeDoesNotMatchCustomerType(CustomerType.Company))
    rejection(run(Command.Update(company.updateCustomerType(CustomerType.Individual)), CustomerAggregate.Active(company))) shouldEqual
      List(CustomerError.TaxCodeDoesNotMatchCustomerType(CustomerType.Individual))

  it should "reject a customer sent to another customer's stream" in:
    val otherStream: CustomerId = CustomerId(UUID.fromString("00000000-0000-0000-0000-0000000000c2").nn)
    rejection(run(Command.Create(individual), CustomerAggregate.Empty, otherStream)) shouldEqual List(CustomerError.CustomerIdMismatch)
  it should "publish the deleted customer's id, like every catalog aggregate" in:
    run(Command.Delete, CustomerAggregate.Active(individual)) match
      case EdomatonResult.Accepted(CustomerAggregate.Deleted(_), _, notifications) =>
        notifications.toList shouldEqual List(Notification.CustomerChanged(customerId))
      case other => fail(s"Unexpected result: $other")
end CustomerServiceTest
