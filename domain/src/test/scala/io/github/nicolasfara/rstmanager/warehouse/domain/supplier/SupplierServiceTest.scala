package io.github.nicolasfara.rstmanager.warehouse.domain.supplier

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.events.SupplierEvent

import cats.Id
import cats.syntax.all.*
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class SupplierServiceTest extends AnyFlatSpecLike:
  private val supplierId: SupplierId = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005a1").nn)

  private val supplier: Supplier =
    Supplier
      .createSupplier(
        supplierId,
        "  Tessuti Nautici Srl ",
        Some(" it 0123 4567 890 "),
        Some("ordini@tessutinautici.it"),
        Some("+39 0541 123456"),
        Some("Via del Porto 3, Rimini"),
        Some(" "),
      )
      .fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private type Result = EdomatonResult[SupplierAggregate, SupplierEvent, SupplierError, Notification]

  private def run(command: Command, state: SupplierAggregate): Result = run(command, state, supplierId)

  private def run(command: Command, state: SupplierAggregate, address: SupplierId): Result =
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, address.toString, command)
    SupplierService[Id].execute(RequestContext(message, state))

  private def rejection(result: Result): List[SupplierError] =
    result match
      case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
      case other => fail(s"Expected a rejection, got: $other")

  "Supplier.createSupplier" should "normalize the raw values" in:
    (supplier.name: String) shouldEqual "Tessuti Nautici Srl"
    supplier.vatNumber shouldEqual Some("IT01234567890")
    supplier.notes shouldEqual None

  it should "accumulate every validation error" in:
    Supplier.createSupplier(supplierId, " ", None, Some("not-an-email"), None, None, None).fold(_.length, _ => 0L) shouldEqual 2L

  "SupplierService" should "create a supplier and publish its id" in:
    run(Command.Create(supplier), SupplierAggregate.Empty) match
      case EdomatonResult.Accepted(SupplierAggregate.Active(created), _, notifications) =>
        created shouldEqual supplier
        notifications.toList shouldEqual List(Notification.SupplierChanged(supplierId))
      case other => fail(s"Unexpected result: $other")

  it should "reject creating an existing supplier, active or deleted" in:
    rejection(run(Command.Create(supplier), SupplierAggregate.Active(supplier))) shouldEqual List(SupplierError.SupplierAlreadyExists)
    rejection(run(Command.Create(supplier), SupplierAggregate.Deleted(supplier))) shouldEqual List(SupplierError.SupplierAlreadyExists)

  it should "update an active supplier and reject updating a missing or deleted one" in:
    val renamed = supplier.copy(notes = Some("Consegna in 5 giorni"))
    run(Command.Update(renamed), SupplierAggregate.Active(supplier)) should matchPattern {
      case EdomatonResult.Accepted(SupplierAggregate.Active(`renamed`), _, _) =>
    }
    rejection(run(Command.Update(renamed), SupplierAggregate.Empty)) shouldEqual List(SupplierError.SupplierNotFound)
    rejection(run(Command.Update(renamed), SupplierAggregate.Deleted(supplier))) shouldEqual List(SupplierError.SupplierNotFound)

  it should "soft-delete a supplier, keeping its data, and publish its id" in:
    run(Command.Delete, SupplierAggregate.Active(supplier)) match
      case EdomatonResult.Accepted(SupplierAggregate.Deleted(kept), _, notifications) =>
        kept shouldEqual supplier
        notifications.toList shouldEqual List(Notification.SupplierChanged(supplierId))
      case other => fail(s"Unexpected result: $other")
    rejection(run(Command.Delete, SupplierAggregate.Deleted(supplier))) shouldEqual List(SupplierError.SupplierNotFound)

  it should "reject a supplier sent to another supplier's stream" in:
    val otherStream = SupplierId(UUID.fromString("00000000-0000-0000-0000-0000000005a2").nn)
    rejection(run(Command.Create(supplier), SupplierAggregate.Empty, otherStream)) shouldEqual List(SupplierError.SupplierIdMismatch)
    rejection(run(Command.Update(supplier), SupplierAggregate.Active(supplier), otherStream)) shouldEqual List(SupplierError.SupplierIdMismatch)

  "SupplierAggregate fold" should "replay the whole lifecycle" in:
    val events = List(SupplierEvent.SupplierCreated(supplier), SupplierEvent.SupplierUpdated(supplier), SupplierEvent.SupplierDeleted)
    val folded =
      events.foldLeft(SupplierAggregate.initial.validNec[SupplierError])((state, event) => state.andThen(SupplierAggregate.transition(event)))
    folded.toOption shouldEqual Some(SupplierAggregate.Deleted(supplier))
end SupplierServiceTest
