package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure supplier form: request building and validation messages. */
final class SupplierFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[SupplierRequest, SupplierRequest] = CanEqual.derived

  private val supplierId = UUID.fromString("00000000-0000-0000-0000-000000000301").nn

  test("request trims the text and sends blank optional values as absent"):
    val state = SupplierForm.empty.copy(name = " Tessuti Srl ", vatNumber = " IT0123 ", email = "  ", notes = "\n")
    state.request shouldBe SupplierRequest("Tessuti Srl", Some("IT0123"), None, None, None, None)

  test("the name is required and an email, when present, must look like one"):
    SupplierForm.empty.errors shouldBe List("Nome obbligatorio")
    SupplierForm.empty.copy(name = "A", email = "not-an-email").errors shouldBe List("Email non valida")
    SupplierForm.empty.copy(name = "A", email = " ordini@tessuti.it ").errors shouldBe Nil

  test("fromResponse -> request round-trips the stored supplier"):
    val response = SupplierResponse(supplierId, "Tessuti Srl", Some("IT0123"), Some("a@b.it"), None, Some("Via Porto 3"), None)
    val state = SupplierForm.fromResponse(response)
    state.editingId shouldBe Some(supplierId)
    state.request shouldBe SupplierRequest("Tessuti Srl", Some("IT0123"), Some("a@b.it"), None, Some("Via Porto 3"), None)
end SupplierFormSpec
