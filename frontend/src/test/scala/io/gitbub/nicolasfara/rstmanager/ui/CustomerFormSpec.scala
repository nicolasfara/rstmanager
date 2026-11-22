package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.CustomerForm.Field
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.FieldIssue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the pure customer form: request building, loading a stored customer, and the field-addressed validation. */
final class CustomerFormSpec extends AnyFunSuite with Matchers:

  private given CanEqual[CustomerRequest, CustomerRequest] = CanEqual.derived
  private given CanEqual[List[FieldIssue[Field]], List[FieldIssue[Field]]] = CanEqual.derived

  private val customerId = UUID.fromString("00000000-0000-0000-0000-000000000501").nn
  private val valid = CustomerForm.empty.copy(name = "Mario", surname = "Rossi")

  private def fieldsOf(state: CustomerForm.State): List[Field] = state.issues.map(_.field)

  test("the customer type is shown in Italian, an unknown one as is"):
    CustomerForm.typeLabel(CustomerForm.individual) shouldBe "Privato"
    CustomerForm.typeLabel(CustomerForm.company) shouldBe "Azienda"
    CustomerForm.typeLabel("other") shouldBe "other"

  test("the display name prefers the business name"):
    valid.displayName shouldBe "Mario Rossi"
    valid.copy(businessName = " Cantiere Rossi Srl ").displayName shouldBe "Cantiere Rossi Srl"

  test("only the name and surname are required"):
    CustomerForm.empty.issues shouldBe List(FieldIssue(Field.Name, "Nome obbligatorio"), FieldIssue(Field.Surname, "Cognome obbligatorio"))
    CustomerForm.empty.errors shouldBe List("Nome obbligatorio", "Cognome obbligatorio")
    valid.errors shouldBe Nil
    CustomerForm.empty.copy(name = "  ", surname = "Rossi").issues.map(_.field) shouldBe List(Field.Name)

  test("the business name stays optional for a company"):
    valid.copy(customerType = CustomerForm.company).errors shouldBe Nil

  test("email and PEC are checked only when filled"):
    valid.copy(email = "  ", pec = "").errors shouldBe Nil
    fieldsOf(valid.copy(email = "mario@", pec = "senza-chiocciola")) shouldBe List(Field.Email, Field.Pec)
    valid.copy(email = " mario@rossi.it ", pec = "mario@pec.it").errors shouldBe Nil

  test("phone and postal code follow the server format"):
    fieldsOf(valid.copy(phone = "333 1234567")) shouldBe List(Field.Phone)
    valid.copy(phone = "+393331234567").errors shouldBe Nil
    fieldsOf(valid.copy(postalCode = "4012")) shouldBe List(Field.PostalCode)
    valid.copy(postalCode = "40121").errors shouldBe Nil

  test("the fiscal identifier is a fiscal code for an individual and a VAT number for a company"):
    valid.copy(fiscalCode = "rssmra80a01h501u").errors shouldBe Nil
    fieldsOf(valid.copy(fiscalCode = "RSSMRA80")) shouldBe List(Field.FiscalCode)
    val company = valid.copy(customerType = CustomerForm.company)
    company.copy(fiscalCode = "00743110157").errors shouldBe Nil
    company.copy(fiscalCode = "00743110158").errors shouldBe List("Partita IVA non valida (cifra di controllo errata)")
    company.copy(fiscalCode = "RSSMRA80A01H501U").errors shouldBe List("Partita IVA non valida (11 cifre)")
    company.fiscalCodeLabel shouldBe "Partita IVA"
    valid.fiscalCodeLabel shouldBe "Codice fiscale"

  test("issues come in form order"):
    val state = CustomerForm.empty.copy(email = "x", postalCode = "1", fiscalCode = "x", phone = "1", pec = "x")
    fieldsOf(state) shouldBe List(Field.Name, Field.Surname, Field.Email, Field.Phone, Field.Pec, Field.PostalCode, Field.FiscalCode)

  test("request trims the text and sends blank optional values as absent"):
    valid.copy(name = " Mario ", email = " ", notes = "\n", port = " Ravenna ").request shouldBe
      CustomerRequest(
        "Mario",
        "Rossi",
        None,
        None,
        None,
        None,
        None,
        Some("IT"),
        None,
        "individual",
        None,
        None,
        None,
        None,
        None,
        None,
        Some("Ravenna"),
      )

  test("fromResponse -> request round-trips the stored customer"):
    val response = CustomerResponse(
      customerId,
      "Anna",
      "Bianchi",
      Some("anna@cantiere.it"),
      None,
      Some("Via Porto 3"),
      Some("Ravenna"),
      Some("48122"),
      Some("IT"),
      Some("00743110157"),
      "company",
      Some("Cantiere Bianchi Srl"),
      None,
      Some("Cliente storico"),
      Some("Beneteau"),
      None,
      Some("A12"),
      Some("Marina di Ravenna"),
    )
    val state = CustomerForm.fromResponse(response)
    state.editingId shouldBe Some(customerId)
    state.errors shouldBe Nil
    state.request shouldBe CustomerRequest(
      "Anna",
      "Bianchi",
      Some("anna@cantiere.it"),
      None,
      Some("Via Porto 3"),
      Some("Ravenna"),
      Some("48122"),
      Some("IT"),
      Some("00743110157"),
      "company",
      Some("Cantiere Bianchi Srl"),
      None,
      Some("Cliente storico"),
      Some("Beneteau"),
      None,
      Some("A12"),
      Some("Marina di Ravenna"),
    )
end CustomerFormSpec
