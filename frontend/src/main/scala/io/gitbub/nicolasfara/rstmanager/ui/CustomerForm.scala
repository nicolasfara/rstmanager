package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ CustomerRequest, CustomerResponse }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messages, FieldIssue }

/**
 * Pure (DOM-free) state of the customer create/edit form: request building and field-addressed validation.
 *
 * Only the name and surname are required, as on the server. The optional values are checked only when filled, and only for the format the server
 * enforces (same patterns as the domain's refined types), so the form flags them before the round trip but is never stricter than the backend.
 */
object CustomerForm:

  val individual = "individual"
  val company = "company"

  val typeOptions: List[(String, String)] = List(individual -> "Privato", company -> "Azienda")

  /** The fields that can carry a validation issue. */
  enum Field derives CanEqual:
    case Name, Surname, Email, Phone, PostalCode, FiscalCode, Pec

  /** DOM id of the control of `field` (label `for`, inline error anchor, focus target). */
  def fieldId(field: Field): String = field match
    case Field.Name => "cust-name"
    case Field.Surname => "cust-surname"
    case Field.Email => "cust-email"
    case Field.Phone => "cust-phone"
    case Field.PostalCode => "cust-postal-code"
    case Field.FiscalCode => "cust-fiscal-code"
    case Field.Pec => "cust-pec"

  // Same shapes as the domain (`Email`, `PhoneNumber`, `PostalCode`, `FiscalCodeRule`, `VatNumberRule`).
  private val emailPattern = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$".r
  private val phonePattern = "\\+?[0-9]{8,15}".r
  private val postalCodePattern = "[0-9]{5}".r
  private val fiscalCodePattern = "[A-Z]{6}[0-9]{2}[A-Z][0-9]{2}[A-Z][0-9]{3}[A-Z]".r
  private val vatNumberPattern = "[0-9]{11}".r

  /** The Italian VAT number check digit (Luhn-like), as verified by the server. */
  private def vatCheckDigitValid(vat: String): Boolean =
    val digits = vat.map(_.asDigit)
    val total = digits
      .take(10)
      .zipWithIndex
      .map { (digit, index) =>
        if index % 2 == 0 then digit
        else
          val doubled = digit * 2
          if doubled > 9 then doubled - 9 else doubled
      }
      .sum
    (10 - total % 10) % 10 == digits(10)

  private def opt(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  final case class State(
      editingId: Option[UUID],
      name: String,
      surname: String,
      email: String,
      phone: String,
      street: String,
      city: String,
      postalCode: String,
      country: String,
      fiscalCode: String,
      customerType: String,
      businessName: String,
      pec: String,
      notes: String,
      boatModel: String,
      boatName: String,
      boatBerth: String,
      port: String,
  ):
    def isCompany: Boolean = customerType == company

    /** "Partita IVA" for a company, "Codice fiscale" for an individual. */
    def fiscalCodeLabel: String = if isCompany then "Partita IVA" else "Codice fiscale"

    def request: CustomerRequest =
      CustomerRequest(
        name.trim.nn,
        surname.trim.nn,
        opt(email),
        opt(phone),
        opt(street),
        opt(city),
        opt(postalCode),
        opt(country),
        opt(fiscalCode),
        customerType,
        opt(businessName),
        opt(pec),
        opt(notes),
        opt(boatModel),
        opt(boatName),
        opt(boatBerth),
        opt(port),
      )

    private def fiscalCodeIssue: Option[String] = opt(fiscalCode).flatMap { raw =>
      if isCompany then
        if !vatNumberPattern.matches(raw) then Some("Partita IVA non valida (11 cifre)")
        else Option.unless(vatCheckDigitValid(raw))("Partita IVA non valida (cifra di controllo errata)")
      else Option.unless(fiscalCodePattern.matches(raw.toUpperCase.nn))("Codice fiscale non valido (16 caratteri, es. RSSMRA80A01H501U)")
    }

    /** The issues in form order: name, surname, contacts, postal code, fiscal identifier. */
    def issues: List[FieldIssue[Field]] =
      List(
        Option.when(name.trim.nn.isEmpty)(FieldIssue(Field.Name, "Nome obbligatorio")),
        Option.when(surname.trim.nn.isEmpty)(FieldIssue(Field.Surname, "Cognome obbligatorio")),
        opt(email).filterNot(emailPattern.matches).map(_ => FieldIssue(Field.Email, "Email non valida")),
        opt(phone)
          .filterNot(phonePattern.matches)
          .map(_ => FieldIssue(Field.Phone, "Telefono non valido (8–15 cifre senza spazi, + iniziale facoltativo)")),
        opt(pec).filterNot(emailPattern.matches).map(_ => FieldIssue(Field.Pec, "PEC non valida")),
        opt(postalCode).filterNot(postalCodePattern.matches).map(_ => FieldIssue(Field.PostalCode, "CAP non valido (5 cifre)")),
        fiscalCodeIssue.map(FieldIssue(Field.FiscalCode, _)),
      ).flatten

    def errors: List[String] = issues.messages
  end State

  val empty: State = State(None, "", "", "", "", "", "", "", "IT", "", individual, "", "", "", "", "", "", "")

  def fromResponse(c: CustomerResponse): State =
    State(
      Some(c.id),
      c.name,
      c.surname,
      c.email.getOrElse(""),
      c.phone.getOrElse(""),
      c.street.getOrElse(""),
      c.city.getOrElse(""),
      c.postalCode.getOrElse(""),
      c.country.getOrElse(""),
      c.fiscalCode.getOrElse(""),
      c.customerType,
      c.businessName.getOrElse(""),
      c.pec.getOrElse(""),
      c.notes.getOrElse(""),
      c.boatModel.getOrElse(""),
      c.boatName.getOrElse(""),
      c.boatBerth.getOrElse(""),
      c.port.getOrElse(""),
    )
end CustomerForm
