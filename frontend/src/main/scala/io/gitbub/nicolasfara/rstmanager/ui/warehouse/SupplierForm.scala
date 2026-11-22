package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ SupplierRequest, SupplierResponse }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messages, FieldIssue }

/** Pure (DOM-free) state of the supplier create/edit form: request building and validation problems, each tied to a [[Field]]. */
object SupplierForm:

  /** Same shape the server accepts for an email (`SupplierEmail` in the domain), so the form flags it before the round trip. */
  private val emailPattern = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$".r

  /** What a validation problem is about. */
  enum Field derives CanEqual:
    case Name
    case Email

  /** The DOM id of the input of `field` (the anchor of its label and of [[FormFocus]]). */
  def fieldId(field: Field): String = field match
    case Field.Name => "supplier-name"
    case Field.Email => "supplier-email"

  final case class State(
      editingId: Option[UUID],
      name: String,
      vatNumber: String,
      email: String,
      phone: String,
      address: String,
      notes: String,
  ) derives CanEqual:
    /** The payload to send: text trimmed, blank optional values absent. */
    def request: SupplierRequest =
      SupplierRequest(name.trim.nn, opt(vatNumber), opt(email), opt(phone), opt(address), opt(notes))

    /** Every validation problem, each tied to its field. */
    def issues: List[FieldIssue[Field]] =
      List(
        Option.when(name.trim.nn.isEmpty)(FieldIssue(Field.Name, "Nome obbligatorio")),
        opt(email).filterNot(emailPattern.matches).map(_ => FieldIssue(Field.Email, "Email non valida")),
      ).flatten

    def errors: List[String] = issues.messages
  end State

  val empty: State = State(None, "", "", "", "", "", "")

  def fromResponse(s: SupplierResponse): State =
    State(
      Some(s.id),
      s.name,
      s.vatNumber.getOrElse(""),
      s.email.getOrElse(""),
      s.phone.getOrElse(""),
      s.address.getOrElse(""),
      s.notes.getOrElse(""),
    )

  /** Trims free text and collapses blank strings to `None`. */
  private[warehouse] def opt(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)
end SupplierForm
