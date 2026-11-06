package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ SupplierRequest, SupplierResponse }

/** Pure (DOM-free) state of the supplier create/edit form: request building and validation messages. */
object SupplierForm:

  /** Same shape the server accepts for an email (`SupplierEmail` in the domain), so the form flags it before the round trip. */
  private val emailPattern = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$".r

  final case class State(
      editingId: Option[UUID],
      name: String,
      vatNumber: String,
      email: String,
      phone: String,
      address: String,
      notes: String,
  ):
    /** The payload to send: text trimmed, blank optional values absent. */
    def request: SupplierRequest =
      SupplierRequest(name.trim.nn, opt(vatNumber), opt(email), opt(phone), opt(address), opt(notes))

    def errors: List[String] =
      List(
        Option.when(name.trim.nn.isEmpty)("Nome obbligatorio"),
        opt(email).filterNot(emailPattern.matches).map(_ => "Email non valida"),
      ).flatten
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
