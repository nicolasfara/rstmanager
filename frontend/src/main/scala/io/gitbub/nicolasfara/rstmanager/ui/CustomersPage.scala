package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Customer registry: minimal CRUD, needed to reference a customer when creating an order. */
object CustomersPage:

  private val typeOptions = List("individual" -> "Privato", "company" -> "Azienda")

  private final case class CustomerFormState(
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
    def isCompany: Boolean = customerType == "company"

    def request: CustomerRequest =
      CustomerRequest(
        name.trim.nn,
        surname.trim.nn,
        email.trim.nn,
        phone.trim.nn,
        street.trim.nn,
        city.trim.nn,
        postalCode.trim.nn,
        country.trim.nn,
        fiscalCode.trim.nn,
        customerType,
        opt(businessName),
        opt(pec),
        opt(notes),
        opt(boatModel),
        opt(boatName),
        opt(boatBerth),
        opt(port),
      )

    def errors: List[String] =
      List(
        Option.when(name.trim.nn.isEmpty)("Nome obbligatorio"),
        Option.when(surname.trim.nn.isEmpty)("Cognome obbligatorio"),
        Option.when(email.trim.nn.isEmpty)("Email obbligatoria"),
        Option.when(phone.trim.nn.isEmpty)("Telefono obbligatorio"),
        Option.when(street.trim.nn.isEmpty)("Via obbligatoria"),
        Option.when(city.trim.nn.isEmpty)("Città obbligatoria"),
        Option.when(postalCode.trim.nn.isEmpty)("CAP obbligatorio"),
        Option.when(fiscalCode.trim.nn.isEmpty)(if isCompany then "Partita IVA obbligatoria" else "Codice fiscale obbligatorio"),
        Option.when(isCompany && businessName.trim.nn.isEmpty)("Ragione sociale obbligatoria per le aziende"),
      ).flatten

  private def opt(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  private def boatCell(c: CustomerResponse): HtmlElement =
    val mooring = List(c.port, c.boatBerth.map(b => s"posto $b")).flatten.mkString(", ")
    if c.boatName.isEmpty && c.boatModel.isEmpty && mooring.isEmpty then div(cls := "text-xs text-slate-400", "—")
    else
      div(
        c.boatName.map(n => div(n)),
        c.boatModel.map(m => div(cls := "text-xs", m)),
        Option.when(mooring.nonEmpty)(div(cls := "text-xs text-slate-400", mooring)),
      )

  private object CustomerFormState:
    val empty: CustomerFormState =
      CustomerFormState(None, "", "", "", "", "", "", "", "IT", "", "individual", "", "", "", "", "", "", "")

    def fromResponse(c: CustomerResponse): CustomerFormState =
      CustomerFormState(
        Some(c.id),
        c.name,
        c.surname,
        c.email,
        c.phone,
        c.street,
        c.city,
        c.postalCode,
        c.country,
        c.fiscalCode,
        c.customerType,
        c.businessName.getOrElse(""),
        c.pec.getOrElse(""),
        c.notes.getOrElse(""),
        c.boatModel.getOrElse(""),
        c.boatName.getOrElse(""),
        c.boatBerth.getOrElse(""),
        c.port.getOrElse(""),
      )

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val form = Var(CustomerFormState.empty)

    def resetForm(): Unit =
      form.set(CustomerFormState.empty)
      formError.set(None)

    def edit(c: CustomerResponse): Unit =
      form.set(CustomerFormState.fromResponse(c))
      formError.set(None)

    def submit(): Unit =
      val current = form.now()
      val effect = current.editingId match
        case Some(id) => ApiClient.updateCustomer(id, current.request)
        case None => ApiClient.createCustomer(current.request)
      effect.foreach {
        case Right(_) => resetForm(); AppBus.mutatedCustomers()
        case Left(err) => showError(formError, "Salvataggio cliente")(err)
      }
    end submit

    def delete(id: UUID): Unit =
      ApiClient.deleteCustomer(id).foreach {
        case Right(_) => AppBus.mutatedCustomers()
        case Left(err) => showError(formError, "Eliminazione cliente")(err)
      }

    val formErrors: Signal[List[String]] = form.signal.map(_.errors)

    val data = loadable(AppBus.customersTicks)(() => ApiClient.listCustomers())

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[24rem_1fr]"),
      roleGated(Role.Admin)(
        card(
          cls := "self-start p-4",
          sectionTitle("Nuovo cliente"),
          div(
            cls := "mt-3 grid grid-cols-2 gap-3",
            field("Nome", textInput(form.signal.map(_.name), Observer[String](v => form.update(_.copy(name = v))), "")),
            field("Cognome", textInput(form.signal.map(_.surname), Observer[String](v => form.update(_.copy(surname = v))), "")),
            div(
              cls := "col-span-2",
              field("Ragione sociale", textInput(form.signal.map(_.businessName), Observer[String](v => form.update(_.copy(businessName = v))), "Opzionale per i privati")),
            ),
            field("Email", textInput(form.signal.map(_.email), Observer[String](v => form.update(_.copy(email = v))), "", "email")),
            field("Telefono", textInput(form.signal.map(_.phone), Observer[String](v => form.update(_.copy(phone = v))), "")),
            field("Via", textInput(form.signal.map(_.street), Observer[String](v => form.update(_.copy(street = v))), "")),
            field("Città", textInput(form.signal.map(_.city), Observer[String](v => form.update(_.copy(city = v))), "")),
            field("CAP", textInput(form.signal.map(_.postalCode), Observer[String](v => form.update(_.copy(postalCode = v))), "")),
            field("Paese", textInput(form.signal.map(_.country), Observer[String](v => form.update(_.copy(country = v))), "")),
            field("Tipo", staticSelect(form.signal.map(_.customerType), Observer[String](v => form.update(_.copy(customerType = v))), typeOptions)),
            child <-- form.signal.map(_.isCompany).distinct.map { company =>
              field(
                if company then "Partita IVA" else "Codice fiscale",
                textInput(form.signal.map(_.fiscalCode), Observer[String](v => form.update(_.copy(fiscalCode = v))), ""),
              )
            },
            div(
              cls := "col-span-2",
              field("PEC", textInput(form.signal.map(_.pec), Observer[String](v => form.update(_.copy(pec = v))), "Opzionale", "email")),
            ),
            field("Modello barca", textInput(form.signal.map(_.boatModel), Observer[String](v => form.update(_.copy(boatModel = v))), "Opzionale")),
            field("Nome barca", textInput(form.signal.map(_.boatName), Observer[String](v => form.update(_.copy(boatName = v))), "Opzionale")),
            field("Posto barca", textInput(form.signal.map(_.boatBerth), Observer[String](v => form.update(_.copy(boatBerth = v))), "Opzionale")),
            field("Porto", textInput(form.signal.map(_.port), Observer[String](v => form.update(_.copy(port = v))), "Opzionale")),
            div(
              cls := "col-span-2",
              field("Note", textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "Note generiche (opzionale)")),
            ),
          ),
          child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
          div(
            cls := "mt-3 flex gap-2",
            button(tpe := "button", cls := btnPrimary, child.text <-- form.signal.map(_.editingId.fold("Crea")(_ => "Salva")), disabled <-- formErrors.map(_.nonEmpty), onClick --> (_ => submit())),
            child.maybe <-- form.signal.map(_.editingId.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
          ),
        ),
      ),
      card(
        cls := "overflow-hidden",
        renderResult(data) { customers =>
          if customers.isEmpty then emptyState("Nessun cliente registrato.")
          else
            table(
              cls := "w-full text-sm",
              thead(
                cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                tr(
                  th(cls := "px-4 py-2", "Cliente"),
                  th(cls := "px-4 py-2", "Contatti"),
                  th(cls := "px-4 py-2", "Barca"),
                  th(cls := "px-4 py-2", "Tipo"),
                  th(cls := "px-4 py-2"),
                ),
              ),
              tbody(
                customers.map { c =>
                  tr(
                    cls := "border-b border-slate-100 last:border-0",
                    td(
                      cls := "px-4 py-2",
                      div(cls := "font-medium text-slate-800", c.businessName.getOrElse(s"${c.name} ${c.surname}")),
                      c.businessName.map(_ => div(cls := "text-xs text-slate-500", s"${c.name} ${c.surname}")),
                      div(cls := "text-xs text-slate-400", s"${c.city}, ${c.country}"),
                    ),
                    td(cls := "px-4 py-2 text-slate-500", div(c.email), div(cls := "text-xs", c.phone)),
                    td(cls := "px-4 py-2 text-slate-500", boatCell(c)),
                    td(cls := "px-4 py-2", statusBadge(c.customerType)),
                    td(
                      cls := "px-4 py-2 text-right",
                      roleGated(Role.Admin)(
                        div(
                          cls := "flex justify-end gap-2",
                          button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(c))),
                          button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => delete(c.id))),
                        ),
                      ),
                    ),
                  )
                },
              ),
            )
        },
      ),
    )
  end apply
end CustomersPage
