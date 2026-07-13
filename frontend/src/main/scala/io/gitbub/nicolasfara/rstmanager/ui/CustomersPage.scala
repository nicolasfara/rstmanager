package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Customer registry: minimal CRUD, needed to reference a customer when creating an order. */
object CustomersPage:

  private val typeOptions = List("individual" -> "Privato", "company" -> "Azienda")

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val editingId = Var(Option.empty[UUID])
    val name = Var("")
    val surname = Var("")
    val email = Var("")
    val phone = Var("")
    val street = Var("")
    val city = Var("")
    val postalCode = Var("")
    val country = Var("IT")
    val fiscalCode = Var("")
    val customerType = Var("individual")

    def resetForm(): Unit =
      editingId.set(None)
      List(name, surname, email, phone, street, city, postalCode, fiscalCode).foreach(_.set(""))
      country.set("IT"); customerType.set("individual"); formError.set(None)

    def edit(c: CustomerResponse): Unit =
      editingId.set(Some(c.id))
      name.set(c.name); surname.set(c.surname); email.set(c.email); phone.set(c.phone)
      street.set(c.street); city.set(c.city); postalCode.set(c.postalCode); country.set(c.country)
      fiscalCode.set(c.fiscalCode); customerType.set(c.customerType); formError.set(None)

    def submit(): Unit =
      val request = CustomerRequest(
        name.now().trim.nn,
        surname.now().trim.nn,
        email.now().trim.nn,
        phone.now().trim.nn,
        street.now().trim.nn,
        city.now().trim.nn,
        postalCode.now().trim.nn,
        country.now().trim.nn,
        fiscalCode.now().trim.nn,
        customerType.now(),
      )
      val effect = editingId.now() match
        case Some(id) => ApiClient.updateCustomer(id, request)
        case None => ApiClient.createCustomer(request)
      effect.foreach {
        case Right(_) => resetForm(); AppBus.mutated()
        case Left(err) => formError.set(Some(err))
      }
    end submit

    def delete(id: UUID): Unit =
      ApiClient.deleteCustomer(id).foreach {
        case Right(_) => AppBus.mutated()
        case Left(err) => formError.set(Some(err))
      }

    val data = loadable(AppBus.ticks)(() => ApiClient.listCustomers())

    div(
      cls := "grid gap-6 lg:grid-cols-[24rem_1fr]",
      card(
        cls := "self-start p-4",
        sectionTitle("Nuovo cliente"),
        div(
          cls := "mt-3 grid grid-cols-2 gap-3",
          field("Nome", textInput(name)),
          field("Cognome", textInput(surname)),
          field("Email", textInput(email, "", "email")),
          field("Telefono", textInput(phone)),
          field("Via", textInput(street)),
          field("Città", textInput(city)),
          field("CAP", textInput(postalCode)),
          field("Paese", textInput(country)),
          field("Codice fiscale", textInput(fiscalCode)),
          field("Tipo", staticSelect(customerType, typeOptions)),
        ),
        child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
        div(
          cls := "mt-3 flex gap-2",
          button(tpe := "button", cls := btnPrimary, child.text <-- editingId.signal.map(_.fold("Crea")(_ => "Salva")), onClick --> (_ => submit())),
          child.maybe <-- editingId.signal.map(_.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
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
                tr(th(cls := "px-4 py-2", "Cliente"), th(cls := "px-4 py-2", "Contatti"), th(cls := "px-4 py-2", "Tipo"), th(cls := "px-4 py-2")),
              ),
              tbody(
                customers.map { c =>
                  tr(
                    cls := "border-b border-slate-100 last:border-0",
                    td(
                      cls := "px-4 py-2",
                      div(cls := "font-medium text-slate-800", s"${c.name} ${c.surname}"),
                      div(cls := "text-xs text-slate-400", s"${c.city}, ${c.country}"),
                    ),
                    td(cls := "px-4 py-2 text-slate-500", div(c.email), div(cls := "text-xs", c.phone)),
                    td(cls := "px-4 py-2", statusBadge(c.customerType)),
                    td(
                      cls := "px-4 py-2 text-right",
                      div(
                        cls := "flex justify-end gap-2",
                        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(c))),
                        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => delete(c.id))),
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
