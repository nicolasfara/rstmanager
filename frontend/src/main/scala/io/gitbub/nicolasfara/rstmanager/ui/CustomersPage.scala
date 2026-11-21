package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, FieldIssue }
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/** Customer registry: minimal CRUD, needed to reference a customer when creating an order. */
object CustomersPage:

  private def boatCell(c: CustomerResponse): HtmlElement =
    val mooring = List(c.port, c.boatBerth.map(b => s"posto $b")).flatten.mkString(", ")
    if c.boatName.isEmpty && c.boatModel.isEmpty && mooring.isEmpty then div(cls := "text-xs text-slate-400", "—")
    else
      div(
        c.boatName.map(n => div(n)),
        c.boatModel.map(m => div(cls := "text-xs", m)),
        Option.when(mooring.nonEmpty)(div(cls := "text-xs text-slate-400", mooring)),
      )

  private def locationLine(c: CustomerResponse): Option[String] =
    Some(List(c.city, c.country).flatten.mkString(", ")).filter(_.nonEmpty)

  private def contactsCell(c: CustomerResponse): HtmlElement =
    if c.email.isEmpty && c.phone.isEmpty then div(cls := "text-xs text-slate-400", "—")
    else div(c.email.map(e => div(e)), c.phone.map(p => div(cls := "text-xs", p)))

  private def displayName(c: CustomerResponse): String = c.businessName.getOrElse(s"${c.name} ${c.surname}")

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val form = Var(CustomerForm.empty)
    val touch = FormTouch()
    val submitting = Var(false)
    val pendingDelete = Var(Option.empty[CustomerResponse])
    val draft = DraftGuard[CustomerForm.State](
      () => form.now(),
      s => DraftGuard.discardTitle(Option.when(s.editingId.isDefined)(s.displayName), "il nuovo cliente"),
    )

    def resetForm(): Unit =
      form.set(CustomerForm.empty)
      formError.set(None)
      touch.reset()
      draft.markClean()

    val formAnchor = FormAnchor()

    // Replacing a dirty draft asks first; the form is revealed once the new content is loaded.
    def edit(c: CustomerResponse): Unit =
      draft.discardThen { () =>
        formAnchor.revealAndFocus()
        form.set(CustomerForm.fromResponse(c))
        formError.set(None)
        touch.reset()
        draft.markClean()
      }

    /** "+ Nuovo cliente" (mobile, where the form follows the list): leaves a record being edited, then shows the form. */
    def startNew(): Unit =
      if form.now().editingId.isDefined then
        draft.discardThen(() =>
          resetForm(); formAnchor.revealAndFocus(),
        )
      else formAnchor.revealAndFocus()

    // Always clickable: an invalid form reveals every error and focuses the first one instead of sending; re-entrant clicks are ignored.
    def submit(): Unit =
      val current = form.now()
      if !submitting.now() then
        current.issues.headOption match
          case Some(first) =>
            touch.attemptSubmit()
            FormFocus.focus(CustomerForm.fieldId(first.field))
          case None =>
            submitting.set(true)
            val effect = current.editingId match
              case Some(id) => ApiClient.updateCustomer(id, current.request)
              case None => ApiClient.createCustomer(current.request)
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(_) =>
                  resetForm()
                  Notifications.success(s"Cliente ${current.displayName} ${if current.editingId.isDefined then "salvato" else "creato"}")
                  AppBus.mutatedCustomers()
                case Left(err) => showError(formError, "Salvataggio cliente")(err)
            }
    end submit

    def delete(c: CustomerResponse): Unit =
      ApiClient.deleteCustomer(c.id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(c.id) then resetForm()
          Notifications.success(s"Cliente ${displayName(c)} eliminato")
          AppBus.mutatedCustomers()
        case Left(err) => showError(formError, "Eliminazione cliente")(err)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(c => s"Eliminare il cliente «${displayName(c)}»?")),
        message = Signal.fromValue("Il cliente viene rimosso dall'anagrafica. L'operazione non è reversibile."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { c => delete(c); pendingDelete.set(None) },
      )

    val issues: Signal[List[FieldIssue[CustomerForm.Field]]] = form.signal.map(_.issues)
    def errorOf(field: CustomerForm.Field): Signal[Option[String]] = issues.map(_.messageFor(field)).distinct

    /** An optional text field without a validation rule. */
    def plainSpec(labelText: String, id: String): FieldSpec = FieldSpec(labelText, id)

    /** A text field with a validation rule, its error gated by [[FormTouch]]. */
    def checkedSpec(labelText: String, field: CustomerForm.Field, required: Boolean): FieldSpec =
      touch.spec(labelText, CustomerForm.fieldId(field), required, errorOf(field))

    def textField(
        spec: FieldSpec,
        get: CustomerForm.State => String,
        set: (CustomerForm.State, String) => CustomerForm.State,
        placeholderText: String,
        inputType: String,
    ): HtmlElement =
      Components.field(spec)(bind =>
        textInput(form.signal.map(get), Observer[String](v => form.update(set(_, v))), placeholderText, inputType).amend(bind),
      )

    def wide(el: HtmlElement): HtmlElement = el.amend(cls := "col-span-2")

    val anagrafica = fieldGroup("Anagrafica")(
      Components.field(FieldSpec("Tipo", "cust-type"))(bind =>
        segmented(
          form.signal.map(_.customerType),
          Observer[String](v => form.update(_.copy(customerType = v))),
          CustomerForm.typeOptions,
          List(bind, aria.label := "Tipo cliente"),
        ),
      ),
      // The business name is optional on the server: shown for a company (or when a stored individual still has one, so it is never hidden).
      child.maybe <-- form.signal.map(s => s.isCompany || s.businessName.trim.nn.nonEmpty).distinct.map { shown =>
        Option.when(shown)(
          textField(
            FieldSpec("Ragione sociale", "cust-business-name", hintText = Some("Nome e cognome sono quelli del referente.")),
            _.businessName,
            (s, v) => s.copy(businessName = v),
            "Cantiere Rossi Srl",
            "text",
          ),
        )
      },
      div(
        cls := "grid grid-cols-2 gap-3",
        textField(checkedSpec("Nome", CustomerForm.Field.Name, required = true), _.name, (s, v) => s.copy(name = v), "", "text"),
        textField(checkedSpec("Cognome", CustomerForm.Field.Surname, required = true), _.surname, (s, v) => s.copy(surname = v), "", "text"),
      ),
    )

    val contatti = fieldGroup("Contatti")(
      div(
        cls := "grid grid-cols-2 gap-3",
        wide(
          textField(
            checkedSpec("Email", CustomerForm.Field.Email, required = false),
            _.email,
            (s, v) => s.copy(email = v),
            "nome@esempio.it",
            "email",
          ),
        ),
        textField(checkedSpec("Telefono", CustomerForm.Field.Phone, required = false), _.phone, (s, v) => s.copy(phone = v), "+393331234567", "tel"),
        textField(checkedSpec("PEC", CustomerForm.Field.Pec, required = false), _.pec, (s, v) => s.copy(pec = v), "nome@pec.it", "email"),
      ),
    )

    val indirizzo = fieldGroup("Indirizzo")(
      div(
        cls := "grid grid-cols-[6rem_minmax(0,1fr)] gap-3",
        wide(textField(plainSpec("Via", "cust-street"), _.street, (s, v) => s.copy(street = v), "Via Roma 1", "text")),
        textField(checkedSpec("CAP", CustomerForm.Field.PostalCode, required = false), _.postalCode, (s, v) => s.copy(postalCode = v), "", "text"),
        textField(plainSpec("Città", "cust-city"), _.city, (s, v) => s.copy(city = v), "", "text"),
        wide(textField(plainSpec("Paese", "cust-country"), _.country, (s, v) => s.copy(country = v), "IT", "text")),
      ),
    )

    // The label (and the format checked) follow the customer type; rebuilt only when the type changes, not on every keystroke.
    val datiFiscali = fieldGroup("Dati fiscali")(
      child <-- form.signal.map(_.fiscalCodeLabel).distinct.map { labelText =>
        textField(checkedSpec(labelText, CustomerForm.Field.FiscalCode, required = false), _.fiscalCode, (s, v) => s.copy(fiscalCode = v), "", "text")
      },
    )

    val imbarcazione = fieldGroup("Imbarcazione")(
      div(
        cls := "grid grid-cols-2 gap-3",
        textField(plainSpec("Nome barca", "cust-boat-name"), _.boatName, (s, v) => s.copy(boatName = v), "", "text"),
        textField(plainSpec("Modello", "cust-boat-model"), _.boatModel, (s, v) => s.copy(boatModel = v), "", "text"),
        textField(plainSpec("Porto", "cust-port"), _.port, (s, v) => s.copy(port = v), "", "text"),
        textField(plainSpec("Posto barca", "cust-berth"), _.boatBerth, (s, v) => s.copy(boatBerth = v), "", "text"),
      ),
    )

    val note = fieldGroup("Note")(
      Components.field(FieldSpec("Note generiche", "cust-notes"))(bind =>
        textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "").amend(bind),
      ),
    )

    val data = loadable(AppBus.customersTicks)(() => ApiClient.listCustomers())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[CustomerResponse](pager)(
      "customer" -> Column.text(displayName),
      "contacts" -> Column.optionalText(c => c.email.orElse(c.phone)),
      "boat" -> Column.optionalText(c => c.boatName.orElse(c.boatModel)),
      "type" -> Column.text(c => CustomerForm.typeLabel(c.customerType)),
    )

    def typeBadge(c: CustomerResponse): HtmlElement = badge(CustomerForm.typeLabel(c.customerType), "bg-slate-50 text-slate-600 border-slate-200")

    def actionButtons(c: CustomerResponse): List[HtmlElement] =
      List(
        button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(c))),
        button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(c)))),
      )

    def renderRow(c: CustomerResponse): HtmlElement =
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        td(
          cls := "px-4 py-2",
          div(cls := "font-medium text-slate-800", displayName(c)),
          c.businessName.map(_ => div(cls := "text-xs text-slate-500", s"${c.name} ${c.surname}")),
          locationLine(c).map(l => div(cls := "text-xs text-slate-400", l)),
        ),
        td(cls := "px-4 py-2 text-slate-500", contactsCell(c)),
        td(cls := "px-4 py-2 text-slate-500", boatCell(c)),
        td(cls := "px-4 py-2", typeBadge(c)),
        td(
          cls := "px-4 py-2 text-right",
          roleGated(Role.Admin)(div(cls := "flex justify-end gap-2", actionButtons(c))),
        ),
      )

    def renderCard(c: CustomerResponse): HtmlElement =
      div(
        cls := "space-y-3 p-4",
        div(
          cls := "flex items-start justify-between gap-3",
          div(
            cls := "min-w-0",
            div(cls := "break-words font-medium text-slate-800", displayName(c)),
            c.businessName.map(_ => div(cls := "text-xs text-slate-500", s"${c.name} ${c.surname}")),
            locationLine(c).map(l => div(cls := "text-xs text-slate-400", l)),
          ),
          div(cls := "shrink-0", typeBadge(c)),
        ),
        div(
          cls := "grid grid-cols-2 gap-3 text-sm text-slate-600",
          div(cls := "min-w-0 break-words", div(cls := "text-xs font-medium text-slate-500", "Contatti"), contactsCell(c)),
          div(cls := "min-w-0 break-words", div(cls := "text-xs font-medium text-slate-500", "Barca"), boatCell(c)),
        ),
        roleGated(Role.Admin)(div(cls := "flex flex-wrap gap-2 border-t border-slate-100 pt-3", actionButtons(c))),
      )

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[24rem_1fr]"),
      roleGated(Role.Admin)(
        card(
          // Mobile: the list comes first (finding a record must not mean scrolling past the form); "+ Nuovo cliente" reaches the form.
          cls := "order-last self-start p-4 lg:order-none",
          formAnchor.bind,
          draft.bind,
          child <-- form.signal
            .map(_.editingId.isDefined)
            .distinct
            .map(editing => sectionTitle(if editing then "Modifica cliente" else "Nuovo cliente")),
          requiredLegend.amend(cls := "mt-0.5"),
          div(cls := "mt-4 space-y-5", anagrafica, contatti, indirizzo, datiFiscali, imbarcazione, note),
          child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
          div(
            cls := "mt-4",
            child <-- form.signal.map(_.editingId.isDefined).distinct.map { editing =>
              formActions(
                if editing then "Salva" else "Crea",
                "Salvataggio…",
                submitting.signal,
                Option.when(editing)(() => draft.discardThen(() => resetForm())),
                () => submit(),
              )
            },
          ),
        ),
      ),
      card(
        cls := "overflow-hidden",
        div(
          cls := "space-y-2 border-b border-slate-100 p-3",
          div(
            cls := "flex items-center gap-2",
            div(cls := "min-w-0 flex-1", searchBox(query, "Cerca per nome, ragione sociale, contatti, barca…", pager)),
            roleGated(Role.Admin)(
              button(tpe := "button", cls := s"$btnSmall min-h-10 shrink-0 lg:hidden", "+ Nuovo cliente", onClick --> (_ => startNew())),
            ),
          ),
          div(cls := "md:hidden", sort.picker(List("customer" -> "Cliente", "contacts" -> "Contatti", "boat" -> "Barca", "type" -> "Tipo"))),
        ),
        renderResult(data) { customers =>
          if customers.isEmpty then div(cls := "p-4", emptyState("Nessun cliente registrato."))
          else
            searchedPages(sort.sorted(customers), query.signal, ListSearch.customer, pager, "Nessun cliente corrisponde alla ricerca.") { shown =>
              div(
                // Mobile: stacked cards. Desktop (md+): the table.
                div(cls := "divide-y divide-slate-100 md:hidden", shown.map(renderCard)),
                div(
                  cls := "hidden overflow-x-auto md:block",
                  table(
                    cls := "w-full text-sm",
                    thead(
                      cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                      tr(
                        sort.header("customer", "Cliente", "px-4 py-2"),
                        sort.header("contacts", "Contatti", "px-4 py-2"),
                        sort.header("boat", "Barca", "px-4 py-2"),
                        sort.header("type", "Tipo", "px-4 py-2"),
                        th(cls := "px-4 py-2"),
                      ),
                    ),
                    tbody(shown.map(renderRow)),
                  ),
                ),
              )
            }
        },
      ),
      confirmDeleteModal,
      draft.dialog,
    )
  end apply
end CustomersPage
