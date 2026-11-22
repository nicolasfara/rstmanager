package io.gitbub.nicolasfara.rstmanager.ui

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messageFor, FieldIssue }

/** What the customer form page shows: a blank new customer, or the stored customer the user clicked "Modifica" on. */
enum CustomerTarget derives CanEqual:
  case New
  case Edit(customer: CustomerResponse)

/**
 * The "Nuovo cliente" / "Modifica cliente «X»" page, shown in place of the customer list: one section per group of fields, then the sticky action
 * bar. `customers` is the list's load, so an edited customer deleted meanwhile (by another user) blocks the save; `onExit` goes back to the list.
 */
object CustomerFormView:

  private val submitErrorId = "cust-submit-error"

  def apply(target: CustomerTarget, customers: Signal[Option[ApiClient.Result[List[CustomerResponse]]]], onExit: () => Unit): HtmlElement =
    val initial = target match
      case CustomerTarget.New => CustomerForm.empty
      case CustomerTarget.Edit(customer) => CustomerForm.fromResponse(customer)
    val editingName = Option.when(initial.editingId.isDefined)(initial.displayName)
    val form = Var(initial)
    val touch = FormTouch()
    val submitting = Var(false)
    val formError = Var(Option.empty[ApiError])
    val gone = Var(false)
    // Once the record is gone the draft cannot be saved, so it no longer counts as unsaved work (`None`): leaving does not ask.
    val draft = DraftGuard[Option[CustomerForm.State]](
      () => Option.unless(gone.now())(form.now()),
      _ => DraftGuard.discardTitle(editingName, "il nuovo cliente"),
    )
    val leave = () => draft.discardThen(onExit)
    val isDirty = form.signal.combineWith(gone.signal).map(_ => draft.isDirtyNow)

    // Always clickable: an invalid form reveals every error and focuses the first one instead of sending; re-entrant clicks are ignored. Saving an
    // untouched existing customer just goes back.
    def submit(): Unit =
      val current = form.now()
      if !submitting.now() && !gone.now() then
        if current.editingId.isDefined && !draft.isDirtyNow then onExit()
        else
          current.issues.headOption match
            case Some(first) =>
              touch.attemptSubmit()
              FormFocus.focus(CustomerForm.fieldId(first.field))
            case None =>
              submitting.set(true)
              formError.set(None)
              val effect = current.editingId match
                case Some(id) => ApiClient.updateCustomer(id, current.request)
                case None => ApiClient.createCustomer(current.request)
              effect.foreach { result =>
                submitting.set(false)
                result match
                  case Right(_) =>
                    Notifications.success(s"Cliente ${current.displayName} ${if current.editingId.isDefined then "salvato" else "creato"}")
                    AppBus.mutatedCustomers()
                    onExit()
                  case Left(err) =>
                    showError(formError, "Salvataggio cliente")(err)
                    FormFocus.focus(submitErrorId)
              }
      end if
    end submit

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

    def wide(el: HtmlElement): HtmlElement = el.amend(cls := "sm:col-span-2")
    def grid(content: Modifier[HtmlElement]*): HtmlElement = div(cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2", content)

    val anagrafica = formSection("Anagrafica", "")(
      grid(
        wide(
          Components.field(FieldSpec("Tipo", "cust-type"))(bind =>
            segmented(
              form.signal.map(_.customerType),
              Observer[String](v => form.update(_.copy(customerType = v))),
              CustomerForm.typeOptions,
              List(bind, aria.label := "Tipo cliente"),
            ),
          ),
        ),
        // The business name is optional on the server: shown for a company (or when a stored individual still has one, so it is never hidden).
        child.maybe <-- form.signal.map(s => s.isCompany || s.businessName.trim.nn.nonEmpty).distinct.map { shown =>
          Option.when(shown)(
            wide(
              textField(
                FieldSpec("Ragione sociale", "cust-business-name", hintText = Some("Nome e cognome sono quelli del referente.")),
                _.businessName,
                (s, v) => s.copy(businessName = v),
                "Cantiere Rossi Srl",
                "text",
              ),
            ),
          )
        },
        textField(checkedSpec("Nome", CustomerForm.Field.Name, required = true), _.name, (s, v) => s.copy(name = v), "", "text"),
        textField(checkedSpec("Cognome", CustomerForm.Field.Surname, required = true), _.surname, (s, v) => s.copy(surname = v), "", "text"),
      ),
    )

    val contatti = formSection("Contatti", "")(
      grid(
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

    val indirizzo = formSection("Indirizzo", "")(
      grid(
        wide(textField(plainSpec("Via", "cust-street"), _.street, (s, v) => s.copy(street = v), "Via Roma 1", "text")),
        textField(checkedSpec("CAP", CustomerForm.Field.PostalCode, required = false), _.postalCode, (s, v) => s.copy(postalCode = v), "", "text"),
        textField(plainSpec("Città", "cust-city"), _.city, (s, v) => s.copy(city = v), "", "text"),
        textField(plainSpec("Paese", "cust-country"), _.country, (s, v) => s.copy(country = v), "IT", "text"),
      ),
    )

    // The label (and the format checked) follow the customer type; rebuilt only when the type changes, not on every keystroke.
    val datiFiscali = formSection("Dati fiscali", "")(
      grid(
        child <-- form.signal.map(_.fiscalCodeLabel).distinct.map { labelText =>
          textField(
            checkedSpec(labelText, CustomerForm.Field.FiscalCode, required = false),
            _.fiscalCode,
            (s, v) => s.copy(fiscalCode = v),
            "",
            "text",
          )
        },
      ),
    )

    val imbarcazione = formSection("Imbarcazione", "")(
      grid(
        textField(plainSpec("Nome barca", "cust-boat-name"), _.boatName, (s, v) => s.copy(boatName = v), "", "text"),
        textField(plainSpec("Modello", "cust-boat-model"), _.boatModel, (s, v) => s.copy(boatModel = v), "", "text"),
        textField(plainSpec("Porto", "cust-port"), _.port, (s, v) => s.copy(port = v), "", "text"),
        textField(plainSpec("Posto barca", "cust-berth"), _.boatBerth, (s, v) => s.copy(boatBerth = v), "", "text"),
      ),
    )

    val note = formSection("Note", "")(
      Components.field(FieldSpec("Note generiche", "cust-notes"))(bind =>
        textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "").amend(bind),
      ),
      RegistryPage.submitError(formError.signal, submitErrorId),
    )

    val goneReason = editingName.fold("")(name => s"Il cliente «$name» non esiste più")

    formPage(
      PageHeader(
        "← Torna ai clienti",
        leave,
        Signal.fromValue(editingName.fold("Nuovo cliente")(name => s"Modifica cliente «$name»")),
        context = "Clienti" :: editingName.toList,
        notes = child.maybe <-- gone.signal.map(
          Option.when(_)(
            warningNote(s"$goneReason: potrebbe essere stato eliminato da un altro utente. Le modifiche non possono essere salvate."),
          ),
        ),
        initialFocus = if editingName.isDefined then PageFocus.Title else PageFocus.FirstField,
      ),
    )(
      draft.bind,
      draft.dialog,
      DraftGuard.trackGone(customers, initial.editingId, _.id, gone)(() => draft.markClean()),
      formColumn(
        anagrafica,
        contatti,
        indirizzo,
        datiFiscali,
        imbarcazione,
        note,
        stickyActionBar(
          dirtyNote(isDirty),
          formActionsInline(
            if editingName.isDefined then "Salva modifiche" else "Crea cliente",
            if editingName.isDefined then "Salvataggio…" else "Creazione…",
            submitting.signal,
            gone.signal.map(Option.when(_)(goneReason)),
            Some(leave),
            () => submit(),
          ),
        ),
      ),
    )
  end apply
end CustomerFormView
