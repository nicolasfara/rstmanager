package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, DirtyTracker, DraftGuard, FormFocus, FormTouch, NavigationGuard, Notifications }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.messageFor

/**
 * The supplier form page, shown in place of the supplier list (see [[SuppliersTab]]): the same page creates a supplier and changes one. Three
 * sections ("Dati fornitore", "Contatti", "Note") in one column, and a sticky action bar with the unsaved-changes note.
 *
 * Rules live in the pure [[SupplierForm]]. Errors show under their input once it was left or after a submit attempt ([[FormTouch]]); an invalid
 * submit focuses the first one. Leaving ("← Torna ai fornitori", "Annulla", the browser back button, the top navigation, a tab switch) asks first
 * while the draft is dirty. Saving an unchanged supplier goes back without a request.
 */
object SupplierFormView:

  /** What the page is open for: a new supplier, or a change of `supplier`. */
  enum Target derives CanEqual:
    case New
    case Edit(supplier: SupplierResponse)

  /** The DOM id of the banner of a refused save (focused when it appears). */
  private val submitErrorId = "supplier-submit-error"

  /**
   * The page of `target`, mounted fresh on every opening. `onExit` goes back to the list: after a successful save (toast and [[AppBus]] tick done),
   * or on a confirmed leave. `gone` turns true once a reload of the registry no longer lists the edited supplier: the page then says so, cannot be
   * saved, and leaves without asking.
   */
  def apply(target: Target, onExit: () => Unit, gone: Signal[Boolean]): HtmlElement =
    val initial = target match
      case Target.New => SupplierForm.empty
      case Target.Edit(supplier) => SupplierForm.fromResponse(supplier)
    val form = Var(initial)
    val tracker = DirtyTracker(initial, form)
    val (leaveTitle, leaveMessage) = target match
      case Target.New => ("Abbandonare il nuovo fornitore?", "Il fornitore non è stato creato: i dati inseriti andranno persi.")
      case Target.Edit(supplier) => (DraftGuard.discardTitle(Some(supplier.name), ""), "Le modifiche non salvate andranno perse.")
    val draft = DraftGuard[SupplierForm.State](() => form.now(), _ => leaveTitle)
    // Sticky for this opening: once a reload no longer listed the supplier, a later failed reload does not make it savable again.
    val isGone = Var(false)
    val leave: () => Unit = () => if isGone.now() then onExit() else draft.discardThen(onExit)
    val touch = FormTouch()
    val submitting = Var(false)
    val error = Var(Option.empty[ApiError])
    val blocked: Signal[Option[String]] = isGone.signal.map(gone => Option.when(gone)("Fornitore eliminato: le modifiche non si possono salvare."))

    def submit(): Unit =
      if !submitting.now() && !isGone.now() then
        val current = form.now()
        current.issues.headOption match
          case Some(first) =>
            touch.attemptSubmit()
            FormFocus.focus(SupplierForm.fieldId(first.field))
          case None if current.editingId.isDefined && !tracker.isDirtyNow => onExit()
          case None =>
            val request = current.request
            submitting.set(true)
            error.set(None)
            val effect = current.editingId match
              case Some(id) => ApiClient.updateSupplier(id, request)
              case None => ApiClient.createSupplier(request)
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(_) =>
                  touch.reset()
                  Notifications.success(s"Fornitore ${request.name} ${if current.editingId.isDefined then "salvato" else "creato"}")
                  AppBus.mutatedWarehouse()
                  onExit()
                case Left(err) =>
                  showError(error, "Salvataggio fornitore")(err)
                  FormFocus.focus(submitErrorId)
            }
        end match

    /** A text field with DOM id `id`; its error (if it has a validated `target`) shows once left or after a submit attempt. */
    def textField(
        labelText: String,
        id: String,
        target: Option[SupplierForm.Field],
        get: SupplierForm.State => String,
        set: (SupplierForm.State, String) => SupplierForm.State,
        placeholderText: String,
        inputType: String,
    ): HtmlElement =
      val spec = target.fold(FieldSpec(labelText, id))(validated =>
        touch.spec(labelText, id, validated == SupplierForm.Field.Name, form.signal.map(_.issues.messageFor(validated)).distinct),
      )
      field(spec)(bind => textInput(form.signal.map(get), Observer[String](v => form.update(set(_, v))), placeholderText, inputType).amend(bind))

    val (titleText, contextTrail, focus, submitLabel) = target match
      case Target.New => ("Nuovo fornitore", List("Magazzino", "Fornitori"), PageFocus.FirstField, "Crea fornitore")
      case Target.Edit(supplier) =>
        (s"Modifica fornitore «${supplier.name}»", List("Magazzino", "Fornitori", supplier.name), PageFocus.Title, "Salva modifiche")
    val busyLabel = if initial.editingId.isDefined then "Salvataggio…" else "Creazione…"
    val goneNote: Modifier[HtmlElement] = target match
      case Target.New => emptyMod
      case Target.Edit(supplier) =>
        child.maybe <-- isGone.signal.map(gone =>
          Option.when(gone)(
            warningNote(
              s"Il fornitore «${supplier.name}» non esiste più: potrebbe essere stato eliminato da un altro utente. Le modifiche non possono essere salvate.",
            ),
          ),
        )

    val grid = "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2"

    formPage(PageHeader("← Torna ai fornitori", leave, Signal.fromValue(titleText), contextTrail, notes = goneNote, initialFocus = focus))(
      gone --> (removed => if removed then isGone.set(true)),
      // Leaving the page (top navigation, a tab switch, closing the tab) asks while dirty, except once the supplier cannot be saved anyway.
      NavigationGuard.guard(leaveTitle, leaveMessage, () => draft.isDirtyNow && !isGone.now()),
      draft.dialog,
      formColumn(
        formSection("Dati fornitore", "Ragione sociale e dati fiscali.")(
          div(
            cls := grid,
            textField(
              "Ragione sociale",
              SupplierForm.fieldId(SupplierForm.Field.Name),
              Some(SupplierForm.Field.Name),
              _.name,
              (s, v) => s.copy(name = v),
              "Es. Tessiture Rossi Srl",
              "text",
            ),
            textField("Partita IVA", "supplier-vat", None, _.vatNumber, (s, v) => s.copy(vatNumber = v), "01234567890", "text"),
          ),
        ),
        formSection("Contatti", "Come raggiungere il fornitore per gli ordini.")(
          div(
            cls := grid,
            textField("Telefono", "supplier-phone", None, _.phone, (s, v) => s.copy(phone = v), "+39051123456", "tel"),
            textField(
              "Email",
              SupplierForm.fieldId(SupplierForm.Field.Email),
              Some(SupplierForm.Field.Email),
              _.email,
              (s, v) => s.copy(email = v),
              "ordini@fornitore.it",
              "email",
            ),
            div(
              cls := "sm:col-span-2",
              textField("Indirizzo", "supplier-address", None, _.address, (s, v) => s.copy(address = v), "Via Roma 1, Bologna", "text"),
            ),
          ),
        ),
        formSection("Note", "")(
          field(FieldSpec("Note", "supplier-notes"))(bind =>
            textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "Note generiche").amend(bind),
          ),
          child.maybe <-- error.signal.map(_.map(err => div(idAttr := submitErrorId, errorBanner(err)))),
        ),
        stickyActionBar(
          dirtyNote(tracker.isDirty),
          formActionsInline(submitLabel, busyLabel, submitting.signal, blocked, Some(leave), () => submit()),
        ),
      ),
    )
  end apply
end SupplierFormView
