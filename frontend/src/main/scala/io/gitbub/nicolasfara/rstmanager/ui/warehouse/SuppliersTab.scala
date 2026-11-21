package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, DraftGuard, FormFocus, FormTouch, ListSearch, Notifications, Pagination, Paging, TableSort }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.messageFor
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.Column

/** Supplier registry of the warehouse: the suppliers the material MPNs are bought from. Same layout as the customer registry. */
object SuppliersTab:

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val form = Var(SupplierForm.empty)
    val pendingDelete = Var(Option.empty[SupplierResponse])

    val touch = FormTouch()
    val submitting = Var(false)
    val draft = DraftGuard[SupplierForm.State](
      () => form.now(),
      s => DraftGuard.discardTitle(Option.when(s.editingId.isDefined)(s.name.trim.nn), "il nuovo fornitore"),
    )

    def resetForm(): Unit =
      form.set(SupplierForm.empty)
      formError.set(None)
      touch.reset()
      draft.markClean()

    val formAnchor = FormAnchor()

    // Replacing a dirty draft asks first; the form is revealed once the new content is loaded.
    def edit(s: SupplierResponse): Unit =
      draft.discardThen { () =>
        formAnchor.revealAndFocus()
        form.set(SupplierForm.fromResponse(s))
        formError.set(None)
        touch.reset()
        draft.markClean()
      }

    /** "+ Nuovo fornitore" (mobile, where the form follows the list): leaves a supplier being edited, then shows the form. */
    def startNew(): Unit =
      if form.now().editingId.isDefined then
        draft.discardThen(() =>
          resetForm(); formAnchor.revealAndFocus(),
        )
      else formAnchor.revealAndFocus()

    def submit(): Unit =
      if !submitting.now() then
        val current = form.now()
        current.issues.headOption match
          case Some(first) =>
            touch.attemptSubmit()
            FormFocus.focus(SupplierForm.fieldId(first.field))
          case None =>
            val request = current.request
            submitting.set(true)
            formError.set(None)
            val effect = current.editingId match
              case Some(id) => ApiClient.updateSupplier(id, request)
              case None => ApiClient.createSupplier(request)
            effect.foreach { result =>
              submitting.set(false)
              result match
                case Right(_) =>
                  resetForm()
                  Notifications.success(s"Fornitore ${request.name} ${if current.editingId.isDefined then "salvato" else "creato"}")
                  AppBus.mutatedWarehouse()
                case Left(err) => showError(formError, "Salvataggio fornitore")(err)
            }
        end match

    // A supplier referenced by a material is refused with 409 `supplier-in-use`; the error details name the materials.
    def delete(supplier: SupplierResponse): Unit =
      ApiClient.deleteSupplier(supplier.id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(supplier.id) then resetForm()
          Notifications.success(s"Fornitore ${supplier.name} eliminato")
          AppBus.mutatedWarehouse()
        case Left(err) => showError(formError, "Eliminazione fornitore")(err)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = pendingDelete.signal.map(_.fold("")(s => s"Eliminare il fornitore «${s.name}»?")),
        message = Signal.fromValue("L'operazione non è reversibile. Non è possibile se un materiale di magazzino ne usa gli MPN."),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { s => delete(s); pendingDelete.set(None) },
      )

    val data = loadable(AppBus.warehouseTicks)(() => ApiClient.listSuppliers())
    val query = Var("")
    val pager = Pagination(Paging.defaultPageSize)
    val sort = TableSort[SupplierResponse](pager)(
      "supplier" -> Column.text(_.name),
      "contacts" -> Column.optionalText(s => s.email.orElse(s.phone)),
      "address" -> Column.optionalText(_.address),
    )

    /** A text field of the form with DOM id `id`; its error (if it has a validated `target`) shows once left or after a submit attempt. */
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

    val editing = form.signal.map(_.editingId.isDefined).distinct

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[24rem_1fr]"),
      roleGated(Role.Admin)(
        card(
          // Mobile: the list comes first (finding a record must not mean scrolling past the form); "+ Nuovo fornitore" reaches the form.
          cls := "order-last self-start p-4 lg:order-none",
          formAnchor.bind,
          draft.bind,
          child <-- editing.map(isEditing => sectionTitle(if isEditing then "Modifica fornitore" else "Nuovo fornitore")),
          requiredLegend.amend(cls := "mt-1"),
          div(
            cls := "mt-3 grid grid-cols-2 gap-3",
            div(
              cls := "col-span-2",
              textField(
                "Ragione sociale",
                SupplierForm.fieldId(SupplierForm.Field.Name),
                Some(SupplierForm.Field.Name),
                _.name,
                (s, v) => s.copy(name = v),
                "",
                "text",
              ),
            ),
            textField("Partita IVA", "supplier-vat", None, _.vatNumber, (s, v) => s.copy(vatNumber = v), "01234567890", "text"),
            textField("Telefono", "supplier-phone", None, _.phone, (s, v) => s.copy(phone = v), "+39051123456", "tel"),
            div(
              cls := "col-span-2",
              textField(
                "Email",
                SupplierForm.fieldId(SupplierForm.Field.Email),
                Some(SupplierForm.Field.Email),
                _.email,
                (s, v) => s.copy(email = v),
                "ordini@fornitore.it",
                "email",
              ),
            ),
            div(
              cls := "col-span-2",
              textField("Indirizzo", "supplier-address", None, _.address, (s, v) => s.copy(address = v), "Via Roma 1, Bologna", "text"),
            ),
            div(
              cls := "col-span-2",
              field(FieldSpec("Note", "supplier-notes"))(bind =>
                textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "Note generiche")
                  .amend(bind),
              ),
            ),
          ),
          errorList(touch.submittedSignal.combineWith(form.signal).map((attempted, state) => if attempted then state.errors else Nil)),
          child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
          div(
            cls := "mt-3",
            child <-- editing.map(isEditing =>
              formActions(
                if isEditing then "Salva" else "Crea",
                "Salvataggio…",
                submitting.signal,
                Option.when(isEditing)(() => draft.discardThen(() => resetForm())),
                () => submit(),
              ),
            ),
          ),
        ),
      ),
      card(
        cls := "overflow-hidden",
        div(
          cls := "flex items-center gap-2 border-b border-slate-100 p-3",
          div(cls := "min-w-0 flex-1", searchBox(query, "Cerca per nome, P. IVA, contatti…", pager)),
          roleGated(Role.Admin)(
            button(tpe := "button", cls := s"$btnSmall min-h-10 shrink-0 lg:hidden", "+ Nuovo fornitore", onClick --> (_ => startNew())),
          ),
        ),
        renderResult(data) { all =>
          if all.isEmpty then div(cls := "p-4", emptyState("Nessun fornitore registrato."))
          else
            searchedPages(sort.sorted(all), query.signal, ListSearch.supplier, pager, "Nessun fornitore corrisponde alla ricerca.") { suppliers =>
              div(
                cls := "overflow-x-auto",
                table(
                  cls := "w-full text-sm",
                  thead(
                    cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                    tr(
                      sort.header("supplier", "Fornitore", "px-4 py-2"),
                      sort.header("contacts", "Contatti", "px-4 py-2"),
                      sort.header("address", "Indirizzo", "px-4 py-2"),
                      th(cls := "px-4 py-2"),
                    ),
                  ),
                  tbody(
                    suppliers.map { s =>
                      tr(
                        cls := "border-b border-slate-100 align-top last:border-0",
                        td(
                          cls := "px-4 py-2",
                          div(cls := "font-medium text-slate-800", s.name),
                          s.vatNumber.map(vat => div(cls := "text-xs text-slate-500", s"P. IVA $vat")),
                          s.notes.map(n => div(cls := "text-xs text-slate-400", n)),
                        ),
                        td(
                          cls := "px-4 py-2 text-slate-500",
                          s.email.map(e => div(e)),
                          s.phone.map(p => div(cls := "text-xs", p)),
                          Option.when(s.email.isEmpty && s.phone.isEmpty)(div(cls := "text-xs text-slate-400", "—")),
                        ),
                        td(cls := "px-4 py-2 text-slate-500", s.address.getOrElse("—")),
                        td(
                          cls := "px-4 py-2 text-right",
                          roleGated(Role.Admin)(
                            div(
                              cls := "flex justify-end gap-2",
                              button(tpe := "button", cls := btnSmall, "Modifica", onClick --> (_ => edit(s))),
                              button(tpe := "button", cls := btnDanger, "Elimina", onClick --> (_ => pendingDelete.set(Some(s)))),
                            ),
                          ),
                        ),
                      )
                    },
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
end SuppliersTab
