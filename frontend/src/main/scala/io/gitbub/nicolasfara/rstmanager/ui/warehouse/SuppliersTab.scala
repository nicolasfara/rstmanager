package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.AppBus
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** Supplier registry of the warehouse: the suppliers the material MPNs are bought from. Same layout as the customer registry. */
object SuppliersTab:

  def apply(): HtmlElement =
    val formError = Var(Option.empty[ApiError])
    val form = Var(SupplierForm.empty)
    val pendingDelete = Var(Option.empty[SupplierResponse])

    def resetForm(): Unit =
      form.set(SupplierForm.empty)
      formError.set(None)

    def edit(s: SupplierResponse): Unit =
      form.set(SupplierForm.fromResponse(s))
      formError.set(None)

    def submit(): Unit =
      val current = form.now()
      val effect = current.editingId match
        case Some(id) => ApiClient.updateSupplier(id, current.request)
        case None => ApiClient.createSupplier(current.request)
      effect.foreach {
        case Right(_) => resetForm(); AppBus.mutatedWarehouse()
        case Left(err) => showError(formError, "Salvataggio fornitore")(err)
      }

    // A supplier referenced by a material is refused with 409 `supplier-in-use`; the error details name the materials.
    def delete(id: UUID): Unit =
      ApiClient.deleteSupplier(id).foreach {
        case Right(_) =>
          if form.now().editingId == Some(id) then resetForm()
          AppBus.mutatedWarehouse()
        case Left(err) => showError(formError, "Eliminazione fornitore")(err)
      }

    val confirmDeleteModal =
      confirmDialog(
        isOpen = pendingDelete.signal.map(_.isDefined),
        titleText = Signal.fromValue("Elimina fornitore"),
        message = pendingDelete.signal.map(
          _.fold("")(s => s"Eliminare il fornitore “${s.name}”? Non è possibile se un materiale di magazzino ne usa gli MPN."),
        ),
        confirmLabel = "Elimina",
        tone = DialogTone.Danger,
        onCancel = () => pendingDelete.set(None),
        onConfirm = () => pendingDelete.now().foreach { s => delete(s.id); pendingDelete.set(None) },
      )

    val formErrors: Signal[List[String]] = form.signal.map(_.errors)

    val data = loadable(AppBus.warehouseTicks)(() => ApiClient.listSuppliers())

    def textField(
        labelText: String,
        get: SupplierForm.State => String,
        set: (SupplierForm.State, String) => SupplierForm.State,
        hint: String,
    ): HtmlElement =
      field(labelText, textInput(form.signal.map(get), Observer[String](v => form.update(set(_, v))), hint))

    div(
      cls := "grid gap-6",
      roleGatedGridCols(Role.Admin, "lg:grid-cols-[24rem_1fr]"),
      roleGated(Role.Admin)(
        card(
          cls := "self-start p-4",
          sectionTitle("Nuovo fornitore"),
          div(
            cls := "mt-3 grid grid-cols-2 gap-3",
            div(cls := "col-span-2", textField("Ragione sociale", _.name, (s, v) => s.copy(name = v), "")),
            textField("Partita IVA", _.vatNumber, (s, v) => s.copy(vatNumber = v), "Opzionale"),
            textField("Telefono", _.phone, (s, v) => s.copy(phone = v), "Opzionale"),
            div(
              cls := "col-span-2",
              field("Email", textInput(form.signal.map(_.email), Observer[String](v => form.update(_.copy(email = v))), "Opzionale", "email")),
            ),
            div(cls := "col-span-2", textField("Indirizzo", _.address, (s, v) => s.copy(address = v), "Opzionale")),
            div(
              cls := "col-span-2",
              field(
                "Note",
                textAreaInput(form.signal.map(_.notes), Observer[String](v => form.update(_.copy(notes = v))), "Note generiche (opzionale)"),
              ),
            ),
          ),
          child.maybe <-- form.signal.combineWith(formErrors).map { (state, errors) =>
            Option.when(state.name.nonEmpty && errors.nonEmpty)(ul(cls := "mt-3 list-disc pl-5 text-xs text-rose-600", errors.map(e => li(e))))
          },
          child.maybe <-- formError.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
          div(
            cls := "mt-3 flex gap-2",
            button(
              tpe := "button",
              cls := btnPrimary,
              child.text <-- form.signal.map(_.editingId.fold("Crea")(_ => "Salva")),
              disabled <-- formErrors.map(_.nonEmpty),
              onClick --> (_ => submit()),
            ),
            child.maybe <-- form.signal.map(_.editingId.map(_ => button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => resetForm())))),
          ),
        ),
      ),
      card(
        cls := "overflow-hidden",
        renderResult(data) { suppliers =>
          if suppliers.isEmpty then emptyState("Nessun fornitore registrato.")
          else
            div(
              cls := "overflow-x-auto",
              table(
                cls := "w-full text-sm",
                thead(
                  cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                  tr(
                    th(cls := "px-4 py-2", "Fornitore"),
                    th(cls := "px-4 py-2", "Contatti"),
                    th(cls := "px-4 py-2", "Indirizzo"),
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
        },
      ),
      confirmDeleteModal,
    )
  end apply
end SuppliersTab
