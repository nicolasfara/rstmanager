package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.auth.Role
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, WarehouseFormats }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*

/** What the stock dialog opened from the materials table is about. */
enum StockDialog derives CanEqual:
  case Purchase(material: MaterialResponse)
  case Unload(material: MaterialResponse)
  case Adjust(material: MaterialResponse)
  case Movements(material: MaterialResponse)

/**
 * The stock dialogs of a material: the purchase ("Carico"), unload ("Scarico"), and adjustment ("Rettifica") forms (Operator), and the movements list
 * with the running balance and the void action (Admin). The list also shows the automatic consumptions by order manufacturings ("Scarico
 * lavorazione", and "Storno lavorazione" when a reopened manufacturing gives the material back) with their order, which cannot be voided.
 *
 * Driven by one `Var[Option[StockDialog]]` owned by the materials tab: `Some` opens the matching modal, `None` closes it. The forms are rebuilt on
 * every opening (the state goes through `None` in between), so a form never shows the values of a previous one. Every successful write calls
 * [[AppBus.mutatedWarehouse]], which refreshes the materials table, the open movements list, and the reports.
 */
object StockDialogs:

  private val disabledInputCls = "disabled:bg-slate-100 disabled:text-slate-400"

  def apply(current: Var[Option[StockDialog]]): HtmlElement =
    val close: () => Unit = () => current.set(None)
    val formDialog: Signal[Option[StockDialog]] = current.signal.map(_.filter {
      case StockDialog.Movements(_) => false
      case _ => true
    })
    val movementsOf: Signal[Option[MaterialResponse]] = current.signal.map(_.collect { case StockDialog.Movements(material) => material })
    // `contents`: the wrapper must not take a cell of the tab grid; the modals are fixed overlays anyway.
    div(
      cls := "contents",
      modal(formDialog.map(_.isDefined), formDialog.map(_.fold("")(formTitle)), "max-w-lg", close)(
        div(child.maybe <-- formDialog.distinct.map(_.map(renderForm(_, close)))),
      ),
      modal(movementsOf.map(_.isDefined), movementsOf.map(_.fold("")(p => s"Movimenti — ${p.internalCode}")), "max-w-6xl", close)(
        div(child.maybe <-- movementsOf.distinct.map(_.map(movementsView))),
      ),
    )
  end apply

  private def formTitle(dialog: StockDialog): String = dialog match
    case StockDialog.Purchase(p) => s"Carico — ${p.internalCode}"
    case StockDialog.Unload(p) => s"Scarico — ${p.internalCode}"
    case StockDialog.Adjust(p) => s"Rettifica — ${p.internalCode}"
    case StockDialog.Movements(p) => s"Movimenti — ${p.internalCode}"

  private def renderForm(dialog: StockDialog, close: () => Unit): HtmlElement = dialog match
    case StockDialog.Purchase(p) => purchaseForm(p, close)
    case StockDialog.Unload(p) => unloadForm(p, close)
    case StockDialog.Adjust(p) => adjustmentForm(p, close)
    case StockDialog.Movements(p) => movementsView(p)

  // ---- Shared pieces -----------------------------------------------------------------------------

  private def materialHeader(material: MaterialResponse): HtmlElement =
    div(
      cls := "mb-4 rounded-md bg-slate-50 px-3 py-2 text-xs text-slate-600",
      span(cls := "font-medium text-slate-800", material.internalCode),
      s" — ${material.description}",
      div(
        cls := "mt-0.5",
        "Giacenza attuale: ",
        span(
          cls := (if material.stock.negative then "font-medium text-rose-700" else "font-medium text-slate-800"),
          WarehouseFormats.formatQty(material.stock.quantity, material.unit),
        ),
      ),
    )

  private def dateInput(valueSignal: Signal[String], writer: Observer[String], today: String): HtmlElement =
    textInput(valueSignal, writer, "", "date").amend(maxAttr := today)

  private def decimalInput(valueSignal: Signal[String], writer: Observer[String], placeholderText: String): HtmlElement =
    textInput(valueSignal, writer, placeholderText).amend(inputMode := "decimal")

  private def hint(text: String): HtmlElement = div(cls := "mt-1 text-xs text-slate-400", text)

  /** The validation messages, hidden while the form is still untouched. */
  private def errorList(errors: Signal[List[String]]): Modifier[HtmlElement] =
    child.maybe <-- errors.map(list => Option.when(list.nonEmpty)(ul(cls := "mt-3 list-disc pl-5 text-xs text-rose-600", list.map(e => li(e)))))

  private def formActions(submitLabel: String, canSubmit: Signal[Boolean], submitting: Signal[Boolean], close: () => Unit, submit: () => Unit) =
    div(
      cls := "mt-4 flex justify-end gap-2",
      button(tpe := "button", cls := btnGhost, "Annulla", onClick --> (_ => close())),
      button(
        tpe := "button",
        cls := btnPrimary,
        child.text <-- submitting.map(busy => if busy then "Registrazione…" else submitLabel),
        disabled <-- canSubmit.combineWith(submitting).map((ok, busy) => !ok || busy),
        onClick --> (_ => submit()),
      ),
    )

  /** Sends a movement; on success refreshes the warehouse and closes the dialog, on failure shows the server message in the dialog. */
  private def record(
      effect: () => Future[ApiClient.Result[MovementRecordedResponse]],
      submitting: Var[Boolean],
      error: Var[Option[ApiError]],
      context: String,
      close: () => Unit,
  ): Unit =
    if !submitting.now() then
      submitting.set(true)
      error.set(None)
      effect().foreach { result =>
        submitting.set(false)
        result match
          case Right(_) =>
            AppBus.mutatedWarehouse()
            close()
          case Left(err) => showError(error, context)(err)
      }

  private def piecesHint(unit: String): Modifier[HtmlElement] =
    Option.when(WarehouseFormats.MeasureUnit.fromWire(unit).exists(!_.allowsFractions))(hint("Gestito a pezzi: solo quantità intere"))

  // ---- Purchase ----------------------------------------------------------------------------------

  private def purchaseForm(material: MaterialResponse, close: () => Unit): HtmlElement =
    val today = WarehousePage.today()
    val state = Var(MovementForm.purchase(material, today))
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    val unitLabel = WarehouseFormats.unitShortLabel(material.unit)
    val mpnChoices = ("" -> "Seleziona un MPN…") :: state.now().mpns.map(o => o.key -> o.label)
    def submit(): Unit =
      state.now().request.foreach(request => record(() => ApiClient.recordPurchase(material.id, request), submitting, error, "Carico", close))
    div(
      materialHeader(material),
      div(
        cls := "grid gap-3 sm:grid-cols-2",
        div(
          cls := "sm:col-span-2",
          field(
            "MPN acquistato (fornitore)",
            if state.now().mpns.isEmpty then div(cls := "text-xs text-rose-600", "Il materiale non ha MPN: aggiungine uno in anagrafica.")
            else staticSelect(state.signal.map(_.mpnKey), Observer[String](v => state.update(_.copy(mpnKey = v))), mpnChoices),
          ),
        ),
        field(
          s"Quantità ($unitLabel)",
          div(
            decimalInput(state.signal.map(_.quantity), Observer[String](v => state.update(_.copy(quantity = v))), "25,5"),
            piecesHint(material.unit),
          ),
        ),
        field("Costo unitario (€)", decimalInput(state.signal.map(_.unitCost), Observer[String](v => state.update(_.copy(unitCost = v))), "3,50")),
        field("Data", dateInput(state.signal.map(_.on), Observer[String](v => state.update(_.copy(on = v))), today)),
        field("Note", textInput(state.signal.map(_.note), Observer[String](v => state.update(_.copy(note = v))), "Es. DDT 1234 (opzionale)")),
      ),
      div(
        cls := "mt-3 flex items-baseline justify-between rounded-md border border-slate-200 px-3 py-2 text-sm",
        span(cls := "text-slate-500", "Totale carico"),
        span(
          cls := "font-semibold tabular-nums text-slate-800",
          child.text <-- state.signal.map(_.totalPreview.fold("—")(WarehouseFormats.formatEuro)),
        ),
      ),
      errorList(state.signal.map(s => if s.pristine then Nil else s.errors)),
      child.maybe <-- error.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      formActions("Registra carico", state.signal.map(_.request.isDefined), submitting.signal, close, () => submit()),
    )
  end purchaseForm

  // ---- Unload ------------------------------------------------------------------------------------

  private def unloadForm(material: MaterialResponse, close: () => Unit): HtmlElement =
    val today = WarehousePage.today()
    val state = Var(MovementForm.unload(material, today))
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    def submit(): Unit =
      state.now().request.foreach(request => record(() => ApiClient.recordUnload(material.id, request), submitting, error, "Scarico", close))
    div(
      materialHeader(material),
      div(
        cls := "grid gap-3 sm:grid-cols-2",
        field(
          s"Quantità (${WarehouseFormats.unitShortLabel(material.unit)})",
          div(decimalInput(state.signal.map(_.quantity), Observer[String](v => state.update(_.copy(quantity = v))), "2"), piecesHint(material.unit)),
        ),
        field("Data", dateInput(state.signal.map(_.on), Observer[String](v => state.update(_.copy(on = v))), today)),
        div(
          cls := "sm:col-span-2",
          field(
            "Motivo",
            textInput(state.signal.map(_.reason), Observer[String](v => state.update(_.copy(reason = v))), "Es. campionatura per cliente"),
          ),
        ),
      ),
      hint("Lo scarico è rifiutato se la giacenza scenderebbe sotto zero in quella data o in seguito."),
      errorList(state.signal.map(s => if s.pristine then Nil else s.errors)),
      child.maybe <-- error.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      formActions("Registra scarico", state.signal.map(_.request.isDefined), submitting.signal, close, () => submit()),
    )
  end unloadForm

  // ---- Adjustment --------------------------------------------------------------------------------

  private def adjustmentForm(material: MaterialResponse, close: () => Unit): HtmlElement =
    val today = WarehousePage.today()
    val state = Var(MovementForm.adjustment(material, today))
    val error = Var(Option.empty[ApiError])
    val submitting = Var(false)
    def submit(): Unit =
      state.now().request.foreach(request => record(() => ApiClient.recordAdjustment(material.id, request), submitting, error, "Rettifica", close))
    div(
      materialHeader(material),
      div(
        cls := "grid gap-3 sm:grid-cols-2",
        field(
          s"Variazione (${WarehouseFormats.unitShortLabel(material.unit)})",
          div(
            textInput(state.signal.map(_.delta), Observer[String](v => state.update(_.copy(delta = v))), "-2 oppure 10"),
            hint("Negativa per togliere, positiva per aggiungere"),
            piecesHint(material.unit),
          ),
        ),
        field(
          "Costo unitario (€)",
          div(
            decimalInput(state.signal.map(_.unitCost), Observer[String](v => state.update(_.copy(unitCost = v))), "Opzionale")
              .amend(cls := disabledInputCls, disabled <-- state.signal.map(!_.costEnabled)),
            hint("Solo per rettifiche in aumento: vale come prezzo d'acquisto"),
          ),
        ),
        field("Data", dateInput(state.signal.map(_.on), Observer[String](v => state.update(_.copy(on = v))), today)),
        field("Motivo", textInput(state.signal.map(_.reason), Observer[String](v => state.update(_.copy(reason = v))), "Es. inventario")),
      ),
      errorList(state.signal.map(s => if s.pristine then Nil else s.errors)),
      child.maybe <-- error.signal.map(_.map(e => div(cls := "mt-3", errorBanner(e)))),
      formActions("Registra rettifica", state.signal.map(_.request.isDefined), submitting.signal, close, () => submit()),
    )
  end adjustmentForm

  // ---- Movements ---------------------------------------------------------------------------------

  private def movementsView(material: MaterialResponse): HtmlElement =
    val today = WarehousePage.today()
    val from = Var("")
    val to = Var("")
    val actionError = Var(Option.empty[ApiError])
    val pendingVoid = Var(Option.empty[MovementResponse])
    val voidReason = Var("")
    val range = from.signal.combineWith(to.signal).map(WarehouseViews.validateOptionalRange).distinct
    // The last valid filter: an invalid one shows a hint and keeps the list of the previous filter.
    val applied = Var((Option.empty[String], Option.empty[String]))
    val movements =
      loadable(AppBus.warehouseTicks.combineWith(applied.signal.distinct))(() =>
        ApiClient.listMovements(material.id, applied.now()._1, applied.now()._2),
      )

    def cancelVoid(): Unit =
      pendingVoid.set(None)
      voidReason.set("")

    def confirmVoid(): Unit =
      val reason = voidReason.now().trim.nn
      pendingVoid.now().filter(_ => reason.nonEmpty).foreach { movement =>
        cancelVoid()
        actionError.set(None)
        ApiClient.voidMovement(material.id, movement.id, reason).foreach {
          case Right(_) => AppBus.mutatedWarehouse()
          case Left(err) => showError(actionError, "Annullo movimento")(err)
        }
      }

    val voidDialog =
      confirmDialog(
        isOpen = pendingVoid.signal.map(_.isDefined),
        titleText = Signal.fromValue("Annulla movimento"),
        message = pendingVoid.signal.map(_.fold("") { m =>
          s"Annullare il movimento “${WarehouseViews.movementKindLabel(m)}” del ${IsoDay.display(m.on)} " +
            s"(${WarehouseViews.signedQty(m.delta, material.unit)})? Resta nello storico, ma non conta più nella giacenza."
        }),
        confirmLabel = "Annulla movimento",
        tone = DialogTone.Danger,
        body = div(cls := "mt-4 text-left", field("Motivo dell'annullo", textInput(voidReason, "Es. carico registrato due volte"))),
        confirmEnabled = voidReason.signal.map(_.trim.nn.nonEmpty),
        onCancel = () => cancelVoid(),
        onConfirm = () => confirmVoid(),
      )

    def balanceBox(label: String, value: String): HtmlElement =
      div(
        cls := "rounded-md border border-slate-200 px-3 py-2",
        div(cls := "text-xs text-slate-500", label),
        div(
          cls := (if WarehouseViews.signOf(value) < 0 then "font-semibold tabular-nums text-rose-700"
                  else "font-semibold tabular-nums text-slate-800"),
          WarehouseFormats.formatQty(value, material.unit),
        ),
      )

    def renderMovement(m: MovementResponse): HtmlElement =
      val struck = if m.voided then "line-through text-slate-400" else ""
      val qtyCls =
        if m.voided then ""
        else if WarehouseViews.signOf(m.delta) > 0 then "text-emerald-700"
        else if WarehouseViews.signOf(m.delta) < 0 then "text-rose-700"
        else ""
      tr(
        cls := "border-b border-slate-100 align-top last:border-0",
        cls := (if m.voided then "bg-slate-50" else ""),
        td(cls := s"px-3 py-2 whitespace-nowrap $struck", IsoDay.display(m.on)),
        td(cls := s"px-3 py-2 $struck", WarehouseViews.movementKindLabel(m)),
        td(
          cls := s"px-3 py-2 $struck",
          WarehouseViews
            .consumptionSource(m)
            .fold[Modifier[HtmlElement]](
              m.supplier
                .fold[Modifier[HtmlElement]]("—")(s => div(s.name, Option.when(!s.active)(span(cls := "text-xs text-slate-400", " (eliminato)")))),
            )(source => div(cls := "text-slate-700", source)),
          m.mpnCode.map(code => div(cls := "text-xs text-slate-400", code)),
        ),
        td(
          cls := s"px-3 py-2 whitespace-nowrap text-right font-medium tabular-nums $qtyCls $struck",
          WarehouseViews.signedQty(m.delta, material.unit),
        ),
        td(cls := s"px-3 py-2 whitespace-nowrap text-right tabular-nums $struck", m.unitCost.fold("—")(WarehouseFormats.formatUnitPrice)),
        td(cls := s"px-3 py-2 whitespace-nowrap text-right tabular-nums $struck", m.value.fold("—")(WarehouseFormats.formatEuro)),
        td(
          cls := "px-3 py-2 whitespace-nowrap text-right tabular-nums",
          cls := (if WarehouseViews.signOf(m.balance) < 0 then "text-rose-700" else "text-slate-700"),
          WarehouseFormats.formatQty(m.balance, material.unit),
        ),
        td(
          cls := "px-3 py-2 text-xs",
          m.reason.map(r => div(cls := (if m.voided then struck else "text-slate-600"), r)),
          m.note.map(n => div(cls := (if m.voided then struck else "text-slate-400"), n)),
          Option.when(m.voided)(
            div(
              cls := "mt-1 text-rose-700",
              badge(m.voidedOn.fold("Annullato")(day => s"Annullato il ${IsoDay.display(day)}"), "bg-rose-50 text-rose-700 border-rose-200"),
              m.voidReason.map(r => div(cls := "mt-0.5", r)),
            ),
          ),
        ),
        td(
          cls := "px-3 py-2 text-right",
          // A consumption follows its order manufacturing (reopening it reverses the consumption): it is never voided by hand.
          Option.when(!m.voided && !material.archived && !WarehouseViews.isConsumption(m))(
            div(roleGated(Role.Admin)(button(tpe := "button", cls := btnDanger, "Annulla", onClick --> (_ => pendingVoid.set(Some(m)))))),
          ),
        ),
      )
    end renderMovement

    def renderList(list: MovementsResponse): HtmlElement =
      div(
        cls := "space-y-3",
        div(
          cls := "grid grid-cols-2 gap-3 sm:max-w-md",
          balanceBox(list.from.fold("Saldo iniziale")(day => s"Saldo prima del ${IsoDay.display(day)}"), list.openingBalance),
          balanceBox(list.to.fold("Saldo finale")(day => s"Saldo al ${IsoDay.display(day)}"), list.closingBalance),
        ),
        if list.movements.isEmpty then emptyState("Nessun movimento nel periodo selezionato.")
        else
          div(
            cls := "overflow-x-auto rounded-md border border-slate-200",
            table(
              cls := "w-full text-sm",
              thead(
                cls := "border-b border-slate-200 bg-slate-50 text-left text-xs uppercase tracking-wide text-slate-500",
                tr(
                  th(cls := "px-3 py-2", "Data"),
                  th(cls := "px-3 py-2", "Tipo"),
                  th(cls := "px-3 py-2", "Fornitore / Origine"),
                  th(cls := "px-3 py-2 text-right", "Quantità"),
                  th(cls := "px-3 py-2 text-right", "Costo unit."),
                  th(cls := "px-3 py-2 text-right", "Valore"),
                  th(cls := "px-3 py-2 text-right", "Saldo"),
                  th(cls := "px-3 py-2", "Motivo / Note"),
                  th(cls := "px-3 py-2"),
                ),
              ),
              tbody(list.movements.map(renderMovement)),
            ),
          ),
      )

    div(
      cls := "space-y-3",
      range --> {
        case Right(bounds) => applied.set(bounds)
        case Left(_) => ()
      },
      div(
        cls := "flex flex-wrap items-end gap-3",
        div(cls := "w-40", field("Dal", dateInput(from.signal, from.writer, today))),
        div(cls := "w-40", field("Al", dateInput(to.signal, to.writer, today))),
        button(
          tpe := "button",
          cls := s"$btnGhost mb-0.5",
          "Tutti i movimenti",
          disabled <-- from.signal.combineWith(to.signal).map((f, t) => f.isEmpty && t.isEmpty),
          onClick --> { _ => from.set(""); to.set("") },
        ),
      ),
      child.maybe <-- range.map(_.left.toOption.map(message => div(cls := "text-xs text-rose-600", message))),
      child.maybe <-- actionError.signal.map(_.map(errorBanner)),
      renderResult(movements)(renderList),
      voidDialog,
    )
  end movementsView
end StockDialogs
