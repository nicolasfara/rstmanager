package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.OrderDrafts.{
  HeaderField,
  HeaderState,
  Issue,
  IssueTarget,
  MfgCoreState,
  MfgField,
  MfgSnapshot,
  TaskField,
}

/**
 * Pure, DOM-free helpers of the "Nuovo ordine" page: the stable DOM ids the issues point to, the inline form of an issue message, and the labels of
 * the manufacturing cards. Kept apart from [[OrderCreateView]] so they can be unit-tested.
 */
private[ui] object OrderCreateModel:

  /** The "+ Da catalogo" button, target of the "no manufacturings" issue. */
  val addMfgId = "oc-add-mfg"

  /** The banner of a failed creation request. */
  val submitErrorId = "oc-submit-error"

  def headerId(field: HeaderField): String = field match
    case HeaderField.Number => "oc-number"
    case HeaderField.Customer => "oc-customer"
    case HeaderField.CreationDate => "oc-creation"
    case HeaderField.DeliveryDate => "oc-delivery"
    case HeaderField.WorkDeadline => "oc-deadline"

  /** The DOM ids of the manufacturing cards of the page. */
  val cardIds: OrderMfgDraftCard.Ids = OrderMfgDraftCard.Ids("oc")

  def mfgFieldId(key: Int, field: MfgField): String = cardIds.mfgField(key, field)

  /** Id of a field of the task row with draft key `taskKey` (stable across removals, unlike the row index). */
  def taskFieldId(key: Int, taskKey: Int, field: TaskField): String = cardIds.taskField(key, taskKey, field)

  /**
   * The DOM id an issue points to. A task issue carries the row index, resolved to the row's draft key by `taskKeyAt(mfgKey, index)`; when the row is
   * gone it falls back to the task list of the manufacturing.
   */
  def domId(target: IssueTarget, taskKeyAt: (Int, Int) => Option[Int]): String = cardIds.domId(target, taskKeyAt, headerId, addMfgId)

  /** The manufacturing draft key an issue belongs to, if any. */
  def mfgKeyOf(target: IssueTarget): Option[Int] = target match
    case IssueTarget.Manufacturing(key, _) => Some(key)
    case IssueTarget.Task(key, _, _) => Some(key)
    case _ => None

  def isHeader(target: IssueTarget): Boolean = target match
    case IssueTarget.Header(_) => true
    case _ => false

  /**
   * An issue message as shown under its field: without the "Lavorazione n, task m:" context (the field already sits there), capitalized. Header
   * messages carry no such prefix and are returned unchanged.
   */
  def inlineMessage(message: String): String =
    val separator = message.indexOf(": ")
    val body = if separator < 0 then message else message.substring(separator + 2).nn
    body.headOption.fold(body)(first => s"${first.toUpper}${body.drop(1)}")

  /**
   * True while a manufacturing draft holds nothing worth confirming before removal (the chosen type alone does not count; a multiplier ×N other than
   * the default 1 does).
   */
  def isBlankDraft(snapshot: MfgSnapshot): Boolean =
    val core = snapshot.core
    List(core.catalogId, core.code, core.completionDate, core.description, core.employeeId).forall(_.trim.nn.isEmpty) &&
    core.quantity.trim.nn == "1" &&
    core.dependsOn.isEmpty &&
    snapshot.tasks.forall(task => task.taskId.trim.nn.isEmpty && task.hours.trim.nn.isEmpty && task.employeeId.trim.nn.isEmpty) &&
    snapshot.materials.forall(MaterialDrafts.isBlank)

  /** Short code of a manufacturing (chips "Dipende da"): the template code, or the typed code; empty while missing. */
  def mfgCode(core: MfgCoreState, catalog: String => Option[ManufacturingCatalogResponse]): String =
    if core.mode == "catalog" then catalog(core.catalogId).fold("")(_.code) else core.code.trim.nn

  /** Card title of a manufacturing: "code · name" of the template, or the typed code; empty while missing. */
  def mfgLabel(core: MfgCoreState, catalog: String => Option[ManufacturingCatalogResponse]): String =
    if core.mode == "catalog" then catalog(core.catalogId).fold("")(template => s"${template.code} · ${template.name}") else core.code.trim.nn

  /** Minutes as `h:mm h`. */
  def hoursText(minutes: Int): String = s"${Formats.duration(minutes)} h"

  /** "n task · h:mm h · € x", the compact totals line of a card, the recap and the mobile bar. */
  def totalsText(totals: OrderDrafts.MfgTotals): String =
    s"${totals.tasks} task · ${hoursText(totals.minutes)} · ${WarehouseFormats.formatEuro(totals.materialsCost)}"
end OrderCreateModel

/**
 * The "Nuovo ordine" page: a full-page form in three numbered sections (Testata, Lavorazioni, Riepilogo e conferma) with a sticky summary sidebar (a
 * bottom bar on mobile). Every rule lives in the pure [[OrderDrafts]]; errors stay hidden until a field is left or a submit is attempted
 * ([[FormTouch]]), and an invalid submit reveals and focuses the first issue. Mounted fresh on every opening, so each one starts from a new draft.
 * The manufacturing cards are [[OrderMfgDraftCard]]s, shared with the new manufacturings of the edit page.
 */
object OrderCreateView:
  import OrderCreateModel.*

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** `java.util.UUID.randomUUID` is unavailable on Scala.js (needs SecureRandom); use the Web Crypto API. */
  private def randomUuid(): UUID = UUID.fromString(js.Dynamic.global.crypto.randomUUID().toString).nn

  private def normalizeStr(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  /** The page; `onExit` goes back to the orders list (after a creation or a confirmed abandon). */
  def apply(deps: OrderFormView.Deps, onExit: () => Unit): HtmlElement =
    var keyCounter = 0
    def nextKey(): Int =
      keyCounter += 1
      keyCounter

    val today = DayText.today()
    val header = Var(HeaderState.initial(GeneratedCodes.next("ORD", Nil), today))
    val mfgs = Var(List.empty[MfgDraft])
    // Materials the draft lines are checked against, written by the drafts' materials editors once loaded.
    val materialsContext = Var(MaterialDrafts.Context.empty)
    // Synchronous mirrors of the loaded catalogs, read by event handlers and by submit.
    val catalogs = Var(List.empty[ManufacturingCatalogResponse])
    val tasksById = Var(Map.empty[String, TaskResponse])
    val touch = FormTouch()
    // One card open at a time; `seen` are the cards collapsed at least once, whose status is shown from then on.
    val expanded = Var(Option.empty[Int])
    val seen = Var(Set.empty[Int])
    // Cards added after the last failed submit: that attempt does not make their errors visible until they are touched, collapsed or submitted again.
    val fresh = Var(Set.empty[Int])
    val submitting = Var(false)
    val submitError = Var(Option.empty[ApiError])
    val leaveOpen = Var(false)
    val removing = Var(Option.empty[Int])

    def catalogById(rawId: String): Option[ManufacturingCatalogResponse] = OrderMfgDraftCard.catalogLookup(catalogs.now())(rawId)

    /** The touch tracker of the fields of card `key`: a past submit attempt counts unless the card is [[fresh]]. */
    def touchOf(key: Int): FormTouch = touch.scoped(fresh.signal.map(set => !set.contains(key)).distinct)

    /** Whether an issue is shown as an error (red) in the checklist: after a submit attempt, except for the fresh cards. */
    val emphasized: Signal[Issue => Boolean] =
      touch.submittedSignal
        .combineWith(fresh.signal)
        .map((attempted, freshKeys) => issue => attempted && !mfgKeyOf(issue.target).exists(freshKeys.contains))

    val snapshots: Signal[List[MfgSnapshot]] = OrderMfgDraftCard.snapshots(mfgs.signal)

    val issues: Signal[List[Issue]] =
      header.signal
        .combineWith(snapshots, catalogs.signal, materialsContext.signal)
        .map((h, list, templates, context) => OrderDrafts.validate(h, list, OrderMfgDraftCard.catalogLookup(templates), context))
        .distinct

    def currentIssues(): List[Issue] =
      OrderDrafts.validate(header.now(), mfgs.now().map(OrderMfgDraftCard.snapshotOf), catalogById, materialsContext.now())

    val totals: Signal[OrderDrafts.MfgTotals] =
      snapshots
        .combineWith(catalogs.signal, materialsContext.signal)
        .map((list, templates, context) => OrderDrafts.orderTotals(list, OrderMfgDraftCard.catalogLookup(templates), context))
        .distinct

    // Stock of the materials the draft uses, loaded once a material line exists and refreshed on order and warehouse changes.
    val hasMaterials = snapshots.map(_.exists(_.materials.exists(row => !MaterialDrafts.isBlank(row)))).distinct
    val requirements: Signal[List[MaterialRequirementResponse]] =
      hasMaterials.flatMapSwitch { any =>
        if any then
          loadable(AppBus.ordersTicks.combineWith(AppBus.warehouseTicks))(() => ApiClient.materialRequirements(None)).map {
            case Some(Right(response)) => response.materials
            case _ => Nil
          }
        else Signal.fromValue(Nil)
      }
    val stockWarnings: Signal[Map[UUID, OrderDrafts.StockWarning]] =
      snapshots.combineWith(requirements).map((list, reqs) => OrderDrafts.stockWarnings(list.flatMap(_.materials), reqs)).distinct
    val stockWarningList: Signal[List[OrderDrafts.StockWarning]] = stockWarnings.map(_.values.toList.sortBy(_.internalCode))
    val stockFootnote = "Avviso non bloccante: l'ordine si può creare comunque."

    def isDirty(): Boolean = !OrderDrafts.isPristine(header.now(), today, mfgs.now().map(OrderMfgDraftCard.snapshotOf))

    /** The inline message of the issue at `target` (the signal lets a task row follow its current index). */
    def rawAt(target: Signal[IssueTarget]): Signal[Option[String]] =
      issues
        .combineWith(target)
        .map((list, at) => list.collectFirst { case issue if issue.target == at => inlineMessage(issue.message) })
        .distinct

    def rawFor(target: IssueTarget): Signal[Option[String]] = rawAt(Signal.fromValue(target))

    // ---- Navigation between cards and issues -------------------------------------------------------

    def expand(key: Int): Unit =
      expanded.now().filter(_ != key).foreach(previous => seen.update(_ + previous))
      expanded.set(Some(key))

    def collapse(key: Int): Unit =
      seen.update(_ + key)
      fresh.update(_ - key)
      if expanded.now().contains(key) then expanded.set(None)

    def toggle(key: Int): Unit = if expanded.now().contains(key) then collapse(key) else expand(key)

    def taskKeyAt(key: Int, index: Int): Option[Int] = mfgs.now().find(_.key == key).flatMap(_.tasks.now().lift(index)).map(_.key)

    /** Expands the card an issue belongs to and focuses its field. */
    def reveal(issue: Issue): Unit =
      mfgKeyOf(issue.target).foreach(expand)
      FormFocus.focus(domId(issue.target, taskKeyAt))

    /** "Fatto ✓": the card collapses and every one of its errors becomes visible. */
    def done(m: MfgDraft): Unit =
      touch.touchAll(cardIds.cardFieldIds(m))
      collapse(m.key)

    def addMfg(mode: String): Unit =
      val m = OrderMfgDraftCard.newDraft(mode, () => nextKey())
      if touch.attempted then fresh.update(_ + m.key)
      mfgs.update(_ :+ m)
      expand(m.key)
      FormFocus.focus(mfgFieldId(m.key, if mode == "catalog" then MfgField.Catalog else MfgField.Code))

    def removeMfg(key: Int): Unit =
      mfgs.update(_.filterNot(_.key == key))
      mfgs.now().foreach(other => other.state.update(core => core.copy(dependsOn = core.dependsOn - key.toString)))
      if expanded.now().contains(key) then expanded.set(None)
      FormFocus.focus(addMfgId)

    def requestRemove(key: Int): Unit =
      mfgs.now().find(_.key == key).foreach { m =>
        if isBlankDraft(OrderMfgDraftCard.snapshotOf(m)) then removeMfg(key) else removing.set(Some(key))
      }

    def requestLeave(): Unit = if isDirty() then leaveOpen.set(true) else onExit()

    // ---- Submit ------------------------------------------------------------------------------------

    def submit(): Unit =
      if !submitting.now() then
        currentIssues() match
          case first :: _ =>
            touch.attemptSubmit()
            fresh.set(Set.empty)
            reveal(first)
          case Nil =>
            val h = header.now()
            val drafts = mfgs.now().map(OrderMfgDraftCard.snapshotOf)
            val manufacturings = OrderDrafts.collectManufacturings(drafts, h.workDeadline, catalogById, materialsContext.now(), () => randomUuid())
            (parseUuid(h.customerId.trim.nn), manufacturings) match
              case (None, _) =>
                showError(submitError, "Creazione ordine")(ApiError("invalid-form", "Seleziona un cliente valido.", Nil))
                FormFocus.focus(submitErrorId)
              case (_, Left(err)) =>
                showError(submitError, "Creazione ordine")(err)
                FormFocus.focus(submitErrorId)
              case (Some(customerId), Right(list)) =>
                val number = h.number.trim.nn
                val request = OrderRequest(
                  number,
                  customerId,
                  Formats.toIso(h.creationDate),
                  Formats.toIso(h.deliveryDate),
                  Formats.toIso(h.workDeadline),
                  h.priority,
                  list,
                  normalizeStr(h.description),
                  OrderDrafts.collectDependencies(drafts),
                )
                submitting.set(true)
                submitError.set(None)
                ApiClient.createOrder(request).foreach { result =>
                  submitting.set(false)
                  result match
                    case Right(_) =>
                      Notifications.success(s"Ordine $number creato")
                      AppBus.mutatedOrders()
                      onExit()
                    case Left(err) =>
                      showError(submitError, "Creazione ordine")(err)
                      FormFocus.focus(submitErrorId)
                }
            end match
    end submit

    // ---- Derived display ---------------------------------------------------------------------------

    def countStatus(count: Signal[Int]): Signal[SectionStatus] =
      count.combineWith(touch.submittedSignal).map { (n, attempted) =>
        if n == 0 then SectionStatus.Complete else if attempted then SectionStatus.Pending(n) else SectionStatus.Neutral
      }

    val headerStatus = countStatus(issues.map(_.count(issue => isHeader(issue.target))))
    val mfgsStatus = countStatus(issues.map(_.count(issue => !isHeader(issue.target))))
    val allStatus = countStatus(issues.map(_.size))

    // ---- Section 1: Testata ------------------------------------------------------------------------

    def headerField(labelText: String, field: HeaderField): FieldSpec =
      touch.spec(labelText, headerId(field), true, rawFor(IssueTarget.Header(field)))

    def dateInput(valueSignal: Signal[String], writer: HeaderState => String => HeaderState, bind: Modifier[HtmlElement]): HtmlElement =
      textInput(valueSignal, Observer[String](v => header.update(h => writer(h)(v))), "", "date").amend(bind)

    val deadlineField =
      OrderFormView.deadlineField(
        touch.spec(
          "Deadline fine lavorazione",
          headerId(HeaderField.WorkDeadline),
          true,
          s"Proposta: ${OrderDrafts.deadlineLeadDays} giorni prima della consegna",
          rawFor(IssueTarget.Header(HeaderField.WorkDeadline)),
        ),
        bind => dateInput(header.signal.map(_.workDeadline), h => h.withDeadline, bind),
        header.signal.map(h => h.workDeadlineManuallyEdited && h.deliveryDate.trim.nn.nonEmpty),
        () => header.update(_.withProposedDeadline),
        header.signal.map(OrderDrafts.deadlineAfterDelivery),
      )

    val headerSection =
      formSection(1, "Testata", "Cliente, date e priorità dell'ordine.", headerStatus)(
        div(
          cls := OrderFormView.headerGridCls,
          field(
            touch.spec(
              "Numero ordine",
              headerId(HeaderField.Number),
              true,
              "Generato automaticamente, modificabile",
              rawFor(IssueTarget.Header(HeaderField.Number)),
            ),
          )(bind => textInput(header.signal.map(_.number), Observer[String](v => header.update(_.withNumber(v))), "ORD-2026-001").amend(bind)),
          field(headerField("Cliente", HeaderField.Customer)) { bind =>
            searchableSelect(
              header.signal.map(_.customerId),
              Observer[String](v => header.update(_.copy(customerId = v))),
              deps.customerOptions,
              "Cerca cliente…",
              20,
              bind,
            )
          },
          field(headerField("Data creazione", HeaderField.CreationDate))(bind =>
            dateInput(header.signal.map(_.creationDate), h => v => h.copy(creationDate = v), bind),
          ),
          field(headerField("Consegna cliente", HeaderField.DeliveryDate))(bind =>
            dateInput(header.signal.map(_.deliveryDate), h => h.withDelivery, bind),
          ),
          deadlineField,
          OrderFormView.priorityField(header.signal.map(_.priority), Observer[String](v => header.update(_.copy(priority = v))), "oc-priority"),
          OrderFormView.descriptionField(
            header.signal.map(_.description),
            Observer[String](v => header.update(_.copy(description = v))),
            "oc-description",
          ),
        ),
      )

    // ---- Section 2: Lavorazioni --------------------------------------------------------------------

    val cardContext = OrderMfgDraftCard.Context(
      cardIds,
      touchOf,
      rawAt,
      key => issues.map(_.count(issue => mfgKeyOf(issue.target).contains(key))).distinct,
      catalogs.signal,
      catalogById,
      deps.tasks,
      id => tasksById.now().get(id),
      deps.employeeOptions,
      header.signal.map(_.workDeadline),
      materialsContext.signal,
      materialsContext.writer,
      stockWarnings,
      () => nextKey(),
      snapshots,
      Signal.fromValue(0),
      true,
      expanded.signal,
      seen.signal,
      toggle,
    )

    def renderMfg(m: MfgDraft): HtmlElement =
      OrderMfgDraftCard(
        cardContext,
        m,
        button(tpe := "button", cls := btnDanger, "Rimuovi", onClick --> (_ => requestRemove(m.key))),
        div(
          cls := "flex justify-end border-t border-slate-100 pt-3",
          button(tpe := "button", cls := btnGhost, "Fatto ✓", onClick --> (_ => done(m))),
        ),
      )

    val addButtons =
      div(
        cls := "flex flex-wrap items-center gap-2",
        button(idAttr := addMfgId, tpe := "button", cls := btnGhost, "+ Da catalogo", onClick --> (_ => addMfg("catalog"))),
        button(tpe := "button", cls := btnGhost, "+ Personalizzata", onClick --> (_ => addMfg("custom"))),
      )

    val mfgsSection =
      formSection(2, "Lavorazioni", "Aggiungi le lavorazioni dal catalogo o personalizzate; apri una scheda per modificarla.", mfgsStatus)(
        div(cls := "space-y-3", children <-- mfgs.signal.split(_.key)((_, initial, _) => renderMfg(initial))),
        child <-- mfgs.signal.map(_.isEmpty).distinct.map { empty =>
          if empty then
            div(
              cls := "flex flex-col items-center gap-3 rounded-lg border border-dashed border-slate-300 px-4 py-8 text-center",
              p(cls := "text-sm text-slate-500", "Nessuna lavorazione. Aggiungine una dal catalogo o creane una personalizzata."),
              addButtons,
            )
          else addButtons
        },
        fieldError(s"$addMfgId-error", touch.visible(addMfgId, rawFor(IssueTarget.NoManufacturings))),
      )

    // ---- Section 3: Riepilogo e conferma -----------------------------------------------------------

    val recap =
      div(
        child <-- snapshots.combineWith(catalogs.signal, materialsContext.signal, header.signal.map(_.workDeadline)).map {
          (list, templates, context, deadline) =>
            val lookup = OrderMfgDraftCard.catalogLookup(templates)
            OrderFormView.recapList(
              list.map { s =>
                val day = DayText.short(if s.core.completionDate.trim.nn.nonEmpty then s.core.completionDate else deadline)
                OrderFormView.RecapItem(
                  mfgLabel(s.core, lookup),
                  totalsText(OrderDrafts.mfgTotals(s, lookup, context)),
                  day,
                  OrderDrafts.quantityOf(s.core),
                )
              },
            )
        },
        OrderFormView.recapTotal(totals.map(totalsText)),
        child.maybe <-- stockWarningList.map(list => OrderFormView.stockWarningsBox(list, stockFootnote).map(box => div(cls := "mt-3", box))),
      )

    val confirmSection =
      formSection(3, "Riepilogo e conferma", "Controlla i dati e crea l'ordine.", allStatus)(
        recap,
        div(
          cls := "space-y-2 lg:hidden",
          subheading("Da completare"),
          issueList(issues, _.message, emphasized, reveal),
        ),
        child.maybe <-- submitError.signal.map(_.map(err => div(idAttr := submitErrorId, errorBanner(err)))),
        // The one "Crea ordine" is in the sidebar (from `lg`) or in the bottom bar (below); here only "Annulla", on mobile.
        div(
          cls := "flex justify-end border-t border-slate-100 pt-3 lg:hidden",
          button(tpe := "button", cls := s"$btnGhost justify-center", "Annulla", onClick --> (_ => requestLeave())),
        ),
      )

    // ---- Sidebar & mobile bar ----------------------------------------------------------------------

    val summary = OrderFormView.Summary(
      header.signal.map(h => OrderFormView.orDash(h.number)),
      OrderFormView.customerName(deps, header.signal.map(_.customerId)),
      header.signal.map(h => OrderFormView.orDash(DayText.long(h.deliveryDate))),
      header.signal.map(h => OrderFormView.orDash(DayText.long(h.workDeadline))),
      header.signal.map(h => OrderFormView.priorityLabel(h.priority)),
      mfgs.signal.map(_.size),
      totals.map(_.minutes),
      totals.map(_.materialsCost),
    )

    val sidebar =
      OrderFormView.sidebar(summary)(
        child.maybe <-- stockWarningList.map(list =>
          OrderFormView.stockWarningsBox(list, stockFootnote).map(box => div(cls := "border-t border-slate-100 pt-3", box)),
        ),
        div(
          cls := "space-y-2 border-t border-slate-100 pt-3",
          div(cls := subheadingCls, child.text <-- issues.map(list => if list.isEmpty then "Da completare" else s"Da completare (${list.size})")),
          issueList(issues, _.message, emphasized, reveal),
        ),
        formActions("Crea ordine", "Creazione…", submitting.signal, Some(() => requestLeave()), () => submit()),
      )

    val mobileBar =
      OrderFormView.mobileBar(
        span(child.text <-- OrderFormView.mobileTotals(summary)),
        OrderFormView.issueBadge(issues.map(_.size).distinct, () => currentIssues().headOption.foreach(reveal)),
        "Crea ordine",
        "Creazione…",
        submitting.signal,
        submitting.signal,
        () => submit(),
      )

    // ---- Dialogs -----------------------------------------------------------------------------------

    val leaveDialog =
      confirmDialog(
        leaveOpen.signal,
        Signal.fromValue("Abbandonare il nuovo ordine?"),
        Signal.fromValue("L'ordine non è stato creato: i dati inseriti andranno persi."),
        "Abbandona",
        "Continua a modificare",
        DialogTone.Danger,
        () => leaveOpen.set(false),
        () =>
          leaveOpen.set(false)
          onExit(),
      )

    val removeDialog =
      confirmDialog(
        removing.signal.map(_.isDefined),
        removing.signal.combineWith(snapshots, catalogs.signal).map { (pending, list, templates) =>
          val label =
            pending.flatMap(key => list.find(_.key == key)).map(s => mfgLabel(s.core, OrderMfgDraftCard.catalogLookup(templates))).getOrElse("")
          if label.isEmpty then "Rimuovere la lavorazione?" else s"Rimuovere la lavorazione «$label»?"
        },
        Signal.fromValue("I dati inseriti nella lavorazione andranno persi."),
        "Rimuovi",
        DialogTone.Danger,
        () => removing.set(None),
        () =>
          val pending = removing.now()
          removing.set(None)
          pending.foreach(removeMfg),
      )

    formPage(PageHeader(OrderFormView.backLabel, () => requestLeave(), Signal.fromValue("Nuovo ordine"), initialFocus = PageFocus.FirstField))(
      NavigationGuard.guard("Abbandonare il nuovo ordine?", "L'ordine non è stato creato: i dati inseriti andranno persi.", () => isDirty()),
      deps.catalogs --> catalogs.writer,
      deps.tasks --> (list => tasksById.set(list.map(task => task.id.toString -> task).toMap)),
      deps.existingOrderNumbers --> (numbers => header.update(_.withGeneratedNumber(GeneratedCodes.next("ORD", numbers)))),
      summaryLayout(div(cls := "space-y-6", headerSection, mfgsSection, confirmSection), sidebar, mobileBar),
      leaveDialog,
      removeDialog,
    )
  end apply
end OrderCreateView
