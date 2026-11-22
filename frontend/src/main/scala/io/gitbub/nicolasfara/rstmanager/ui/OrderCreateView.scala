package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
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
  TaskState,
}
import org.scalajs.dom

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

  def mfgFieldId(key: Int, field: MfgField): String = field match
    case MfgField.Catalog => s"oc-mfg-$key-catalog"
    case MfgField.Code => s"oc-mfg-$key-code"
    case MfgField.Quantity => s"oc-mfg-$key-quantity"
    case MfgField.Tasks => s"oc-mfg-$key-tasks"
    case MfgField.Materials => s"oc-mfg-$key-materials"

  /** Id of a field of the task row with draft key `taskKey` (stable across removals, unlike the row index). */
  def taskFieldId(key: Int, taskKey: Int, field: TaskField): String = field match
    case TaskField.Task => s"oc-mfg-$key-task-$taskKey-task"
    case TaskField.Duration => s"oc-mfg-$key-task-$taskKey-duration"

  /**
   * The DOM id an issue points to. A task issue carries the row index, resolved to the row's draft key by `taskKeyAt(mfgKey, index)`; when the row is
   * gone it falls back to the task list of the manufacturing.
   */
  def domId(target: IssueTarget, taskKeyAt: (Int, Int) => Option[Int]): String = target match
    case IssueTarget.Header(field) => headerId(field)
    case IssueTarget.NoManufacturings => addMfgId
    case IssueTarget.Manufacturing(key, field) => mfgFieldId(key, field)
    case IssueTarget.Task(key, index, field) => taskKeyAt(key, index).fold(mfgFieldId(key, MfgField.Tasks))(taskFieldId(key, _, field))

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

  /** True while a manufacturing draft holds nothing worth confirming before removal (the chosen type alone does not count). */
  def isBlankDraft(snapshot: MfgSnapshot): Boolean =
    val core = snapshot.core
    List(core.catalogId, core.code, core.completionDate, core.description, core.employeeId).forall(_.trim.nn.isEmpty) &&
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
 */
object OrderCreateView:
  import OrderCreateModel.*

  /** The shared data of the orders page the form needs. */
  final case class Deps(
      customerOptions: Signal[List[(String, String)]],
      tasks: Signal[List[TaskResponse]],
      catalogs: Signal[List[ManufacturingCatalogResponse]],
      employeeOptions: Signal[List[(String, String)]],
      existingOrderNumbers: Signal[List[String]],
  )

  private val priorityOptions = List("normal" -> "Normale", "urgent" -> "Urgente")

  private val modeOptions = List("catalog" -> "Da catalogo", "custom" -> "Personalizzata")

  private val labelCls = "mb-1 block text-xs font-medium text-slate-500"

  /** Mutable draft of a custom task row: `durationEdited` stops the duration from following the picked task. */
  private final case class TaskDraft(key: Int, state: Var[TaskState], durationEdited: Var[Boolean])

  /** `tasks` and `materials` are separate `Var`s because they undergo their own add/remove mutations. */
  private final case class MfgDraft(key: Int, state: Var[MfgCoreState], tasks: Var[List[TaskDraft]], materials: Var[List[MaterialDrafts.Row]])

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** `java.util.UUID.randomUUID` is unavailable on Scala.js (needs SecureRandom); use the Web Crypto API. */
  private def randomUuid(): UUID = UUID.fromString(js.Dynamic.global.crypto.randomUUID().toString).nn

  private def normalizeStr(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  private def catalogLookup(list: List[ManufacturingCatalogResponse])(rawId: String): Option[ManufacturingCatalogResponse] =
    parseUuid(rawId).flatMap(id => list.find(_.id == id))

  /** A label over a control that is not a labelable element (a segmented group). */
  private def labelled(text: String, control: HtmlElement): HtmlElement = div(div(cls := labelCls, text), control)

  private def orDash(text: String): String = if text.trim.nn.isEmpty then "—" else text

  // ---- Catalog helpers (shared with the edit modal) ------------------------------------------------

  /** Effective minutes of each template task: the template override, else the task-catalog duration. */
  private def effectiveMinutes(template: ManufacturingCatalogResponse): List[(TaskResponse, Int)] =
    val overrides = template.taskDurations.map(entry => entry.taskId -> entry.minutes).toMap
    template.tasks.map(task => task -> overrides.getOrElse(task.id, task.requiredMinutes))

  /** A compact read-only preview of the selected catalog template (tasks with their effective durations, total, dependencies). */
  private[ui] def catalogPreview(catalogIdSignal: Signal[String], catalogs: Signal[List[ManufacturingCatalogResponse]]): HtmlElement =
    div(
      child <-- catalogIdSignal.combineWith(catalogs).map { (rawId, list) =>
        catalogLookup(list)(rawId) match
          case None => emptyNode
          case Some(template) =>
            val tasks = effectiveMinutes(template)
            div(
              cls := "rounded-md border border-slate-200 bg-slate-50 p-3 text-xs text-slate-600",
              div(cls := "font-medium text-slate-700", s"${template.code} · ${template.name}"),
              template.description.map(d => div(cls := "mt-0.5 text-slate-500", d)),
              div(cls := "mt-1", tasks.map((task, minutes) => s"${task.name} (${Formats.duration(minutes)}h)").mkString(", ")),
              div(
                cls := "mt-1 text-slate-400",
                s"${Formats.duration(tasks.map(_._2).sum)}h totali · ${template.dependencies.map(_.dependsOn.size).sum} dipendenze",
              ),
            )
      },
    )

  /**
   * Per-task employee selects for a catalog-mode manufacturing: one row per template task, prefilled with the catalog default and overridable before
   * submit. Hidden until a valid template is selected.
   */
  private[ui] def catalogTaskEmployeeOverrides(
      catalogIdSignal: Signal[String],
      catalogs: Signal[List[ManufacturingCatalogResponse]],
      employeeOptions: Signal[List[(String, String)]],
      currentMap: Signal[Map[String, String]],
      setEmployee: (String, String) => Unit,
  ): HtmlElement =
    div(
      child <-- catalogIdSignal.combineWith(catalogs).map { (rawId, list) =>
        catalogLookup(list)(rawId) match
          case None => emptyNode
          case Some(template) =>
            div(
              cls := "space-y-2 rounded-md border border-slate-200 p-3",
              subheading("Dipendente per task"),
              effectiveMinutes(template).map { (task, minutes) =>
                div(
                  cls := "grid grid-cols-1 gap-1 sm:grid-cols-[minmax(0,1fr)_minmax(0,14rem)] sm:items-center",
                  div(cls := "text-sm text-slate-700", s"${task.name} (${Formats.duration(minutes)}h)"),
                  selectInput(
                    currentMap.map(_.getOrElse(task.id.toString, "")),
                    Observer[String](v => setEmployee(task.id.toString, v)),
                    employeeOptions,
                  ).amend(aria.label := s"Dipendente per ${task.name}"),
                )
              },
            )
      },
    )

  /**
   * The per-unit materials of the selected template: what is prefilled into the draft (× N), and a warning for the archived or unknown ones, which
   * are not prefilled. Hidden while no template (or one without materials) is selected.
   */
  private def catalogMaterialsNote(catalogIdSignal: Signal[String], catalogs: Signal[List[ManufacturingCatalogResponse]]): HtmlElement =
    def lineText(line: CatalogMaterialResponse): String =
      val code = line.internalCode.getOrElse(Formats.shortId(line.materialId))
      val quantity =
        line.unit.fold(WarehouseFormats.formatDecimal(line.quantity, 0, MaterialDrafts.quantityScale))(WarehouseFormats.formatQty(line.quantity, _))
      s"$code ($quantity)"
    div(
      child <-- catalogIdSignal.combineWith(catalogs).map { (rawId, list) =>
        catalogLookup(list)(rawId).filter(_.materials.nonEmpty) match
          case None => emptyNode
          case Some(template) =>
            val unavailable = OrderDrafts.unavailableCatalogMaterials(template)
            div(
              cls := "space-y-1 text-xs",
              div(cls := "text-slate-500", s"Materiali per unità: ${template.materials.map(lineText).mkString(", ")} — precompilati × quantità."),
              Option.when(unavailable.nonEmpty)(
                div(
                  cls := "rounded-md border border-amber-200 bg-amber-50 px-2 py-1 text-amber-800",
                  role := "status",
                  s"Non precompilati perché archiviati o non più in magazzino: ${unavailable.map(lineText).mkString(", ")}.",
                ),
              ),
            )
      },
    )
  end catalogMaterialsNote

  /** The stock warnings of the whole draft (non-blocking): one amber line per material the draft needs more of than is available. */
  private def stockWarningsBox(warnings: List[OrderDrafts.StockWarning]): com.raquo.laminar.nodes.ChildNode.Base =
    if warnings.isEmpty then emptyNode
    else
      div(
        cls := "space-y-1 rounded-md border border-amber-200 bg-amber-50 px-3 py-2 text-xs text-amber-800",
        role := "status",
        div(cls := "font-semibold", s"Materiali da ordinare (${warnings.size})"),
        ul(cls := "space-y-0.5", warnings.map(w => li(span(cls := "font-medium", w.internalCode), s": ${OrderDrafts.stockWarningText(w)}"))),
        div(cls := "text-amber-700", "Avviso non bloccante: l'ordine si può creare comunque."),
      )

  /**
   * The "×N" badge of a manufacturing done `times` > 1 times (none for one). It stays outside the truncated label so a phone still shows it, and
   * reads "quantità N" to a screen reader.
   */
  private[ui] def quantityBadge(times: Int): Option[HtmlElement] =
    Option.when(times > 1)(
      span(
        cls := "shrink-0 rounded-full bg-slate-100 px-1.5 py-0.5 text-xs font-semibold tabular-nums text-slate-600",
        title := s"Quantità: la lavorazione si esegue $times volte (durate e materiali sono già i totali)",
        span(aria.hidden := true, s"×$times"),
        span(cls := "sr-only", s"quantità $times"),
      ),
    )

  // ---- Page --------------------------------------------------------------------------------------

  /** The page; `onExit` goes back to the orders list (after a creation or a confirmed abandon). */
  def apply(deps: Deps, onExit: () => Unit): HtmlElement =
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

    def catalogById(rawId: String): Option[ManufacturingCatalogResponse] = catalogLookup(catalogs.now())(rawId)

    /** The touch tracker of the fields of card `key`: a past submit attempt counts unless the card is [[fresh]]. */
    def touchOf(key: Int): FormTouch = touch.scoped(fresh.signal.map(set => !set.contains(key)).distinct)

    /** Whether an issue is shown as an error (red) in the checklist: after a submit attempt, except for the fresh cards. */
    val emphasized: Signal[Issue => Boolean] =
      touch.submittedSignal
        .combineWith(fresh.signal)
        .map((attempted, freshKeys) => issue => attempted && !mfgKeyOf(issue.target).exists(freshKeys.contains))

    def newTask(): TaskDraft = TaskDraft(nextKey(), Var(TaskState("", "", Set.empty, "")), Var(false))

    def snapshotOf(m: MfgDraft): MfgSnapshot = MfgSnapshot(m.key, m.state.now(), m.tasks.now().map(_.state.now()), m.materials.now())

    def taskStatesOf(m: MfgDraft): Signal[List[(Int, TaskState)]] =
      m.tasks.signal.flatMapSwitch(rows => combineAll(rows.map(row => row.state.signal.map(row.key -> _))))

    def snapshotSignal(m: MfgDraft): Signal[MfgSnapshot] =
      m.state.signal
        .combineWith(taskStatesOf(m), m.materials.signal)
        .map((core, tasks, rows) => MfgSnapshot(m.key, core, tasks.map(_._2), rows))

    val snapshots: Signal[List[MfgSnapshot]] = mfgs.signal.flatMapSwitch(list => combineAll(list.map(snapshotSignal)))

    val issues: Signal[List[Issue]] =
      header.signal
        .combineWith(snapshots, catalogs.signal, materialsContext.signal)
        .map((h, list, templates, context) => OrderDrafts.validate(h, list, catalogLookup(templates), context))
        .distinct

    def currentIssues(): List[Issue] =
      OrderDrafts.validate(header.now(), mfgs.now().map(snapshotOf), catalogById, materialsContext.now())

    val totals: Signal[OrderDrafts.MfgTotals] =
      snapshots
        .combineWith(catalogs.signal, materialsContext.signal)
        .map((list, templates, context) => OrderDrafts.orderTotals(list, catalogLookup(templates), context))
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

    def isDirty(): Boolean = !OrderDrafts.isPristine(header.now(), today, mfgs.now().map(snapshotOf))

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

    def cardFieldIds(m: MfgDraft): List[String] =
      List(MfgField.Catalog, MfgField.Code, MfgField.Quantity, MfgField.Tasks).map(mfgFieldId(m.key, _)) ++
        m.tasks.now().flatMap(row => List(TaskField.Task, TaskField.Duration).map(taskFieldId(m.key, row.key, _)))

    /** "Fatto ✓": the card collapses and every one of its errors becomes visible. */
    def done(m: MfgDraft): Unit =
      touch.touchAll(cardFieldIds(m))
      collapse(m.key)

    def addMfg(mode: String): Unit =
      val m = MfgDraft(
        nextKey(),
        Var(MfgCoreState(mode, "", "", "", "", "", Set.empty, Map.empty)),
        Var(if mode == "custom" then List(newTask()) else Nil),
        Var(Nil),
      )
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
        if isBlankDraft(snapshotOf(m)) then removeMfg(key) else removing.set(Some(key))
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
            val drafts = mfgs.now().map(snapshotOf)
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

    val customerName: Signal[String] =
      header.signal.map(_.customerId).combineWith(deps.customerOptions).map { (selected, options) =>
        options.collectFirst { case (id, labelText) if id == selected => labelText }.getOrElse("—")
      }

    val catalogOptions: Signal[List[(String, String)]] = catalogs.signal.map(_.map(c => c.id.toString -> s"${c.code} · ${c.name}"))

    val taskOptions: Signal[List[(String, String)]] =
      deps.tasks.map(_.map(t => t.id.toString -> s"${t.name} (${Formats.duration(t.requiredMinutes)}h)"))

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
      div(
        field(
          touch.spec(
            "Deadline fine lavorazione",
            headerId(HeaderField.WorkDeadline),
            true,
            s"Proposta: ${OrderDrafts.deadlineLeadDays} giorni prima della consegna",
            rawFor(IssueTarget.Header(HeaderField.WorkDeadline)),
          ),
        )(bind => dateInput(header.signal.map(_.workDeadline), h => h.withDeadline, bind)),
        child.maybe <-- header.signal.map(h => h.workDeadlineManuallyEdited && h.deliveryDate.trim.nn.nonEmpty).distinct.map { show =>
          Option.when(show)(
            button(
              tpe := "button",
              cls := s"$btnLink mt-1 text-xs",
              "↺ Usa proposta",
              onClick --> (_ => header.update(_.withProposedDeadline)),
            ),
          )
        },
        child.maybe <-- header.signal.map(OrderDrafts.deadlineAfterDelivery).distinct.map { late =>
          Option.when(late)(
            div(
              cls := "mt-1 rounded-md bg-amber-50 px-2 py-1 text-xs text-amber-800",
              "La deadline è successiva alla consegna cliente: verifica le date.",
            ),
          )
        },
      )

    val headerSection =
      formSection(1, "Testata", "Cliente, date e priorità dell'ordine.", headerStatus)(
        div(
          cls := "grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-2",
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
          labelled(
            "Priorità",
            segmented(
              header.signal.map(_.priority),
              Observer[String](v => header.update(_.copy(priority = v))),
              priorityOptions,
              aria.label := "Priorità",
            ),
          ),
          div(
            cls := "sm:col-span-2",
            field(FieldSpec("Descrizione ordine", "oc-description"))(bind =>
              textAreaInput(header.signal.map(_.description), Observer[String](v => header.update(_.copy(description = v))), "Opzionale")
                .amend(bind),
            ),
          ),
        ),
      )

    // ---- Section 2: Lavorazioni --------------------------------------------------------------------

    def renderTask(m: MfgDraft, t: TaskDraft): HtmlElement =
      val k = m.key
      val cardTouch = touchOf(k)
      val rowIndex = m.tasks.signal.map(_.indexWhere(_.key == t.key)).distinct

      /** Removes the row and moves the focus to the next row's task (the previous one when it was the last), or to "+ Task". */
      def removeRow(): Unit =
        val rows = m.tasks.now()
        val index = rows.indexWhere(_.key == t.key)
        val remaining = rows.filterNot(_.key == t.key)
        m.tasks.set(remaining)
        FormFocus.focus(
          remaining.lift(index).orElse(remaining.lastOption).fold(mfgFieldId(k, MfgField.Tasks))(row => taskFieldId(k, row.key, TaskField.Task)),
        )
      def rowRaw(field: TaskField): Signal[Option[String]] = rawAt(rowIndex.map(index => IssueTarget.Task(k, index, field)))

      def pickTask(next: String): Unit =
        val picked = tasksById.now().get(next)
        t.state.update { ts =>
          ts.copy(
            taskId = next,
            dependsOn = ts.dependsOn - next,
            employeeId = picked.flatMap(_.defaultEmployeeId).fold("")(_.toString),
            hours =
              if t.durationEdited.now() then ts.hours
              else picked.fold(ts.hours)(task => Formats.duration(task.requiredMinutes * OrderDrafts.quantityOf(m.state.now()))),
          )
        }

      val dependencyChoices: Signal[List[(String, String)]] =
        taskStatesOf(m).combineWith(t.state.signal.map(_.taskId), deps.tasks).map { (rows, own, tasks) =>
          val names = tasks.map(task => task.id.toString -> task.name).toMap
          rows
            .filter(_._1 != t.key)
            .flatMap((_, state) => if state.taskId.isEmpty || state.taskId == own then None else names.get(state.taskId).map(state.taskId -> _))
            .distinctBy(_._1)
        }

      div(
        cls := "rounded-md border border-slate-200 bg-slate-50/60 p-3",
        div(
          cls := "grid grid-cols-1 gap-3 sm:grid-cols-[minmax(0,1fr)_12rem_7rem_auto] sm:items-start",
          field(cardTouch.spec("Task", taskFieldId(k, t.key, TaskField.Task), true, rowRaw(TaskField.Task))) { bind =>
            searchableSelect(t.state.signal.map(_.taskId), Observer[String](pickTask), taskOptions, "Cerca task…", 20, bind)
          },
          field(FieldSpec("Dipendente", s"oc-mfg-$k-task-${t.key}-employee"))(bind =>
            selectInput(t.state.signal.map(_.employeeId), Observer[String](v => t.state.update(_.copy(employeeId = v))), deps.employeeOptions)
              .amend(bind),
          ),
          field(cardTouch.spec("Durata (h:mm)", taskFieldId(k, t.key, TaskField.Duration), true, rowRaw(TaskField.Duration))) { bind =>
            textInput(
              t.state.signal.map(_.hours),
              Observer[String] { v =>
                t.durationEdited.set(true)
                t.state.update(_.copy(hours = v))
              },
              "1:30",
            ).amend(bind)
          },
          button(
            tpe := "button",
            cls := s"$btnDanger justify-center sm:mt-5 sm:py-1.5",
            aria.label := "Rimuovi task",
            title := "Rimuovi task",
            "✕",
            onClick --> (_ => removeRow()),
          ),
        ),
        checkboxChips(
          "Dipende da",
          dependencyChoices,
          t.state.signal.map(_.dependsOn),
          (id, isChecked) => t.state.update(s => s.copy(dependsOn = if isChecked then s.dependsOn + id else s.dependsOn - id)),
        ),
      )
    end renderTask

    def catalogBody(m: MfgDraft): HtmlElement =
      val k = m.key
      val cardTouch = touchOf(k)
      val catalogIdSignal = m.state.signal.map(_.catalogId)
      div(
        cls := "space-y-3",
        field(
          cardTouch.spec("Lavorazione a catalogo", mfgFieldId(k, MfgField.Catalog), true, rawFor(IssueTarget.Manufacturing(k, MfgField.Catalog))),
        ) { bind =>
          searchableSelect(
            catalogIdSignal,
            // Selecting a template also prefills the per-task employees from the catalog defaults.
            // Selecting a template also prefills its per-unit materials × N (the lines added or edited by hand stay).
            Observer[String] { v =>
              if v != m.state.now().catalogId then
                val template = catalogById(v)
                val defaults = template.fold(Map.empty[String, String])(
                  _.defaultEmployees.map(d => d.taskId.toString -> d.employeeId.toString).toMap,
                )
                m.state.update(_.copy(catalogId = v, taskEmployees = defaults))
                m.materials.update(rows => OrderDrafts.withCatalogMaterials(rows, template, OrderDrafts.quantityOf(m.state.now()), () => nextKey()))
            },
            catalogOptions,
            "Cerca per codice o nome…",
            20,
            bind,
          )
        },
        catalogPreview(catalogIdSignal, catalogs.signal),
        catalogMaterialsNote(catalogIdSignal, catalogs.signal),
        catalogTaskEmployeeOverrides(
          catalogIdSignal,
          catalogs.signal,
          deps.employeeOptions,
          m.state.signal.map(_.taskEmployees),
          (taskId, employeeId) => m.state.update(s => s.copy(taskEmployees = s.taskEmployees.updated(taskId, employeeId))),
        ),
      )
    end catalogBody

    def customBody(m: MfgDraft): HtmlElement =
      val k = m.key
      val cardTouch = touchOf(k)
      val tasksId = mfgFieldId(k, MfgField.Tasks)
      val tasksError = cardTouch.visible(tasksId, rawFor(IssueTarget.Manufacturing(k, MfgField.Tasks)))
      div(
        cls := "space-y-4",
        div(
          cls := "grid grid-cols-1 gap-4 sm:grid-cols-[minmax(0,14rem)_minmax(0,1fr)]",
          field(cardTouch.spec("Codice lavorazione", mfgFieldId(k, MfgField.Code), true, rawFor(IssueTarget.Manufacturing(k, MfgField.Code)))) {
            bind =>
              textInput(m.state.signal.map(_.code), Observer[String](v => m.state.update(_.copy(code = v))), "MFG-2026-001").amend(bind)
          },
          field(FieldSpec("Descrizione lavorazione", s"oc-mfg-$k-description"))(bind =>
            textInput(m.state.signal.map(_.description), Observer[String](v => m.state.update(_.copy(description = v))), "Opzionale").amend(bind),
          ),
        ),
        div(
          cls := "space-y-2",
          subheading("Task"),
          div(cls := "space-y-2", children <-- m.tasks.signal.split(_.key)((_, initial, _) => renderTask(m, initial))),
          div(
            button(
              idAttr := tasksId,
              tpe := "button",
              cls := btnSmall,
              "+ Task",
              aria.describedBy := s"$tasksId-error",
              onBlur --> (_ => touch.touch(tasksId)),
              onClick --> { _ =>
                val row = newTask()
                m.tasks.update(_ :+ row)
                FormFocus.focus(taskFieldId(k, row.key, TaskField.Task))
              },
            ),
            fieldError(s"$tasksId-error", tasksError),
          ),
        ),
      )
    end customBody

    /** A new multiplier: the material lines and the custom task durations that were not edited by hand follow it. */
    def setQuantity(m: MfgDraft, text: String): Unit =
      m.state.update(_.copy(quantity = text))
      OrderDrafts.parseQuantity(text).foreach { times =>
        m.materials.update(MaterialDrafts.rescale(_, times))
        m.tasks.now().foreach { row =>
          val minutes = tasksById.now().get(row.state.now().taskId).map(_.requiredMinutes)
          row.state.update(OrderDrafts.followTask(_, row.durationEdited.now(), minutes, times))
        }
      }

    def commonFields(m: MfgDraft): HtmlElement =
      val k = m.key
      val cardTouch = touchOf(k)
      val dependencyChoices: Signal[List[(String, String)]] =
        snapshots.combineWith(catalogs.signal).map { (list, templates) =>
          list.zipWithIndex.filter(_._1.key != k).map { (other, index) =>
            val code = mfgCode(other.core, catalogLookup(templates))
            other.key.toString -> s"#${index + 1} ${if code.isEmpty then "lavorazione" else code}"
          }
        }
      div(
        cls := "space-y-3",
        div(
          cls := "grid grid-cols-1 gap-4 sm:grid-cols-2",
          field(
            cardTouch.spec(
              "Quantità (×N)",
              mfgFieldId(k, MfgField.Quantity),
              true,
              "Quante volte si esegue la lavorazione: durate e materiali non modificati a mano si moltiplicano",
              rawFor(IssueTarget.Manufacturing(k, MfgField.Quantity)),
            ),
          )(bind =>
            textInput(m.state.signal.map(_.quantity), Observer[String](v => setQuantity(m, v)), "1")
              .amend(bind, inputMode := "numeric", maxLength := 3),
          ),
          div(
            field(FieldSpec("Completamento lavorazione", s"oc-mfg-$k-completion"))(bind =>
              textInput(m.state.signal.map(_.completionDate), Observer[String](v => m.state.update(_.copy(completionDate = v))), "", "date")
                .amend(bind),
            ),
            div(
              cls := "mt-1 text-xs text-slate-400",
              child.text <-- header.signal.map(_.workDeadline).map { deadline =>
                val shown = DayText.short(deadline)
                if shown.isEmpty then "Vuota = deadline lavorazione dell'ordine" else s"Vuota = deadline lavorazione $shown"
              },
            ),
          ),
          field(FieldSpec("Dipendente preferito", s"oc-mfg-$k-employee"))(bind =>
            selectInput(m.state.signal.map(_.employeeId), Observer[String](v => m.state.update(_.copy(employeeId = v))), deps.employeeOptions)
              .amend(bind),
          ),
        ),
        checkboxChips(
          "Dipende da",
          dependencyChoices,
          m.state.signal.map(_.dependsOn),
          (id, isChecked) => m.state.update(s => s.copy(dependsOn = if isChecked then s.dependsOn + id else s.dependsOn - id)),
        ),
      )
    end commonFields

    def renderMfg(m: MfgDraft): HtmlElement =
      val k = m.key
      val position = mfgs.signal.map(_.indexWhere(_.key == k) + 1).distinct
      val snapshot = snapshotSignal(m)
      val cardIssueCount = issues.map(_.count(issue => mfgKeyOf(issue.target).contains(k))).distinct
      val showStatus = touchOf(k).submittedSignal.combineWith(seen.signal).map((attempted, collapsed) => attempted || collapsed.contains(k)).distinct
      val cardTotals = snapshot
        .combineWith(catalogs.signal, materialsContext.signal)
        .map((s, templates, context) => OrderDrafts.mfgTotals(s, catalogLookup(templates), context))
      val titleText = m.state.signal.combineWith(catalogs.signal).map((core, templates) => mfgLabel(core, catalogLookup(templates)))
      // ×N next to a named manufacturing.
      val titleQuantity = m.state.signal.combineWith(titleText).map((core, text) => Option.when(text.nonEmpty)(OrderDrafts.quantityOf(core)))
      val completion = m.state.signal.map(_.completionDate).combineWith(header.signal.map(_.workDeadline)).map { (own, deadline) =>
        if own.trim.nn.nonEmpty then own else deadline
      }
      val summary = div(
        cls := "flex flex-wrap items-center gap-x-3 gap-y-1",
        span(cls := "text-sm font-semibold tabular-nums text-slate-400", child.text <-- position.map(n => s"#$n")),
        span(
          cls := "min-w-0 truncate text-sm font-medium",
          cls <-- titleText.map(text => if text.isEmpty then "italic text-slate-400" else "text-slate-800"),
          child.text <-- titleText.map(text => if text.isEmpty then "Nuova lavorazione" else text),
        ),
        child.maybe <-- titleQuantity.distinct.map(_.flatMap(quantityBadge)),
        span(cls := "text-xs tabular-nums text-slate-500", child.text <-- cardTotals.map(totalsText)),
        child.maybe <-- completion.map(day =>
          Option.when(DayText.short(day).nonEmpty)(span(cls := "text-xs text-slate-500", s"entro ${DayText.short(day)}")),
        ),
        child <-- showStatus.combineWith(cardIssueCount).map((show, count) => if show then statusIcon(count) else emptyNode),
      )
      accordionCard(
        s"oc-mfg-$k-panel",
        expanded.signal.map(_.contains(k)),
        () => toggle(k),
        summary,
        button(tpe := "button", cls := btnDanger, "Rimuovi", onClick --> (_ => requestRemove(k))),
        showStatus.combineWith(cardIssueCount).map((show, count) => show && count > 0),
      )(
        div(
          cls := "space-y-5",
          labelled(
            "Tipo lavorazione",
            segmented(
              m.state.signal.map(_.mode),
              Observer[String] { v =>
                m.state.update(_.copy(mode = v))
                if v == "custom" && m.tasks.now().isEmpty then m.tasks.set(List(newTask()))
              },
              modeOptions,
              aria.label := "Tipo lavorazione",
            ),
          ),
          // `.distinct` is required: without it every keystroke re-emits the state Var, recreating this subtree and stealing focus.
          child <-- m.state.signal.map(_.mode).distinct.map {
            case "catalog" => catalogBody(m)
            case _ => customBody(m)
          },
          commonFields(m),
          MaterialsEditor.draftEditor(m.materials, () => nextKey(), materialsContext.writer, mfgFieldId(k, MfgField.Materials), stockWarnings),
          div(
            cls := "flex justify-end border-t border-slate-100 pt-3",
            button(tpe := "button", cls := btnGhost, "Fatto ✓", onClick --> (_ => done(m))),
          ),
        ),
      )
    end renderMfg

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
            if list.isEmpty then p(cls := "text-sm text-slate-400", "Nessuna lavorazione aggiunta.")
            else
              ul(
                cls := "divide-y divide-slate-100 rounded-md border border-slate-200",
                list.zipWithIndex.map { (s, index) =>
                  val lookup = catalogLookup(templates)
                  val label = mfgLabel(s.core, lookup)
                  val day = DayText.short(if s.core.completionDate.trim.nn.nonEmpty then s.core.completionDate else deadline)
                  li(
                    cls := "flex flex-col gap-0.5 px-3 py-2 text-sm sm:flex-row sm:items-baseline sm:justify-between sm:gap-3",
                    span(
                      cls := "flex min-w-0 items-baseline gap-1.5",
                      span(
                        cls := "min-w-0 truncate",
                        span(cls := "mr-1.5 tabular-nums text-slate-400", s"#${index + 1}"),
                        span(
                          cls := (if label.isEmpty then "italic text-slate-400" else "text-slate-800"),
                          if label.isEmpty then "Nuova lavorazione" else label,
                        ),
                      ),
                      Option.when(label.nonEmpty)(OrderDrafts.quantityOf(s.core)).flatMap(quantityBadge),
                    ),
                    span(
                      cls := "shrink-0 text-xs tabular-nums text-slate-500",
                      totalsText(OrderDrafts.mfgTotals(s, lookup, context)) + (if day.isEmpty then "" else s" · entro $day"),
                    ),
                  )
                },
              )
        },
        div(
          cls := "mt-2 flex justify-between gap-3 px-3 text-sm",
          span(cls := "text-slate-500", "Totale ordine"),
          span(cls := "font-semibold tabular-nums text-slate-800", child.text <-- totals.map(totalsText)),
        ),
        div(cls := "mt-3", child <-- stockWarningList.map(stockWarningsBox)),
      )

    val sidebarWarnings = div(
      child <-- stockWarningList.map(list => if list.isEmpty then emptyNode else div(cls := "border-t border-slate-100 pt-3", stockWarningsBox(list))),
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

    val sidebar =
      card(
        cls := "space-y-4 p-4",
        h2(cls := "text-sm font-semibold text-slate-800", "Riepilogo"),
        div(
          cls := "space-y-1.5",
          summaryRow("Numero", header.signal.map(h => orDash(h.number))),
          summaryRow("Cliente", customerName),
          summaryRow("Consegna", header.signal.map(h => orDash(DayText.long(h.deliveryDate)))),
          summaryRow("Deadline", header.signal.map(h => orDash(DayText.long(h.workDeadline)))),
          summaryRow("Priorità", header.signal.map(h => if h.priority == "urgent" then "Urgente" else "Normale")),
        ),
        div(
          cls := "space-y-1.5 border-t border-slate-100 pt-3",
          summaryRow("Lavorazioni", mfgs.signal.map(_.size.toString)),
          summaryRow("Ore totali", totals.map(t => hoursText(t.minutes))),
          summaryRow("Costo materiali", totals.map(t => WarehouseFormats.formatEuro(t.materialsCost))),
        ),
        sidebarWarnings,
        div(
          cls := "space-y-2 border-t border-slate-100 pt-3",
          div(cls := subheadingCls, child.text <-- issues.map(list => if list.isEmpty then "Da completare" else s"Da completare (${list.size})")),
          issueList(issues, _.message, emphasized, reveal),
        ),
        formActions("Crea ordine", "Creazione…", submitting.signal, Some(() => requestLeave()), () => submit()),
      )

    val mobileBar =
      div(
        cls := "flex items-center gap-3",
        div(
          cls := "min-w-0 flex-1 truncate text-sm tabular-nums text-slate-700",
          child.text <-- mfgs.signal.map(_.size).combineWith(totals).map { (count, t) =>
            s"$count lav. · ${hoursText(t.minutes)} · ${WarehouseFormats.formatEuro(t.materialsCost)}"
          },
        ),
        child.maybe <-- issues.map(_.size).distinct.map { count =>
          Option.when(count > 0)(
            button(
              tpe := "button",
              cls := "shrink-0 rounded-full bg-amber-50 px-2.5 py-1 text-xs font-medium text-amber-700 hover:bg-amber-100",
              aria.label := s"$count elementi da completare: vai al primo",
              s"⚠ $count",
              onClick --> (_ => currentIssues().headOption.foreach(reveal)),
            ),
          )
        },
        button(
          tpe := "button",
          cls := s"$btnPrimary min-h-10 shrink-0",
          disabled <-- submitting.signal,
          aria.busy <-- submitting.signal,
          child.text <-- submitting.signal.map(busy => if busy then "Creazione…" else "Crea ordine"),
          onClick --> (_ => submit()),
        ),
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
          val label = pending.flatMap(key => list.find(_.key == key)).map(s => mfgLabel(s.core, catalogLookup(templates))).getOrElse("")
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

    div(
      cls := "space-y-6",
      NavigationGuard.guard("Abbandonare il nuovo ordine?", "L'ordine non è stato creato: i dati inseriti andranno persi.", () => isDirty()),
      onMountCallback(_ => dom.window.scrollTo(0, 0)),
      deps.catalogs --> catalogs.writer,
      deps.tasks --> (list => tasksById.set(list.map(task => task.id.toString -> task).toMap)),
      deps.existingOrderNumbers --> (numbers => header.update(_.withGeneratedNumber(GeneratedCodes.next("ORD", numbers)))),
      div(
        button(tpe := "button", cls := btnLink, "← Torna agli ordini", onClick --> (_ => requestLeave())),
        h1(cls := "mt-3 text-xl font-semibold text-slate-900", "Nuovo ordine"),
        requiredLegend.amend(cls := "mt-1"),
      ),
      summaryLayout(div(cls := "space-y-6", headerSection, mfgsSection, confirmSection), sidebar, mobileBar),
      leaveDialog,
      removeDialog,
    )
  end apply
end OrderCreateView
