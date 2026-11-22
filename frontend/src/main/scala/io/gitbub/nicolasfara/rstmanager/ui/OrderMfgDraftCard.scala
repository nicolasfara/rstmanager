package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.util.Try

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.OrderDrafts.{ IssueTarget, MfgCoreState, MfgField, MfgSnapshot, StockWarning, TaskField, TaskState }

/** Mutable draft of a custom task row: `durationEdited` stops the duration from following the picked task. */
private[ui] final case class TaskDraft(key: Int, state: Var[TaskState], durationEdited: Var[Boolean])

/** Mutable draft of a new manufacturing. `tasks` and `materials` are separate `Var`s because they undergo their own add/remove mutations. */
private[ui] final case class MfgDraft(key: Int, state: Var[MfgCoreState], tasks: Var[List[TaskDraft]], materials: Var[List[MaterialDrafts.Row]])

/**
 * The card of a new manufacturing draft (catalog or custom, task rows, multiplier ×N, completion date, preferred employee, materials), shared by the
 * "Nuovo ordine" page and the new manufacturings of the "Modifica ordine" page. Picking a template prefills its per-unit materials × N; changing N
 * rescales only the material lines and task durations not edited by hand. The page owns the drafts, the validation and the navigation between cards;
 * it hands them to the card through a [[OrderMfgDraftCard.Context]], plus the header actions and the footer of the card.
 */
private[ui] object OrderMfgDraftCard:

  private val modeOptions = List("catalog" -> "Da catalogo", "custom" -> "Personalizzata")

  private val labelCls = "mb-1 block text-xs font-medium text-slate-500"

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** A label over a control that is not a labelable element (a segmented group). */
  private[ui] def labelled(text: String, control: HtmlElement): HtmlElement = div(div(cls := labelCls, text), control)

  /** The DOM ids of the draft cards of a page, under `prefix` ("oc" on the create page, "oe-new" on the edit page). */
  final case class Ids(prefix: String):
    def mfgField(key: Int, field: MfgField): String = field match
      case MfgField.Catalog => s"$prefix-mfg-$key-catalog"
      case MfgField.Code => s"$prefix-mfg-$key-code"
      case MfgField.Quantity => s"$prefix-mfg-$key-quantity"
      case MfgField.Tasks => s"$prefix-mfg-$key-tasks"
      case MfgField.Materials => s"$prefix-mfg-$key-materials"

    /** Id of a field of the task row with draft key `taskKey` (stable across removals, unlike the row index). */
    def taskField(key: Int, taskKey: Int, field: TaskField): String = field match
      case TaskField.Task => s"$prefix-mfg-$key-task-$taskKey-task"
      case TaskField.Duration => s"$prefix-mfg-$key-task-$taskKey-duration"

    def taskEmployee(key: Int, taskKey: Int): String = s"$prefix-mfg-$key-task-$taskKey-employee"

    /** Id of another element of card `key`: "panel", "description", "completion", "employee". */
    def of(key: Int, name: String): String = s"$prefix-mfg-$key-$name"

    /** The DOM id an issue of a card points to (a task issue resolved to its row key by `taskKeyAt`, else the task list). */
    def domId(target: IssueTarget, taskKeyAt: (Int, Int) => Option[Int], header: OrderDrafts.HeaderField => String, addMfg: String): String =
      target match
        case IssueTarget.Header(field) => header(field)
        case IssueTarget.NoManufacturings => addMfg
        case IssueTarget.Manufacturing(key, field) => mfgField(key, field)
        case IssueTarget.Task(key, index, field) => taskKeyAt(key, index).fold(mfgField(key, MfgField.Tasks))(taskField(key, _, field))

    /** The ids of the fields of card `m` that carry an error, so they can all be marked as visited. */
    def cardFieldIds(m: MfgDraft): List[String] =
      List(MfgField.Catalog, MfgField.Code, MfgField.Quantity, MfgField.Tasks, MfgField.Materials).map(mfgField(m.key, _)) ++
        m.tasks.now().flatMap(row => List(TaskField.Task, TaskField.Duration).map(taskField(m.key, row.key, _)))
  end Ids

  /**
   * What a draft card needs from its page.
   *
   * @param touchOf
   *   the touch tracker of the fields of card `key`
   * @param issueAt
   *   the inline message (if any) of the issue at a target (a signal, so a task row follows its current index)
   * @param cardIssueCount
   *   how many issues card `key` has
   * @param siblings
   *   the snapshots of every draft of the page (its position, the "Dipende da" choices)
   * @param positionOffset
   *   added to the position of the card in `siblings` ("#n"): the saved manufacturings listed before the drafts
   * @param showMfgDependencies
   *   whether the card offers "Dipende da" between manufacturings; otherwise a hint says they are set after the add
   * @param stockWarnings
   *   the non-blocking stock warnings of the page by material, shown on the material lines
   */
  final case class Context(
      ids: Ids,
      touchOf: Int => FormTouch,
      issueAt: Signal[IssueTarget] => Signal[Option[String]],
      cardIssueCount: Int => Signal[Int],
      catalogs: Signal[List[ManufacturingCatalogResponse]],
      catalogById: String => Option[ManufacturingCatalogResponse],
      tasks: Signal[List[TaskResponse]],
      taskById: String => Option[TaskResponse],
      employeeOptions: Signal[List[(String, String)]],
      workDeadline: Signal[String],
      materialsContext: Signal[MaterialDrafts.Context],
      materialsSink: Observer[MaterialDrafts.Context],
      stockWarnings: Signal[Map[UUID, StockWarning]],
      nextKey: () => Int,
      siblings: Signal[List[MfgSnapshot]],
      positionOffset: Signal[Int],
      showMfgDependencies: Boolean,
      expanded: Signal[Option[Int]],
      seen: Signal[Set[Int]],
      onToggle: Int => Unit,
  ):
    def issueFor(target: IssueTarget): Signal[Option[String]] = issueAt(Signal.fromValue(target))

    val catalogOptions: Signal[List[(String, String)]] = catalogs.map(_.map(c => c.id.toString -> s"${c.code} · ${c.name}"))

    val taskOptions: Signal[List[(String, String)]] = tasks.map(_.map(t => t.id.toString -> s"${t.name} (${Formats.duration(t.requiredMinutes)}h)"))
  end Context

  // ---- Drafts ------------------------------------------------------------------------------------

  def newTask(nextKey: () => Int): TaskDraft = TaskDraft(nextKey(), Var(TaskState("", "", Set.empty, "")), Var(false))

  /** A blank draft of `mode` ("catalog" or "custom", which starts with one task row). */
  def newDraft(mode: String, nextKey: () => Int): MfgDraft =
    MfgDraft(
      nextKey(),
      Var(MfgCoreState(mode, "", "", "", "", "", Set.empty, Map.empty)),
      Var(if mode == "custom" then List(newTask(nextKey)) else Nil),
      Var(Nil),
    )

  def snapshotOf(m: MfgDraft): MfgSnapshot = MfgSnapshot(m.key, m.state.now(), m.tasks.now().map(_.state.now()), m.materials.now())

  def taskStatesOf(m: MfgDraft): Signal[List[(Int, TaskState)]] =
    m.tasks.signal.flatMapSwitch(rows => combineAll(rows.map(row => row.state.signal.map(row.key -> _))))

  def snapshotSignal(m: MfgDraft): Signal[MfgSnapshot] =
    m.state.signal
      .combineWith(taskStatesOf(m), m.materials.signal)
      .map((core, tasks, rows) => MfgSnapshot(m.key, core, tasks.map(_._2), rows))

  def snapshots(drafts: Signal[List[MfgDraft]]): Signal[List[MfgSnapshot]] = drafts.flatMapSwitch(list => combineAll(list.map(snapshotSignal)))

  // ---- Catalog helpers ---------------------------------------------------------------------------

  def catalogLookup(list: List[ManufacturingCatalogResponse])(rawId: String): Option[ManufacturingCatalogResponse] =
    parseUuid(rawId).flatMap(id => list.find(_.id == id))

  /** Effective minutes of each template task: the template override, else the task-catalog duration. */
  private def effectiveMinutes(template: ManufacturingCatalogResponse): List[(TaskResponse, Int)] =
    val overrides = template.taskDurations.map(entry => entry.taskId -> entry.minutes).toMap
    template.tasks.map(task => task -> overrides.getOrElse(task.id, task.requiredMinutes))

  /** A compact read-only preview of the selected catalog template (tasks with their effective durations, total, dependencies). */
  def catalogPreview(catalogIdSignal: Signal[String], catalogs: Signal[List[ManufacturingCatalogResponse]]): HtmlElement =
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
   * The per-unit materials of the selected template: what is prefilled into the draft (× N), and a warning for the archived or unknown ones, which
   * are not prefilled. Hidden while no template (or one without materials) is selected.
   */
  def catalogMaterialsNote(catalogIdSignal: Signal[String], catalogs: Signal[List[ManufacturingCatalogResponse]]): HtmlElement =
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

  /**
   * Per-task employee selects for a catalog-mode manufacturing: one row per template task, prefilled with the catalog default and overridable before
   * submit. Hidden until a valid template is selected.
   */
  def catalogTaskEmployeeOverrides(
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

  // ---- Task row ----------------------------------------------------------------------------------

  /** The DOM ids of the three inputs of a task row. */
  final case class TaskRowIds(task: String, employee: String, duration: String)

  /** The grid of a task row (task, employee, duration, then an action), shared by the draft rows and the edit page's "+ Task" row. */
  val taskRowGridCls = "grid grid-cols-1 gap-3 sm:grid-cols-[minmax(0,1fr)_12rem_7rem_auto] sm:items-start"

  /**
   * The task (searchable), employee and duration inputs of task row `t`. Picking a task proposes its default employee and, until the duration was
   * typed, its catalog duration × `times()` (the multiplier ×N of the manufacturing: durations are the totals for the N units).
   */
  def taskInputs(
      t: TaskDraft,
      ids: TaskRowIds,
      touch: FormTouch,
      taskError: Signal[Option[String]],
      durationError: Signal[Option[String]],
      taskOptions: Signal[List[(String, String)]],
      employeeOptions: Signal[List[(String, String)]],
      taskById: String => Option[TaskResponse],
      times: () => Int,
  ): List[HtmlElement] =
    def pickTask(next: String): Unit =
      val picked = taskById(next)
      t.state.update { ts =>
        ts.copy(
          taskId = next,
          dependsOn = ts.dependsOn - next,
          employeeId = picked.flatMap(_.defaultEmployeeId).fold("")(_.toString),
          hours = if t.durationEdited.now() then ts.hours else picked.fold(ts.hours)(task => Formats.duration(task.requiredMinutes * times())),
        )
      }
    List(
      field(touch.spec("Task", ids.task, true, taskError)) { bind =>
        searchableSelect(t.state.signal.map(_.taskId), Observer[String](pickTask), taskOptions, "Cerca task…", 20, bind)
      },
      field(FieldSpec("Dipendente", ids.employee))(bind =>
        selectInput(t.state.signal.map(_.employeeId), Observer[String](v => t.state.update(_.copy(employeeId = v))), employeeOptions).amend(bind),
      ),
      field(touch.spec("Durata (h:mm)", ids.duration, true, durationError)) { bind =>
        textInput(
          t.state.signal.map(_.hours),
          Observer[String] { v =>
            t.durationEdited.set(true)
            t.state.update(_.copy(hours = v))
          },
          "1:30",
        ).amend(bind)
      },
    )
  end taskInputs

  private def renderTask(ctx: Context, m: MfgDraft, t: TaskDraft): HtmlElement =
    val k = m.key
    val ids = ctx.ids
    val rowIndex = m.tasks.signal.map(_.indexWhere(_.key == t.key)).distinct

    /** Removes the row and moves the focus to the next row's task (the previous one when it was the last), or to "+ Task". */
    def removeRow(): Unit =
      val rows = m.tasks.now()
      val index = rows.indexWhere(_.key == t.key)
      val remaining = rows.filterNot(_.key == t.key)
      m.tasks.set(remaining)
      FormFocus.focus(
        remaining.lift(index).orElse(remaining.lastOption).fold(ids.mfgField(k, MfgField.Tasks))(row => ids.taskField(k, row.key, TaskField.Task)),
      )
    def rowRaw(field: TaskField): Signal[Option[String]] = ctx.issueAt(rowIndex.map(index => IssueTarget.Task(k, index, field)))

    val dependencyChoices: Signal[List[(String, String)]] =
      taskStatesOf(m).combineWith(t.state.signal.map(_.taskId), ctx.tasks).map { (rows, own, tasks) =>
        val names = tasks.map(task => task.id.toString -> task.name).toMap
        rows
          .filter(_._1 != t.key)
          .flatMap((_, state) => if state.taskId.isEmpty || state.taskId == own then None else names.get(state.taskId).map(state.taskId -> _))
          .distinctBy(_._1)
      }

    div(
      cls := "rounded-md border border-slate-200 bg-slate-50/60 p-3",
      div(
        cls := taskRowGridCls,
        taskInputs(
          t,
          TaskRowIds(ids.taskField(k, t.key, TaskField.Task), ids.taskEmployee(k, t.key), ids.taskField(k, t.key, TaskField.Duration)),
          ctx.touchOf(k),
          rowRaw(TaskField.Task),
          rowRaw(TaskField.Duration),
          ctx.taskOptions,
          ctx.employeeOptions,
          ctx.taskById,
          () => OrderDrafts.quantityOf(m.state.now()),
        ),
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

  // ---- Card bodies -------------------------------------------------------------------------------

  private def catalogBody(ctx: Context, m: MfgDraft): HtmlElement =
    val k = m.key
    val catalogIdSignal = m.state.signal.map(_.catalogId)
    div(
      cls := "space-y-3",
      field(
        ctx
          .touchOf(k)
          .spec(
            "Lavorazione a catalogo",
            ctx.ids.mfgField(k, MfgField.Catalog),
            true,
            ctx.issueFor(IssueTarget.Manufacturing(k, MfgField.Catalog)),
          ),
      ) { bind =>
        searchableSelect(
          catalogIdSignal,
          // Selecting a template also prefills the per-task employees from the catalog defaults and its per-unit materials × N (the lines added or
          // edited by hand stay).
          Observer[String] { v =>
            if v != m.state.now().catalogId then
              val template = ctx.catalogById(v)
              val defaults = template.fold(Map.empty[String, String])(_.defaultEmployees.map(d => d.taskId.toString -> d.employeeId.toString).toMap)
              m.state.update(_.copy(catalogId = v, taskEmployees = defaults))
              m.materials.update(rows => OrderDrafts.withCatalogMaterials(rows, template, OrderDrafts.quantityOf(m.state.now()), ctx.nextKey))
          },
          ctx.catalogOptions,
          "Cerca per codice o nome…",
          20,
          bind,
        )
      },
      catalogPreview(catalogIdSignal, ctx.catalogs),
      catalogMaterialsNote(catalogIdSignal, ctx.catalogs),
      catalogTaskEmployeeOverrides(
        catalogIdSignal,
        ctx.catalogs,
        ctx.employeeOptions,
        m.state.signal.map(_.taskEmployees),
        (taskId, employeeId) => m.state.update(s => s.copy(taskEmployees = s.taskEmployees.updated(taskId, employeeId))),
      ),
    )
  end catalogBody

  private def customBody(ctx: Context, m: MfgDraft): HtmlElement =
    val k = m.key
    val cardTouch = ctx.touchOf(k)
    val tasksId = ctx.ids.mfgField(k, MfgField.Tasks)
    val tasksError = cardTouch.visible(tasksId, ctx.issueFor(IssueTarget.Manufacturing(k, MfgField.Tasks)))
    div(
      cls := "space-y-4",
      div(
        cls := "grid grid-cols-1 gap-4 sm:grid-cols-[minmax(0,14rem)_minmax(0,1fr)]",
        field(
          cardTouch.spec("Codice lavorazione", ctx.ids.mfgField(k, MfgField.Code), true, ctx.issueFor(IssueTarget.Manufacturing(k, MfgField.Code))),
        )(bind => textInput(m.state.signal.map(_.code), Observer[String](v => m.state.update(_.copy(code = v))), "MFG-2026-001").amend(bind)),
        field(FieldSpec("Descrizione lavorazione", ctx.ids.of(k, "description")))(bind =>
          textInput(m.state.signal.map(_.description), Observer[String](v => m.state.update(_.copy(description = v))), "Opzionale").amend(bind),
        ),
      ),
      div(
        cls := "space-y-2",
        subheading("Task"),
        div(cls := "space-y-2", children <-- m.tasks.signal.split(_.key)((_, initial, _) => renderTask(ctx, m, initial))),
        div(
          button(
            idAttr := tasksId,
            tpe := "button",
            cls := btnSmall,
            "+ Task",
            aria.describedBy := s"$tasksId-error",
            onBlur --> (_ => cardTouch.touch(tasksId)),
            onClick --> { _ =>
              val row = newTask(ctx.nextKey)
              m.tasks.update(_ :+ row)
              FormFocus.focus(ctx.ids.taskField(k, row.key, TaskField.Task))
            },
          ),
          fieldError(s"$tasksId-error", tasksError),
        ),
      ),
    )
  end customBody

  /** A new multiplier: the material lines and the custom task durations that were not edited by hand follow it. */
  private def setQuantity(ctx: Context, m: MfgDraft, text: String): Unit =
    m.state.update(_.copy(quantity = text))
    OrderDrafts.parseQuantity(text).foreach { times =>
      m.materials.update(MaterialDrafts.rescale(_, times))
      m.tasks.now().foreach { row =>
        val minutes = ctx.taskById(row.state.now().taskId).map(_.requiredMinutes)
        row.state.update(OrderDrafts.followTask(_, row.durationEdited.now(), minutes, times))
      }
    }

  private def commonFields(ctx: Context, m: MfgDraft): HtmlElement =
    val k = m.key
    val cardTouch = ctx.touchOf(k)
    val dependencies: HtmlElement =
      if ctx.showMfgDependencies then
        val choices: Signal[List[(String, String)]] =
          ctx.siblings.combineWith(ctx.catalogs).map { (list, templates) =>
            list.zipWithIndex.filter(_._1.key != k).map { (other, index) =>
              val code = OrderCreateModel.mfgCode(other.core, catalogLookup(templates))
              other.key.toString -> s"#${index + 1} ${if code.isEmpty then "lavorazione" else code}"
            }
          }
        checkboxChips(
          "Dipende da",
          choices,
          m.state.signal.map(_.dependsOn),
          (id, isChecked) => m.state.update(s => s.copy(dependsOn = if isChecked then s.dependsOn + id else s.dependsOn - id)),
        )
      else hint("Le dipendenze dalle altre lavorazioni si impostano dopo l'aggiunta.")
    div(
      cls := "space-y-3",
      div(
        cls := "grid grid-cols-1 gap-4 sm:grid-cols-2",
        field(
          cardTouch.spec(
            "Quantità (×N)",
            ctx.ids.mfgField(k, MfgField.Quantity),
            true,
            "Quante volte si esegue la lavorazione: durate e materiali non modificati a mano si moltiplicano",
            ctx.issueFor(IssueTarget.Manufacturing(k, MfgField.Quantity)),
          ),
        )(bind =>
          textInput(m.state.signal.map(_.quantity), Observer[String](v => setQuantity(ctx, m, v)), "1")
            .amend(bind, inputMode := "numeric", maxLength := 3),
        ),
        div(
          field(FieldSpec("Completamento lavorazione", ctx.ids.of(k, "completion")))(bind =>
            textInput(m.state.signal.map(_.completionDate), Observer[String](v => m.state.update(_.copy(completionDate = v))), "", "date")
              .amend(bind),
          ),
          div(
            cls := "mt-1 text-xs text-slate-400",
            child.text <-- ctx.workDeadline.map { deadline =>
              val shown = DayText.short(deadline)
              if shown.isEmpty then "Vuota = deadline lavorazione dell'ordine" else s"Vuota = deadline lavorazione $shown"
            },
          ),
        ),
        field(FieldSpec("Dipendente preferito", ctx.ids.of(k, "employee")))(bind =>
          selectInput(m.state.signal.map(_.employeeId), Observer[String](v => m.state.update(_.copy(employeeId = v))), ctx.employeeOptions)
            .amend(bind),
        ),
      ),
      dependencies,
    )
  end commonFields

  // ---- Card --------------------------------------------------------------------------------------

  /** The accordion card of draft `m`: the summary line ("#n", title, totals, "entro …", status), `actions` next to it, the body, then `footer`. */
  def apply(ctx: Context, m: MfgDraft, actions: Modifier[HtmlElement], footer: HtmlElement): HtmlElement =
    val k = m.key
    val position = ctx.siblings.combineWith(ctx.positionOffset).map((list, offset) => list.indexWhere(_.key == k) + 1 + offset).distinct
    val snapshot = snapshotSignal(m)
    val cardIssueCount = ctx.cardIssueCount(k)
    val showStatus =
      ctx.touchOf(k).submittedSignal.combineWith(ctx.seen).map((attempted, collapsed) => attempted || collapsed.contains(k)).distinct
    val cardTotals = snapshot
      .combineWith(ctx.catalogs, ctx.materialsContext)
      .map((s, templates, context) => OrderDrafts.mfgTotals(s, catalogLookup(templates), context))
    val titleText = m.state.signal.combineWith(ctx.catalogs).map((core, templates) => OrderCreateModel.mfgLabel(core, catalogLookup(templates)))
    // ×N next to a named manufacturing.
    val titleQuantity = m.state.signal.combineWith(titleText).map((core, text) => Option.when(text.nonEmpty)(OrderDrafts.quantityOf(core)))
    val completion = m.state.signal.map(_.completionDate).combineWith(ctx.workDeadline).map { (own, deadline) =>
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
      child.maybe <-- titleQuantity.distinct.map(_.flatMap(OrderFormView.quantityBadge)),
      span(cls := "text-xs tabular-nums text-slate-500", child.text <-- cardTotals.map(OrderCreateModel.totalsText)),
      child.maybe <-- completion.map(day =>
        Option.when(DayText.short(day).nonEmpty)(span(cls := "text-xs text-slate-500", s"entro ${DayText.short(day)}")),
      ),
      child <-- showStatus.combineWith(cardIssueCount).map((show, count) => if show then statusIcon(count) else emptyNode),
    )
    accordionCard(
      ctx.ids.of(k, "panel"),
      ctx.expanded.map(_.contains(k)),
      () => ctx.onToggle(k),
      summary,
      actions,
      showStatus.combineWith(cardIssueCount).map((show, count) => show && count > 0),
    )(
      div(
        cls := "space-y-5",
        labelled(
          "Tipo lavorazione",
          segmented(
            m.state.signal.map(_.mode),
            Observer[String] { v =>
              val core = m.state.now()
              if v != core.mode then
                m.state.update(_.copy(mode = v))
                // Leaving catalog mode drops the untouched lines prefilled from the template; returning re-applies it.
                m.materials.update(rows =>
                  OrderDrafts.materialsForMode(rows, v, ctx.catalogById(core.catalogId), OrderDrafts.quantityOf(core), ctx.nextKey),
                )
              if v == "custom" && m.tasks.now().isEmpty then m.tasks.set(List(newTask(ctx.nextKey)))
            },
            modeOptions,
            aria.label := "Tipo lavorazione",
          ),
        ),
        // `.distinct` is required: without it every keystroke re-emits the state Var, recreating this subtree and stealing focus.
        child <-- m.state.signal.map(_.mode).distinct.map {
          case "catalog" => catalogBody(ctx, m)
          case _ => customBody(ctx, m)
        },
        commonFields(ctx, m),
        MaterialsEditor.draftEditor(m.materials, ctx.nextKey, ctx.materialsSink, ctx.ids.mfgField(k, MfgField.Materials), ctx.stockWarnings),
        footer,
      ),
    )
  end apply
end OrderMfgDraftCard
