package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.OrderDrafts.{ TaskField, TaskState }

/**
 * Pure (DOM-free) state of the "Modifica ordine" page: the value edits of a saved order (header, manufacturings, scheduled tasks, materials) held as
 * a draft next to the server values they started from.
 *
 * Every row keeps its `baseline` (the server values) and its `current` (what the user typed). A field is a change while the two differ; [[changes]]
 * lists them, [[issues]] the ones that cannot be sent, and [[effects]] the requests that save them, as data executed by the view. After any server
 * response the draft is [[rebase]]d onto the new snapshot: the edits survive, the untouched fields take the server values, so a structural operation
 * (add or remove a manufacturing or a task) never loses the unsaved edits.
 *
 * The multiplier ×N of a saved manufacturing (`quantity`) is fixed at its creation (the server has no request to change it): it is read from the last
 * snapshot, so a rebase follows it, and it never makes a change or an effect. Its task durations and material quantities are already the totals for
 * the N units.
 */
object OrderEditDraft:

  // ---- State -------------------------------------------------------------------------------------

  /** The editable order fields (`OrderUpdateRequest`). `deadline` is the work deadline as `yyyy-MM-dd` date-input text. */
  final case class HeaderValues(priority: String, deadline: String, description: String) derives CanEqual

  /** The editable fields of a saved manufacturing. `dependsOn` holds the ids (as text) of the other manufacturings of the order. */
  final case class MfgValues(description: String, deadline: String, status: String, employeeId: String, dependsOn: Set[String]) derives CanEqual

  /** The editable fields of a scheduled task: durations as `h:mm` text, `dependsOn` the template-task ids (as text) of the same manufacturing. */
  final case class TaskValues(expected: String, completed: String, employeeId: String, dependsOn: Set[String]) derives CanEqual

  /** A scheduled task as last received (`server`) with its baseline and the user's values. */
  final case class TaskRow(server: ScheduledTaskDto, baseline: TaskValues, current: TaskValues):
    def id: UUID = server.id
    def templateTaskId: UUID = server.taskId

  /** A saved manufacturing as last received (`server`), its values, its task rows and its material lines (baseline and current). */
  final case class MfgRow(
      server: ManufacturingResponse,
      baseline: MfgValues,
      current: MfgValues,
      tasks: List[TaskRow],
      baseMaterials: List[MaterialDrafts.Row],
      materials: List[MaterialDrafts.Row],
  ):
    def id: UUID = server.id
    def code: String = server.code

    /** The multiplier ×N of the manufacturing (read-only after its creation). */
    def quantity: Int = server.quantity

    /** Whether the manufacturing still needs its materials from the stock: a completed one has already unloaded them. */
    def needsStock: Boolean = server.status != "completed"

  /** The whole draft: the last order snapshot received, the header values, and one row per manufacturing in the server order. */
  final case class Draft(order: OrderResponse, baseline: HeaderValues, header: HeaderValues, mfgs: List[MfgRow])

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value.trim).nn).toOption

  private def optionalId(id: Option[UUID]): String = id.fold("")(_.toString)

  /** The header values of `order`. */
  def headerOf(order: OrderResponse): HeaderValues =
    HeaderValues(order.priority, order.promisedDeliveryDate.fold("")(DayText.dayOf), order.description.getOrElse(""))

  /** The values of manufacturing `m` of `order` (its prerequisites come from the order-level graph). */
  def mfgValuesOf(order: OrderResponse, m: ManufacturingResponse): MfgValues =
    val dependsOn = order.dependencies.find(_.manufacturingId == m.id).fold(Set.empty[String])(_.dependsOn.map(_.toString).toSet)
    MfgValues(m.description.getOrElse(""), DayText.dayOf(m.completionDate), m.status, optionalId(m.preferredEmployeeId), dependsOn)

  /** The values of scheduled task `t` of manufacturing `m` (its prerequisites come from the manufacturing's task graph). */
  def taskValuesOf(m: ManufacturingResponse, t: ScheduledTaskDto): TaskValues =
    val dependsOn = m.dependencies.find(_.taskId == t.taskId).fold(Set.empty[String])(_.dependsOn.map(_.toString).toSet)
    TaskValues(Formats.duration(t.expectedMinutes), Formats.duration(t.completedMinutes.getOrElse(0)), optionalId(t.preferredEmployeeId), dependsOn)

  private def freshMfg(order: OrderResponse, m: ManufacturingResponse, newKey: () => Int): MfgRow =
    val values = mfgValuesOf(order, m)
    val rows = MaterialDrafts.rowsOf(m.materials, newKey)
    MfgRow(m, values, values, m.tasks.map(t => freshTask(m, t)), rows, rows)

  private def freshTask(m: ManufacturingResponse, t: ScheduledTaskDto): TaskRow =
    val values = taskValuesOf(m, t)
    TaskRow(t, values, values)

  /** A draft without edits for `order`; `newKey` mints the keys of the material rows. */
  def fromOrder(order: OrderResponse, newKey: () => Int): Draft =
    val header = headerOf(order)
    Draft(order, header, header, order.manufacturings.map(freshMfg(order, _, newKey)))

  // ---- Rebase ------------------------------------------------------------------------------------

  /** The user's value when it differs from the old baseline, else the server's. */
  private def pick[A](baseline: A, current: A, server: A)(using CanEqual[A, A]): A = if current != baseline then current else server

  /** The material lines as compared for a change: the non-blank rows without their keys. */
  private def linesOf(rows: List[MaterialDrafts.Row]): List[(String, String, String)] =
    rows.filterNot(MaterialDrafts.isBlank).map(row => (row.materialId.trim.nn, row.quantity.trim.nn, row.unitPrice.trim.nn))

  /** Server material lines as compared to detect a save: material, quantity and frozen price. */
  private def savedLines(lines: List[MaterialLineResponse]): List[(String, String, String)] =
    lines.map(line => (line.materialId.toString, line.quantity, line.unitPrice))

  private def materialsChanged(row: MfgRow): Boolean = linesOf(row.materials) != linesOf(row.baseMaterials)

  /**
   * The draft moved onto a newer `order` snapshot. Rows that still exist keep the fields the user changed (their baseline moves to the server) and
   * take the server values for the others; new rows are added, removed rows are dropped, and "depends on" ids that no longer exist are dropped. Row
   * order follows the server. `newKey` mints the keys of the material rows taken from the server.
   */
  def rebase(draft: Draft, order: OrderResponse, newKey: () => Int): Draft =
    val serverHeader = headerOf(order)
    val header = HeaderValues(
      pick(draft.baseline.priority, draft.header.priority, serverHeader.priority),
      pick(draft.baseline.deadline, draft.header.deadline, serverHeader.deadline),
      pick(draft.baseline.description, draft.header.description, serverHeader.description),
    )
    val mfgIds = order.manufacturings.map(_.id.toString).toSet
    val previous = draft.mfgs.map(row => row.id -> row).toMap
    val mfgs = order.manufacturings.map { m =>
      previous.get(m.id) match
        case None => freshMfg(order, m, newKey)
        case Some(row) =>
          val server = mfgValuesOf(order, m)
          val (base, cur) = (row.baseline, row.current)
          val values = MfgValues(
            pick(base.description, cur.description, server.description),
            pick(base.deadline, cur.deadline, server.deadline),
            pick(base.status, cur.status, server.status),
            pick(base.employeeId, cur.employeeId, server.employeeId),
            pick(base.dependsOn, cur.dependsOn, server.dependsOn).filter(id => mfgIds.contains(id) && id != m.id.toString),
          )
          val serverRows = MaterialDrafts.rowsOf(m.materials, newKey)
          // The user's lines survive unless the server lines changed meanwhile (they were just saved).
          val materials = if materialsChanged(row) && savedLines(m.materials) == savedLines(row.server.materials) then row.materials else serverRows
          MfgRow(m, server, values, rebaseTasks(row.tasks, m), serverRows, materials)
    }
    Draft(order, serverHeader, header, mfgs)
  end rebase

  private def rebaseTasks(rows: List[TaskRow], m: ManufacturingResponse): List[TaskRow] =
    val templateIds = m.tasks.map(_.taskId.toString).toSet
    val previous = rows.map(row => row.id -> row).toMap
    m.tasks.map { t =>
      previous.get(t.id) match
        case None => freshTask(m, t)
        case Some(row) =>
          val server = taskValuesOf(m, t)
          val (base, cur) = (row.baseline, row.current)
          val values = TaskValues(
            pick(base.expected, cur.expected, server.expected),
            pick(base.completed, cur.completed, server.completed),
            pick(base.employeeId, cur.employeeId, server.employeeId),
            pick(base.dependsOn, cur.dependsOn, server.dependsOn).filter(id => templateIds.contains(id) && id != t.taskId.toString),
          )
          TaskRow(t, server, values)
    }

  // ---- Updates (used by the view) ----------------------------------------------------------------

  def updateHeader(draft: Draft)(change: HeaderValues => HeaderValues): Draft = draft.copy(header = change(draft.header))

  def updateMfg(draft: Draft, mfgId: UUID)(change: MfgValues => MfgValues): Draft =
    draft.copy(mfgs = draft.mfgs.map(row => if row.id == mfgId then row.copy(current = change(row.current)) else row))

  def updateTask(draft: Draft, mfgId: UUID, taskId: UUID)(change: TaskValues => TaskValues): Draft =
    draft.copy(mfgs = draft.mfgs.map { row =>
      if row.id == mfgId then row.copy(tasks = row.tasks.map(task => if task.id == taskId then task.copy(current = change(task.current)) else task))
      else row
    })

  def updateMaterials(draft: Draft, mfgId: UUID)(change: List[MaterialDrafts.Row] => List[MaterialDrafts.Row]): Draft =
    draft.copy(mfgs = draft.mfgs.map(row => if row.id == mfgId then row.copy(materials = change(row.materials)) else row))

  // ---- Fields, changes and issues ----------------------------------------------------------------

  enum HeaderPart derives CanEqual:
    case Priority, Deadline, Description

  enum MfgPart derives CanEqual:
    case Description, Deadline, Status, Employee, Dependencies, Materials

  enum TaskPart derives CanEqual:
    case Employee, Expected, Completed, Dependencies

  /** An editable field of the page: what a change or an issue points to. */
  enum EditField derives CanEqual:
    case Header(part: HeaderPart)
    case Mfg(mfgId: UUID, part: MfgPart)
    case Task(mfgId: UUID, taskId: UUID, part: TaskPart)

    /** The manufacturing the field belongs to (its card), if any. */
    def card: Option[UUID] = this match
      case Header(_) => None
      case Mfg(id, _) => Some(id)
      case Task(id, _, _) => Some(id)

  /** The stable DOM id of a field (prefix `oe-`, so it never collides with the create page's `oc-` ids). */
  def domId(field: EditField): String = field match
    case EditField.Header(HeaderPart.Priority) => "oe-priority"
    case EditField.Header(HeaderPart.Deadline) => "oe-deadline"
    case EditField.Header(HeaderPart.Description) => "oe-description"
    case EditField.Mfg(id, MfgPart.Description) => s"oe-mfg-$id-description"
    case EditField.Mfg(id, MfgPart.Deadline) => s"oe-mfg-$id-deadline"
    case EditField.Mfg(id, MfgPart.Status) => s"oe-mfg-$id-status"
    case EditField.Mfg(id, MfgPart.Employee) => s"oe-mfg-$id-employee"
    case EditField.Mfg(id, MfgPart.Dependencies) => s"oe-mfg-$id-dependencies"
    case EditField.Mfg(id, MfgPart.Materials) => s"oe-mfg-$id-materials"
    case EditField.Task(_, id, TaskPart.Employee) => s"oe-task-$id-employee"
    case EditField.Task(_, id, TaskPart.Expected) => s"oe-task-$id-expected"
    case EditField.Task(_, id, TaskPart.Completed) => s"oe-task-$id-completed"
    case EditField.Task(_, id, TaskPart.Dependencies) => s"oe-task-$id-dependencies"

  /** One unsaved change, e.g. «Testata: priorità», «MFG-01 · Cucitura: ore fatte», «MFG-02: materiali». */
  final case class Change(field: EditField, label: String) derives CanEqual

  /**
   * One value that cannot be sent: `message` is the inline text under the field (capitalized, e.g. «Usa il formato h:mm (es. 1:35)»), `context` names
   * the field in the checklist (e.g. «MFG-01 · Cucitura, ore fatte»).
   */
  final case class Issue(field: EditField, context: String, message: String) derives CanEqual:
    /** The checklist line: «context: message», the message decapitalized. */
    def text: String = s"$context: ${message.headOption.fold(message)(first => s"${first.toLower}${message.drop(1)}")}"

  private def durationChanged(baseline: String, current: String): Boolean = Formats.parseDuration(baseline) != Formats.parseDuration(current)

  private def headerChanges(draft: Draft): List[HeaderPart] =
    val (base, cur) = (draft.baseline, draft.header)
    List(
      Option.when(cur.priority != base.priority)(HeaderPart.Priority),
      Option.when(cur.deadline.trim.nn != base.deadline.trim)(HeaderPart.Deadline),
      Option.when(cur.description.trim.nn != base.description.trim)(HeaderPart.Description),
    ).flatten

  private def mfgChanges(row: MfgRow): List[MfgPart] =
    val (base, cur) = (row.baseline, row.current)
    List(
      Option.when(cur.description.trim.nn != base.description.trim)(MfgPart.Description),
      Option.when(cur.deadline.trim.nn != base.deadline.trim)(MfgPart.Deadline),
      Option.when(cur.status != base.status)(MfgPart.Status),
      Option.when(cur.employeeId != base.employeeId)(MfgPart.Employee),
      Option.when(cur.dependsOn != base.dependsOn)(MfgPart.Dependencies),
      Option.when(materialsChanged(row))(MfgPart.Materials),
    ).flatten

  private def taskChanges(row: TaskRow): List[TaskPart] =
    val (base, cur) = (row.baseline, row.current)
    List(
      Option.when(cur.employeeId != base.employeeId)(TaskPart.Employee),
      Option.when(durationChanged(base.expected, cur.expected))(TaskPart.Expected),
      Option.when(durationChanged(base.completed, cur.completed))(TaskPart.Completed),
      Option.when(cur.dependsOn != base.dependsOn)(TaskPart.Dependencies),
    ).flatten

  private def headerLabel(part: HeaderPart): String = part match
    case HeaderPart.Priority => "priorità"
    case HeaderPart.Deadline => "deadline fine lavorazione"
    case HeaderPart.Description => "descrizione"

  private def mfgLabel(part: MfgPart): String = part match
    case MfgPart.Description => "descrizione"
    case MfgPart.Deadline => "deadline"
    case MfgPart.Status => "stato"
    case MfgPart.Employee => "dipendente preferito"
    case MfgPart.Dependencies => "dipendenze"
    case MfgPart.Materials => "materiali"

  private def taskLabel(part: TaskPart): String = part match
    case TaskPart.Employee => "dipendente"
    case TaskPart.Expected => "ore previste"
    case TaskPart.Completed => "ore fatte"
    case TaskPart.Dependencies => "dipendenze"

  /** «code · task name», how a task is named in the change and issue lists. */
  private def taskSubject(row: MfgRow, task: TaskRow, taskName: UUID => String): String = s"${row.code} · ${taskName(task.templateTaskId)}"

  /** Every unsaved change, in page order: the header, then each manufacturing with its tasks. `taskName` names a template task. */
  def changes(draft: Draft, taskName: UUID => String): List[Change] =
    headerChanges(draft).map(part => Change(EditField.Header(part), s"Testata: ${headerLabel(part)}")) ++
      draft.mfgs.flatMap { row =>
        mfgChanges(row).map(part => Change(EditField.Mfg(row.id, part), s"${row.code}: ${mfgLabel(part)}")) ++
          row.tasks.flatMap(task =>
            taskChanges(task).map(part => Change(EditField.Task(row.id, task.id, part), s"${taskSubject(row, task, taskName)}: ${taskLabel(part)}")),
          )
      }

  /** True while some value differs from the server one. */
  def isDirty(draft: Draft): Boolean =
    headerChanges(draft).nonEmpty || draft.mfgs.exists(row => mfgChanges(row).nonEmpty || row.tasks.exists(task => taskChanges(task).nonEmpty))

  /** True while some value of manufacturing `row` (or of its tasks or materials) differs from the server one: its card shows "non salvata". */
  def isMfgDirty(row: MfgRow): Boolean = mfgChanges(row).nonEmpty || row.tasks.exists(task => taskChanges(task).nonEmpty)

  /** Inline message of a blank duration. */
  val durationRequired: String = "Indica le ore (h:mm, es. 1:35)"

  /** Inline message of an unreadable duration (e.g. "1.30"). */
  val durationFormat: String = "Usa il formato h:mm (es. 1:35)"

  /** Inline message of planned hours below one minute. */
  val expectedTooLow: String = "Le ore previste devono essere almeno 0:01"

  /** The issue of a duration text, if any: blank, unreadable, or (for planned hours, `atLeastOne`) zero. */
  def durationIssue(text: String, atLeastOne: Boolean): Option[String] =
    if text.trim.nn.isEmpty then Some(durationRequired)
    else
      Formats.parseDuration(text) match
        case None => Some(durationFormat)
        case Some(minutes) if atLeastOne && minutes < 1 => Some(expectedTooLow)
        case Some(_) => None

  /**
   * Every value that cannot be sent, in page order: a cleared work deadline (header or manufacturing), unreadable or blank durations, planned hours
   * below one minute, and the messages of changed material lines checked against `materials(mfgId)`.
   */
  def issues(draft: Draft, taskName: UUID => String, materials: UUID => MaterialDrafts.Context): List[Issue] =
    val header = Option
      .when(draft.header.deadline.trim.nn.isEmpty && draft.baseline.deadline.trim.nn.nonEmpty)(
        Issue(EditField.Header(HeaderPart.Deadline), "Testata, deadline fine lavorazione", "Deadline fine lavorazione obbligatoria"),
      )
      .toList
    header ++ draft.mfgs.flatMap { row =>
      val deadline = Option
        .when(row.current.deadline.trim.nn.isEmpty)(Issue(EditField.Mfg(row.id, MfgPart.Deadline), s"${row.code}, deadline", "Deadline obbligatoria"))
        .toList
      val tasks = row.tasks.flatMap { task =>
        val subject = taskSubject(row, task, taskName)
        durationIssue(task.current.expected, true).map(m =>
          Issue(EditField.Task(row.id, task.id, TaskPart.Expected), s"$subject, ore previste", m),
        ) ++
          durationIssue(task.current.completed, false).map(m => Issue(EditField.Task(row.id, task.id, TaskPart.Completed), s"$subject, ore fatte", m))
      }
      val lines =
        if materialsChanged(row) then
          MaterialDrafts
            .errors(row.materials, materials(row.id))
            .map(m => Issue(EditField.Mfg(row.id, MfgPart.Materials), s"${row.code}, materiali", m))
        else Nil
      deadline ++ tasks ++ lines
    }
  end issues

  // ---- Effects -----------------------------------------------------------------------------------

  /** One request that saves part of the draft; each returns the updated order. Executed in list order, stopping at the first failure. */
  enum EditOp derives CanEqual:
    case UpdateOrder(request: OrderUpdateRequest)
    case UpdateMfg(mfgId: UUID, request: ManufacturingUpdateRequest)
    case SetMfgEmployee(mfgId: UUID, employeeId: Option[UUID])
    case UpdateTask(mfgId: UUID, taskId: UUID, request: TaskProgressUpdateRequest)
    case SetTaskEmployee(mfgId: UUID, taskId: UUID, employeeId: Option[UUID])
    case OrderDependencies(request: OrderDependenciesUpdateRequest)
    case TaskDependencies(mfgId: UUID, request: TaskDependenciesUpdateRequest)
    case Materials(mfgId: UUID, lines: List[MaterialLineDto])

  /**
   * The requests that save the changes of a draft without [[issues]], only what changed: the order fields, the manufacturing fields, the preferred
   * employees, the task hours (only the changed one of the two, so hours recorded meanwhile by the workshop are not overwritten), the dependency
   * graphs (each replaced whole), then the material lines (replaced whole, checked against `materials(mfgId)`).
   */
  def effects(draft: Draft, materials: UUID => MaterialDrafts.Context): List[EditOp] =
    val header = headerChanges(draft)
    val orderUpdate = Option.when(header.nonEmpty)(
      EditOp.UpdateOrder(
        OrderUpdateRequest(
          Option.when(header.contains(HeaderPart.Priority))(draft.header.priority),
          Option.when(header.contains(HeaderPart.Deadline))(Formats.toIso(draft.header.deadline.trim.nn)),
          Option.when(header.contains(HeaderPart.Description))(draft.header.description.trim.nn),
        ),
      ),
    )
    val byMfg = draft.mfgs.map(row => row -> mfgChanges(row))
    val mfgUpdates = byMfg.flatMap { (row, parts) =>
      val description = Option.when(parts.contains(MfgPart.Description))(row.current.description.trim.nn)
      val deadline = Option.when(parts.contains(MfgPart.Deadline))(Formats.toIso(row.current.deadline.trim.nn))
      val status = Option.when(parts.contains(MfgPart.Status))(row.current.status)
      Option.when(description.isDefined || deadline.isDefined || status.isDefined)(
        EditOp.UpdateMfg(row.id, ManufacturingUpdateRequest(description, deadline, status, None)),
      )
    }
    val mfgEmployees = byMfg.collect {
      case (row, parts) if parts.contains(MfgPart.Employee) => EditOp.SetMfgEmployee(row.id, parseUuid(row.current.employeeId))
    }
    val byTask = draft.mfgs.flatMap(row => row.tasks.map(task => (row, task, taskChanges(task))))
    val taskUpdates = byTask.flatMap { (row, task, parts) =>
      val completed = Option.when(parts.contains(TaskPart.Completed))(Formats.parseDuration(task.current.completed)).flatten
      val expected = Option.when(parts.contains(TaskPart.Expected))(Formats.parseDuration(task.current.expected)).flatten
      Option.when(completed.isDefined || expected.isDefined)(EditOp.UpdateTask(row.id, task.id, TaskProgressUpdateRequest(completed, expected)))
    }
    val taskEmployees = byTask.collect {
      case (row, task, parts) if parts.contains(TaskPart.Employee) => EditOp.SetTaskEmployee(row.id, task.id, parseUuid(task.current.employeeId))
    }
    val orderDependencies = Option.when(byMfg.exists(_._2.contains(MfgPart.Dependencies)))(
      EditOp.OrderDependencies(
        OrderDependenciesUpdateRequest(
          draft.mfgs.flatMap { row =>
            val dependsOn = row.current.dependsOn.toList.sorted.flatMap(parseUuid).filter(_ != row.id)
            Option.when(dependsOn.nonEmpty)(ManufacturingDependencyDto(row.id, dependsOn))
          },
        ),
      ),
    )
    val taskDependencies = draft.mfgs.flatMap { row =>
      Option.when(row.tasks.exists(task => task.current.dependsOn != task.baseline.dependsOn))(
        EditOp.TaskDependencies(
          row.id,
          TaskDependenciesUpdateRequest(
            row.tasks.flatMap { task =>
              val dependsOn = task.current.dependsOn.toList.sorted.flatMap(parseUuid).filter(_ != task.templateTaskId)
              Option.when(dependsOn.nonEmpty)(TaskDependencyDto(task.templateTaskId, dependsOn))
            }
              .distinctBy(_.taskId),
          ),
        ),
      )
    }
    val materialUpdates = byMfg.flatMap { (row, parts) =>
      if parts.contains(MfgPart.Materials) then MaterialDrafts.request(row.materials, materials(row.id)).toOption.map(EditOp.Materials(row.id, _))
      else None
    }
    orderUpdate.toList ++ mfgUpdates ++ mfgEmployees ++ taskUpdates ++ taskEmployees ++ orderDependencies ++ taskDependencies ++ materialUpdates
  end effects

  // ---- Stock warnings ----------------------------------------------------------------------------

  /**
   * The material lines of the order that still need stock: the current (possibly edited) lines of every saved manufacturing that is not completed.
   * The server leaves the order out of the demand it reports (`excludeOrder`), so these lines are added back to the edited ones.
   */
  def demandRows(draft: Draft): List[MaterialDrafts.Row] = draft.mfgs.filter(_.needsStock).flatMap(_.materials)

  /**
   * The non-blocking stock warnings of the page (see [[OrderDrafts.stockWarnings]]): the lines of [[demandRows]] plus `newRows` (the manufacturings
   * being added) against the `requirements` read without this order.
   */
  def stockWarnings(
      draft: Draft,
      newRows: List[MaterialDrafts.Row],
      requirements: List[MaterialRequirementResponse],
  ): Map[UUID, OrderDrafts.StockWarning] =
    OrderDrafts.stockWarnings(demandRows(draft) ++ newRows, requirements)

  // ---- Structural operations ---------------------------------------------------------------------

  /**
   * The issues of the inline "+ Task" row of a saved manufacturing whose template tasks are `present`: a selected task not already in it, and a
   * readable duration of at least one minute. Messages are inline (capitalized).
   */
  def newTaskIssues(state: TaskState, present: Set[UUID]): List[(TaskField, String)] =
    val task = parseUuid(state.taskId) match
      case None => Some(TaskField.Task -> "Seleziona un task")
      case Some(id) if present.contains(id) => Some(TaskField.Task -> "Task già presente in questa lavorazione")
      case Some(_) => None
    task.toList ++ durationIssue(state.hours, true).map(TaskField.Duration -> _)

  /** The request of a valid inline "+ Task" row (see [[newTaskIssues]]). */
  def addTaskRequest(state: TaskState): Option[AddTaskRequest] =
    for
      taskId <- parseUuid(state.taskId)
      minutes <- Formats.parseDuration(state.hours)
    yield AddTaskRequest(taskId, minutes, Nil, parseUuid(state.employeeId))

  /** The manufacturings of `after` that were not in `before` (the one just added). */
  def addedManufacturings(before: OrderResponse, after: OrderResponse): List[ManufacturingResponse] =
    val known = before.manufacturings.map(_.id).toSet
    after.manufacturings.filterNot(m => known.contains(m.id))
end OrderEditDraft
