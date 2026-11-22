package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*

/**
 * Pure transformations from the create-order form drafts to the order-creation DTOs.
 *
 * Deliberately free of Laminar `Var`s and the DOM: the caller resolves the mutable draft state into the immutable snapshots below, and UUID minting
 * is injected as `newId`, so the mapping and validation rules can be unit-tested deterministically.
 */
object OrderDrafts:

  /** Immutable state of one task draft in the create form. `dependsOn` holds template-task id strings. */
  final case class TaskState(taskId: String, hours: String, dependsOn: Set[String], employeeId: String) derives CanEqual

  /**
   * Immutable state of the non-list fields of a manufacturing draft. `taskEmployees` holds, for catalog mode, the per-task preferred employee
   * (template-task id -> employee id). `dependsOn` holds sibling draft key strings. `quantity` is the typed multiplier ×N (see [[quantityOf]]).
   */
  final case class MfgCoreState(
      mode: String,
      catalogId: String,
      code: String,
      completionDate: String,
      description: String,
      employeeId: String,
      dependsOn: Set[String],
      taskEmployees: Map[String, String],
      quantity: String = "1",
  )

  /**
   * A manufacturing draft resolved to plain data: its stable key, core fields, the state of its task drafts, and its material lines (see
   * [[MaterialDrafts]]; blank lines are ignored, none means no materials).
   */
  final case class MfgSnapshot(key: Int, core: MfgCoreState, tasks: List[TaskState], materials: List[MaterialDrafts.Row] = Nil)

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  /** Trims free text and collapses blank strings to `None` (an empty value clears the field). */
  private def normalizeStr(value: String): Option[String] = Some(value.trim.nn).filter(_.nonEmpty)

  // ---- Multiplier ×N -----------------------------------------------------------------------------

  /** Largest multiplier ×N the server accepts. */
  val maxQuantity: Int = 999

  /** The multiplier typed in `text`: a whole number from 1 to [[maxQuantity]]; `None` otherwise. */
  def parseQuantity(text: String): Option[Int] = text.trim.nn.toIntOption.filter(n => n >= 1 && n <= maxQuantity)

  /** The multiplier of a draft, 1 while the typed one is not valid (the draft is then blocked by [[manufacturingIssues]]). */
  def quantityOf(core: MfgCoreState): Int = parseQuantity(core.quantity).getOrElse(1)

  /**
   * The material rows of a draft once the catalog template `template` is picked with multiplier `times`: the lines the user added or edited stay, the
   * ones prefilled from a previous template go, and each per-unit material of the template not already in a kept row is added as `quantity × times`
   * (without price: the current customer price is frozen on save). Archived or unknown materials of the template are not prefilled (see
   * [[unavailableCatalogMaterials]]).
   */
  def withCatalogMaterials(
      rows: List[MaterialDrafts.Row],
      template: Option[ManufacturingCatalogResponse],
      times: Int,
      newKey: () => Int,
  ): List[MaterialDrafts.Row] =
    val kept = rows.filter(row => row.perUnit.isEmpty && !MaterialDrafts.isBlank(row))
    val keptIds = kept.map(_.materialId.trim.nn).toSet
    val prefilled = template.toList.flatMap(_.materials).filter(line => isPrefillable(line) && !keptIds.contains(line.materialId.toString)).map {
      line => MaterialDrafts.scaledRow(newKey(), line.materialId.toString, line.quantity, times)
    }
    kept ++ prefilled

  private def isPrefillable(line: CatalogMaterialResponse): Boolean = !line.archived && line.internalCode.isDefined

  /** The per-unit materials of a template that are not prefilled into an order because they are archived or no longer in the warehouse. */
  def unavailableCatalogMaterials(template: ManufacturingCatalogResponse): List[CatalogMaterialResponse] =
    template.materials.filterNot(isPrefillable)

  /**
   * A custom task row once the multiplier is `times`: a duration that still follows the picked task (not `edited`) becomes the task-catalog
   * `catalogMinutes × times`; an edited one is kept as typed.
   */
  def followTask(state: TaskState, edited: Boolean, catalogMinutes: Option[Int], times: Int): TaskState =
    if edited then state else catalogMinutes.fold(state)(minutes => state.copy(hours = Formats.duration(minutes * times)))

  /**
   * The template tasks with their effective duration (minutes): the per-task override configured on the catalog template, else the task-catalog
   * duration. Shared by [[fromCatalog]] and the totals, so the preview always matches what is sent.
   */
  private def catalogMinutes(template: ManufacturingCatalogResponse): List[(TaskResponse, Int)] =
    val durationOverride = template.taskDurations.map(entry => entry.taskId -> entry.minutes).toMap
    template.tasks.map(task => task -> durationOverride.getOrElse(task.id, task.requiredMinutes))

  /** Builds a manufacturing done once from a catalog template (see the overload with the multiplier). */
  def fromCatalog(
      template: ManufacturingCatalogResponse,
      completionDate: String,
      employeeId: String,
      taskEmployees: Map[String, String],
      newId: () => UUID,
  ): ManufacturingDto =
    fromCatalog(template, completionDate, employeeId, taskEmployees, 1, newId)

  /**
   * Builds a manufacturing from a catalog template, prefilling tasks/dependencies and the per-task employees. The task minutes are the effective
   * durations × `times` (the server never rescales them) and `quantity` carries the multiplier.
   */
  def fromCatalog(
      template: ManufacturingCatalogResponse,
      completionDate: String,
      employeeId: String,
      taskEmployees: Map[String, String],
      times: Int,
      newId: () => UUID,
  ): ManufacturingDto =
    ManufacturingDto(
      template.code,
      Formats.toIso(completionDate),
      "not_started",
      catalogMinutes(template).map { (task, minutes) =>
        ScheduledTaskDto(
          newId(),
          task.id,
          "pending",
          minutes * times,
          Some(0),
          None,
          taskEmployees.get(task.id.toString).flatMap(parseUuid),
        )
      },
      template.dependencies.map(dependency => TaskDependencyDto(dependency.taskId, dependency.dependsOn)),
      None,
      None,
      None,
      None,
      template.description,
      parseUuid(employeeId),
      None,
      Some(times),
    )
  end fromCatalog

  /**
   * Builds a manufacturing from a custom draft: only tasks with a valid id survive, and dependencies are kept only between tasks that made it into
   * the manufacturing (self-edges dropped). The durations are sent as shown (already the totals for the multiplier, see [[followTask]]).
   */
  def fromCustom(core: MfgCoreState, tasks: List[TaskState], completionDate: String, newId: () => UUID): ManufacturingDto =
    val scheduled = tasks.flatMap { ts =>
      parseUuid(ts.taskId).map { taskId =>
        ScheduledTaskDto(newId(), taskId, "pending", Formats.parseDuration(ts.hours).getOrElse(0), Some(0), None, parseUuid(ts.employeeId))
      }
    }
    val selectedIds = scheduled.map(_.taskId.toString).toSet
    val dependencies = tasks.flatMap { ts =>
      parseUuid(ts.taskId).flatMap { taskId =>
        val dependsOn = ts.dependsOn.toList.filter(dep => selectedIds.contains(dep) && dep != taskId.toString).flatMap(parseUuid)
        if dependsOn.isEmpty then None else Some(TaskDependencyDto(taskId, dependsOn))
      }
    }
    ManufacturingDto(
      core.code.trim.nn,
      Formats.toIso(completionDate),
      "not_started",
      scheduled,
      dependencies,
      None,
      None,
      None,
      None,
      normalizeStr(core.description),
      parseUuid(core.employeeId),
      None,
      Some(quantityOf(core)),
    )
  end fromCustom

  /** Resolves one manufacturing snapshot to a DTO, without materials context (any material line is then reported as unavailable). */
  def manufacturingFromDraft(
      snapshot: MfgSnapshot,
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      newId: () => UUID,
  ): Either[ApiError, ManufacturingDto] =
    manufacturingFromDraft(snapshot, fallbackDeadline, catalog, MaterialDrafts.Context.empty, newId)

  /**
   * Resolves one manufacturing snapshot to a DTO. Empty completion date falls back to the order-level work deadline; the material lines are checked
   * against `materials` (a manufacturing without lines omits `materials`).
   */
  def manufacturingFromDraft(
      snapshot: MfgSnapshot,
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      materials: MaterialDrafts.Context,
      newId: () => UUID,
  ): Either[ApiError, ManufacturingDto] =
    val core = snapshot.core
    val completionDate = Some(core.completionDate.trim.nn).filter(_.nonEmpty).getOrElse(fallbackDeadline)
    val manufacturing =
      if core.mode == "catalog" then
        catalog(core.catalogId)
          .toRight(ApiError("invalid-form", "Seleziona una lavorazione a catalogo valida.", Nil))
          .map(template => fromCatalog(template, completionDate, core.employeeId, core.taskEmployees, quantityOf(core), newId))
      else Right(fromCustom(core, snapshot.tasks, completionDate, newId))
    manufacturing.flatMap(withMaterials(_, snapshot.materials, materials))

  /** Attaches the non-blank material lines of a draft to `manufacturing`, or reports their validation messages. */
  private def withMaterials(
      manufacturing: ManufacturingDto,
      rows: List[MaterialDrafts.Row],
      context: MaterialDrafts.Context,
  ): Either[ApiError, ManufacturingDto] =
    if rows.forall(MaterialDrafts.isBlank) then Right(manufacturing)
    else
      MaterialDrafts
        .request(rows, context)
        .left
        .map { errors =>
          val subject = Some(manufacturing.code).filter(_.nonEmpty).fold("di una lavorazione")(code => s"della lavorazione $code")
          ApiError("invalid-form", s"Materiali $subject non validi.", errors)
        }
        .map(lines => manufacturing.copy(materials = Some(lines)))

  /** Resolves every manufacturing snapshot without materials context (see the overload with one). */
  def collectManufacturings(
      snapshots: List[MfgSnapshot],
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      newId: () => UUID,
  ): Either[ApiError, List[ManufacturingDto]] =
    collectManufacturings(snapshots, fallbackDeadline, catalog, MaterialDrafts.Context.empty, newId)

  /** Resolves every manufacturing snapshot, short-circuiting on the first invalid one. Order is preserved. */
  def collectManufacturings(
      snapshots: List[MfgSnapshot],
      fallbackDeadline: String,
      catalog: String => Option[ManufacturingCatalogResponse],
      materials: MaterialDrafts.Context,
      newId: () => UUID,
  ): Either[ApiError, List[ManufacturingDto]] =
    snapshots.foldLeft(Right(List.empty): Either[ApiError, List[ManufacturingDto]]) { (acc, snapshot) =>
      acc.flatMap(list => manufacturingFromDraft(snapshot, fallbackDeadline, catalog, materials, newId).map(list :+ _))
    }

  /** Maps the draft-key based "depends on" selections to positional indexes for the creation request. */
  def collectDependencies(snapshots: List[MfgSnapshot]): Option[List[ManufacturingDependencyByIndexDto]] =
    val indexByKey = snapshots.zipWithIndex.map((snapshot, index) => snapshot.key.toString -> index).toMap
    val entries = snapshots.zipWithIndex.flatMap { (snapshot, index) =>
      val dependsOnIndexes = snapshot.core.dependsOn.toList.flatMap(indexByKey.get).filter(_ != index).sorted
      if dependsOnIndexes.isEmpty then None else Some(ManufacturingDependencyByIndexDto(index, dependsOnIndexes))
    }
    if entries.isEmpty then None else Some(entries)

  // ---- Header ------------------------------------------------------------------------------------

  /** Days between the proposed work deadline and the customer delivery date. */
  val deadlineLeadDays: Int = 5

  /**
   * Immutable state of the create-order header. `numberManuallyEdited` stops the generated number from being refreshed; `workDeadlineManuallyEdited`
   * stops the work deadline from following the delivery date (see [[withDelivery]]). Dates are `yyyy-MM-dd` date-input text.
   */
  final case class HeaderState(
      number: String,
      numberManuallyEdited: Boolean,
      customerId: String,
      creationDate: String,
      deliveryDate: String,
      workDeadline: String,
      workDeadlineManuallyEdited: Boolean,
      priority: String,
      description: String,
  ) derives CanEqual:

    /** The number typed by the user: from now on it is no longer regenerated. */
    def withNumber(text: String): HeaderState = copy(number = text, numberManuallyEdited = true)

    /** A freshly generated number, applied only while the user has not typed one. */
    def withGeneratedNumber(text: String): HeaderState = if numberManuallyEdited then this else copy(number = text)

    /** A new delivery date: the work deadline follows it ([[proposedDeadline]]) until the user edits the deadline by hand. */
    def withDelivery(day: String): HeaderState =
      if workDeadlineManuallyEdited then copy(deliveryDate = day) else copy(deliveryDate = day, workDeadline = proposedDeadline(day))

    /** A work deadline typed by the user: it stops following the delivery date. */
    def withDeadline(day: String): HeaderState = copy(workDeadline = day, workDeadlineManuallyEdited = true)

    /** Back to the proposed work deadline ("↺ Usa proposta"), which follows the delivery date again. */
    def withProposedDeadline: HeaderState = copy(workDeadline = proposedDeadline(deliveryDate), workDeadlineManuallyEdited = false)
  end HeaderState

  object HeaderState:
    /** A blank header with the generated `number`, created `today` (`yyyy-MM-dd`), normal priority. */
    def initial(number: String, today: String): HeaderState = HeaderState(number, false, "", today, "", "", false, "normal", "")

  private def pad2(value: Int): String = if value < 10 then s"0$value" else value.toString

  // Proleptic Gregorian day number <-> civil date (H. Hinnant's algorithms): pure, time-zone free day arithmetic.
  private def daysFromCivil(year: Int, month: Int, day: Int): Int =
    val y = if month <= 2 then year - 1 else year
    val era = (if y >= 0 then y else y - 399) / 400
    val yoe = y - era * 400
    val doy = (153 * ((month + 9) % 12) + 2) / 5 + day - 1
    val doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
    era * 146097 + doe - 719468

  private def civilFromDays(days: Int): String =
    val z = days + 719468
    val era = (if z >= 0 then z else z - 146096) / 146097
    val doe = z - era * 146097
    val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp = (5 * doy + 2) / 153
    val day = doy - (153 * mp + 2) / 5 + 1
    val month = if mp < 10 then mp + 3 else mp - 9
    val year = yoe + era * 400 + (if month <= 2 then 1 else 0)
    s"$year-${pad2(month)}-${pad2(day)}"

  /** Shifts a `yyyy-MM-dd` day by `delta` days; `None` when the text is not such a day. */
  private def shiftDay(day: String, delta: Int): Option[String] =
    day.trim.nn.split("-").nn.toList.map(_.nn.toIntOption) match
      case Some(year) :: Some(month) :: Some(dayOfMonth) :: Nil if month >= 1 && month <= 12 && dayOfMonth >= 1 && dayOfMonth <= 31 =>
        Some(civilFromDays(daysFromCivil(year, month, dayOfMonth) + delta))
      case _ => None

  /** The work deadline proposed for a delivery date: [[deadlineLeadDays]] days earlier; empty when the delivery date is empty or unreadable. */
  def proposedDeadline(deliveryDate: String): String = shiftDay(deliveryDate, -deadlineLeadDays).getOrElse("")

  /** Soft warning only: the work deadline falls after the customer delivery date. */
  def deadlineAfterDelivery(header: HeaderState): Boolean =
    val deadline = header.workDeadline.trim.nn
    val delivery = header.deliveryDate.trim.nn
    deadline.nonEmpty && delivery.nonEmpty && deadline > delivery

  /** True while nothing was entered: the header is still the initial one (number not typed, created `today`) and no manufacturing was added. */
  def isPristine(header: HeaderState, today: String, snapshots: List[MfgSnapshot]): Boolean =
    snapshots.isEmpty && header == HeaderState.initial(header.number, today)

  // ---- Validation --------------------------------------------------------------------------------

  /** Header fields that can carry an issue. */
  enum HeaderField derives CanEqual:
    case Number, Customer, CreationDate, DeliveryDate, WorkDeadline

  /** Manufacturing-level fields that can carry an issue. */
  enum MfgField derives CanEqual:
    case Catalog, Code, Quantity, Tasks, Materials

  /** Fields of a custom manufacturing's task row that can carry an issue. */
  enum TaskField derives CanEqual:
    case Task, Duration

  /** Where an issue points to: `key` is the [[MfgSnapshot.key]], `index` the 0-based position of the task row in the snapshot. */
  enum IssueTarget derives CanEqual:
    case Header(field: HeaderField)
    case NoManufacturings
    case Manufacturing(key: Int, field: MfgField)
    case Task(key: Int, index: Int, field: TaskField)

  /** One blocking problem of the create-order draft, with its Italian message. */
  final case class Issue(target: IssueTarget, message: String) derives CanEqual

  private def isBlankText(value: String): Boolean = value.trim.nn.isEmpty

  /** The header issues, in form order: number, customer, creation, delivery, work deadline. */
  def headerIssues(header: HeaderState): List[Issue] =
    def at(field: HeaderField, message: String): Issue = Issue(IssueTarget.Header(field), message)
    List(
      Option.when(isBlankText(header.number))(at(HeaderField.Number, "Numero ordine obbligatorio")),
      if isBlankText(header.customerId) then Some(at(HeaderField.Customer, "Cliente obbligatorio"))
      else Option.when(parseUuid(header.customerId.trim.nn).isEmpty)(at(HeaderField.Customer, "Seleziona un cliente valido dall'elenco")),
      Option.when(isBlankText(header.creationDate))(at(HeaderField.CreationDate, "Data di creazione obbligatoria")),
      Option.when(isBlankText(header.deliveryDate))(at(HeaderField.DeliveryDate, "Data di consegna cliente obbligatoria")),
      Option.when(isBlankText(header.workDeadline))(at(HeaderField.WorkDeadline, "Deadline fine lavorazione obbligatoria")),
    ).flatten

  /**
   * The issues of the manufacturing at 0-based `index`: a catalog one needs a resolvable template; a custom one a code, at least one task, and per
   * task row a selected task (not repeated in the same manufacturing), a readable duration (0 allowed). Then the material line messages, suppressed
   * while `materials` is still empty (not loaded yet) although lines were entered. Both modes need a multiplier ×N from 1 to [[maxQuantity]].
   */
  def manufacturingIssues(
      snapshot: MfgSnapshot,
      index: Int,
      catalog: String => Option[ManufacturingCatalogResponse],
      materials: MaterialDrafts.Context,
  ): List[Issue] =
    val key = snapshot.key
    val prefix = s"Lavorazione ${index + 1}"
    def at(field: MfgField, message: String): Issue = Issue(IssueTarget.Manufacturing(key, field), s"$prefix: $message")
    val core = snapshot.core
    val structural =
      if core.mode == "catalog" then
        if isBlankText(core.catalogId) then List(at(MfgField.Catalog, "seleziona la lavorazione a catalogo"))
        else if catalog(core.catalogId).isEmpty then List(at(MfgField.Catalog, "la lavorazione a catalogo selezionata non è disponibile"))
        else Nil
      else
        val code = Option.when(isBlankText(core.code))(at(MfgField.Code, "codice obbligatorio")).toList
        val noTasks = Option.when(snapshot.tasks.isEmpty)(at(MfgField.Tasks, "aggiungi almeno un task")).toList
        val rows = snapshot.tasks.zipWithIndex.flatMap { (task, row) =>
          def atRow(field: TaskField, message: String): Issue = Issue(IssueTarget.Task(key, row, field), s"$prefix, task ${row + 1}: $message")
          val taskId = parseUuid(task.taskId.trim.nn)
          val taskIssue =
            if taskId.isEmpty then Some(atRow(TaskField.Task, "seleziona un task"))
            else
              val repeated = snapshot.tasks.take(row).exists(earlier => parseUuid(earlier.taskId.trim.nn).map(_.toString) == taskId.map(_.toString))
              Option.when(repeated)(atRow(TaskField.Task, "task già presente in questa lavorazione"))
          val durationIssue =
            if isBlankText(task.hours) then Some(atRow(TaskField.Duration, "durata obbligatoria"))
            else
              Option.when(Formats.parseDuration(task.hours).isEmpty)(
                atRow(TaskField.Duration, "durata non valida (usa h:mm, es. 1:30, o ore intere)"),
              )
          taskIssue.toList ++ durationIssue
        }
        code ++ noTasks ++ rows
    val quantity = Option
      .when(parseQuantity(core.quantity).isEmpty)(at(MfgField.Quantity, s"quantità non valida: indica un numero intero da 1 a $maxQuantity"))
      .toList
    val loading = materials.materials.isEmpty && snapshot.materials.exists(row => !MaterialDrafts.isBlank(row))
    val materialIssues =
      if loading then Nil
      else
        MaterialDrafts
          .errors(snapshot.materials, materials)
          .map(message => Issue(IssueTarget.Manufacturing(key, MfgField.Materials), s"$prefix, materiali — $message"))
    structural ++ quantity ++ materialIssues
  end manufacturingIssues

  /** Every issue of the draft, in display order: header, then the missing-manufacturings one, then each manufacturing in sequence. */
  def validate(
      header: HeaderState,
      snapshots: List[MfgSnapshot],
      catalog: String => Option[ManufacturingCatalogResponse],
      materials: MaterialDrafts.Context,
  ): List[Issue] =
    val noManufacturings = Option.when(snapshots.isEmpty)(Issue(IssueTarget.NoManufacturings, "Aggiungi almeno una lavorazione")).toList
    headerIssues(header) ++ noManufacturings ++ snapshots.zipWithIndex.flatMap((snapshot, index) =>
      manufacturingIssues(snapshot, index, catalog, materials),
    )

  // ---- Totals ------------------------------------------------------------------------------------

  /** Live totals: task count, work minutes and the materials cost (2 decimals, as the server computes it). */
  final case class MfgTotals(tasks: Int, minutes: Int, materialsCost: String) derives CanEqual

  /**
   * Totals of one manufacturing draft: a catalog one counts the template tasks with their effective (overridden) durations × N, nothing while the
   * template is unresolved; a custom one counts the rows with a selected task and their readable durations (already the totals for N).
   */
  def mfgTotals(snapshot: MfgSnapshot, catalog: String => Option[ManufacturingCatalogResponse], materials: MaterialDrafts.Context): MfgTotals =
    val (tasks, minutes) =
      if snapshot.core.mode == "catalog" then
        catalog(snapshot.core.catalogId).fold((0, 0)) { template =>
          val durations = catalogMinutes(template).map(_._2)
          (durations.size, durations.sum * quantityOf(snapshot.core))
        }
      else
        val selected = snapshot.tasks.filter(task => parseUuid(task.taskId.trim.nn).isDefined)
        (selected.size, selected.flatMap(task => Formats.parseDuration(task.hours)).sum)
    MfgTotals(tasks, minutes, MaterialDrafts.totalCost(snapshot.materials, materials))

  /** Totals of the whole order: the sums of [[mfgTotals]], the cost summed over every rounded line like the server. */
  def orderTotals(
      snapshots: List[MfgSnapshot],
      catalog: String => Option[ManufacturingCatalogResponse],
      materials: MaterialDrafts.Context,
  ): MfgTotals =
    val each = snapshots.map(mfgTotals(_, catalog, materials))
    MfgTotals(each.map(_.tasks).sum, each.map(_.minutes).sum, MaterialDrafts.totalCost(snapshots.flatMap(_.materials), materials))

  // ---- Stock warnings ----------------------------------------------------------------------------

  /**
   * A material the draft lines need more of than is available (stock minus what the other open orders commit): `required` is the total of the lines,
   * `missing` what has to be ordered (all of `required` when `available` is not positive). Quantities are canonical (3 decimals).
   */
  final case class StockWarning(
      materialId: UUID,
      internalCode: String,
      unit: String,
      available: String,
      required: String,
      missing: String,
  ) derives CanEqual

  private def canonical(value: BigDecimal): String = value.setScale(MaterialDrafts.quantityScale, RoundingMode.HALF_UP).bigDecimal.toPlainString.nn

  /**
   * The non-blocking stock warnings of `rows` (every line of the draft, across all its manufacturings, since the same material may appear in
   * several): the readable quantities are summed per material and compared with its `available` figure in `requirements`. A material missing from
   * `requirements` (not loaded yet, unknown) gets no warning.
   */
  def stockWarnings(rows: List[MaterialDrafts.Row], requirements: List[MaterialRequirementResponse]): Map[UUID, StockWarning] =
    val demand = rows
      .filterNot(MaterialDrafts.isBlank)
      .flatMap(row => parseUuid(row.materialId.trim.nn).zip(WarehouseFormats.parseDecimal(row.quantity, MaterialDrafts.quantityScale)))
      .groupMapReduce(_._1)((_, quantity) => BigDecimal(quantity))(_ + _)
    requirements.flatMap { requirement =>
      for
        required <- demand.get(requirement.materialId)
        available <- Try(BigDecimal(requirement.available)).toOption
        if required.signum > 0 && required > available
      yield
        val missing = required.min(required - available)
        requirement.materialId -> StockWarning(
          requirement.materialId,
          requirement.internalCode,
          requirement.unit,
          canonical(available),
          canonical(required),
          canonical(missing),
        )
    }.toMap
  end stockWarnings

  /** Italian text of a warning: "Disponibili X {unità}, richiesti Y: mancano Z — da ordinare". */
  def stockWarningText(warning: StockWarning): String =
    def qty(value: String): String = WarehouseFormats.formatQty(value, warning.unit)
    s"Disponibili ${qty(warning.available)}, richiesti ${qty(warning.required)}: mancano ${qty(warning.missing)} — da ordinare"
end OrderDrafts
