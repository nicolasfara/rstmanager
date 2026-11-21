package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messages, FieldIssue }

/**
 * Pure (DOM-free) state of the manufacturing-catalog create/edit form: loading a stored template, request building, the live total duration and
 * field-addressed validation. Task rows are identified by a stable `key` (injected by the caller), so their issues and DOM ids survive reordering.
 */
object ManufacturingCatalogForm:

  /** One task of the template: the catalog task, its dependencies (task ids), the default employee and the duration (`h:mm`). */
  final case class TaskRow(key: Int, taskId: String, dependsOn: Set[String], employeeId: String, hours: String) derives CanEqual

  /** The fields that can carry a validation issue. */
  enum Field derives CanEqual:
    case Code
    case Name
    case Tasks
    case RowTask(key: Int)
    case RowDuration(key: Int)

  /** DOM id of the task select of row `key`. */
  def rowTaskId(key: Int): String = s"mfg-row-$key-task"

  /** DOM id of the duration input of row `key`. */
  def rowDurationId(key: Int): String = s"mfg-row-$key-duration"

  /** DOM id of the task block (its inline error and touch tracking). */
  val tasksId = "mfg-tasks"

  /** DOM id of the control of `field` (label `for`, inline error anchor). */
  def fieldId(field: Field): String = field match
    case Field.Code => "mfg-code"
    case Field.Name => "mfg-name"
    case Field.Tasks => tasksId
    case Field.RowTask(key) => rowTaskId(key)
    case Field.RowDuration(key) => rowDurationId(key)

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value).nn).toOption

  final case class State(
      editingId: Option[UUID],
      code: String,
      codeManuallyEdited: Boolean,
      name: String,
      description: String,
      taskRows: List[TaskRow],
  ):
    /**
     * The issues in form order: code, name, then the task block. With no task picked at all there is a single "pick a task" issue; otherwise each row
     * reports a missing task (only when there are several rows), a repeated task (the server rejects duplicates) and a missing or malformed duration
     * (`h:mm`, or whole hours; 0 is allowed).
     */
    def issues: List[FieldIssue[Field]] =
      val header = List(
        Option.when(code.trim.nn.isEmpty)(FieldIssue(Field.Code, "Codice obbligatorio")),
        Option.when(name.trim.nn.isEmpty)(FieldIssue(Field.Name, "Nome obbligatorio")),
      ).flatten
      val rows =
        if !taskRows.exists(_.taskId.nonEmpty) then List(FieldIssue(Field.Tasks, "Seleziona almeno un task"))
        else
          taskRows.zipWithIndex.flatMap { (row, index) =>
            val prefix = s"Task ${index + 1}"
            if row.taskId.isEmpty then List(FieldIssue(Field.RowTask(row.key), s"$prefix: seleziona un task o rimuovi la riga"))
            else
              val repeated = taskRows.take(index).exists(_.taskId == row.taskId)
              List(
                Option.when(repeated)(FieldIssue(Field.RowTask(row.key), s"$prefix: task già presente in questa lavorazione")),
                if row.hours.trim.nn.isEmpty then Some(FieldIssue(Field.RowDuration(row.key), s"$prefix: durata obbligatoria"))
                else
                  Option.when(Formats.parseDuration(row.hours).isEmpty)(
                    FieldIssue(Field.RowDuration(row.key), s"$prefix: durata non valida (usa h:mm, es. 1:30, o ore intere)"),
                  ),
              ).flatten
          }
      header ++ rows
    end issues

    def errors: List[String] = issues.messages

    /** Sum of the valid durations of the rows with a task (a malformed duration counts as 0 until fixed). */
    def totalMinutes: Int = taskRows.filter(_.taskId.nonEmpty).flatMap(row => Formats.parseDuration(row.hours)).sum

    /**
     * The request for the current state. `catalogMinutes` (task id -> catalog duration) decides the overrides: a duration is sent only when it
     * differs from the catalog task's, so untouched tasks keep following the catalog.
     */
    def request(catalogMinutes: Map[String, Int]): ManufacturingCatalogRequest =
      val selectedIds = taskRows.flatMap(row => parseUuid(row.taskId))
      val selectedStrings = selectedIds.map(_.toString).toSet
      val dependencies = taskRows.flatMap { row =>
        parseUuid(row.taskId).map { taskId =>
          val dependsOn = row.dependsOn.toList.filter(id => selectedStrings.contains(id) && id != taskId.toString).flatMap(parseUuid)
          ManufacturingCatalogDependencyDto(taskId, dependsOn)
        }
      }
      val defaultEmployees = taskRows.flatMap { row =>
        for
          taskId <- parseUuid(row.taskId)
          employeeId <- parseUuid(row.employeeId)
        yield TaskDefaultEmployeeDto(taskId, employeeId)
      }
      val taskDurations = taskRows.flatMap { row =>
        for
          taskId <- parseUuid(row.taskId)
          minutes <- Formats.parseDuration(row.hours)
          if catalogMinutes.get(row.taskId).forall(_ != minutes)
        yield TaskDurationOverrideDto(taskId, minutes)
      }
      ManufacturingCatalogRequest(
        code.trim.nn,
        name.trim.nn,
        Some(description.trim.nn).filter(_.nonEmpty),
        selectedIds,
        dependencies,
        Some(defaultEmployees),
        Some(taskDurations),
      )
    end request
  end State

  /** A blank form proposing `code`, with one empty task row keyed `rowKey`. */
  def fresh(code: String, rowKey: Int): State = State(None, code, false, "", "", List(emptyRow(rowKey)))

  def emptyRow(key: Int): TaskRow = TaskRow(key, "", Set.empty, "", "")

  /** Loads a stored template: one row per task (keys from `newKey`), its duration being the override or else the catalog task's. */
  def fromResponse(manufacturing: ManufacturingCatalogResponse, newKey: () => Int): State =
    val dependencyMap = manufacturing.dependencies.map(d => d.taskId.toString -> d.dependsOn.map(_.toString).toSet).toMap
    val defaultEmployeeMap = manufacturing.defaultEmployees.map(d => d.taskId.toString -> d.employeeId.toString).toMap
    val overrideMinutesMap = manufacturing.taskDurations.map(h => h.taskId.toString -> h.minutes).toMap
    val catalogMinutesMap = manufacturing.tasks.map(task => task.id.toString -> task.requiredMinutes).toMap
    State(
      Some(manufacturing.id),
      manufacturing.code,
      false,
      manufacturing.name,
      manufacturing.description.getOrElse(""),
      manufacturing.taskIds.map { id =>
        val key = id.toString
        val minutes = overrideMinutesMap.getOrElse(key, catalogMinutesMap.getOrElse(key, 0))
        TaskRow(newKey(), key, dependencyMap.getOrElse(key, Set.empty), defaultEmployeeMap.getOrElse(key, ""), Formats.duration(minutes))
      },
    )
  end fromResponse
end ManufacturingCatalogForm
