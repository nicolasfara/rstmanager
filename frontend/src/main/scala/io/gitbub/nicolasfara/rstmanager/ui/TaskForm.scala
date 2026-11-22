package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ TaskRequest, TaskResponse }
import io.gitbub.nicolasfara.rstmanager.ui.FormValidation.{ messages, FieldIssue }

/**
 * Pure (DOM-free) state of the catalog-task create/edit form: loading a stored task, request building and field-addressed validation. As on the
 * server, the name is required and the duration is a non-negative `h:mm` (or whole hours).
 */
object TaskForm:

  /** The fields that can carry a validation issue. */
  enum Field derives CanEqual:
    case Name, Duration

  /** DOM id of the control of `field` (label `for`, inline error anchor, focus target). */
  def fieldId(field: Field): String = field match
    case Field.Name => "task-name"
    case Field.Duration => "task-duration"

  /** The duration proposed for a new task. */
  val defaultHours = "8:00"

  final case class State(editingId: Option[UUID], name: String, description: String, hours: String, defaultEmployee: String) derives CanEqual:

    /** How the task is named in the UI (trimmed). */
    def displayName: String = name.trim.nn

    /** The issues in form order: name, then duration (missing or malformed). */
    def issues: List[FieldIssue[Field]] =
      val durationIssue =
        if hours.trim.nn.isEmpty then Some("Durata obbligatoria")
        else Option.when(Formats.parseDuration(hours).isEmpty)("Durata non valida (usa h:mm, es. 1:35, o ore intere)")
      List(
        Option.when(name.trim.nn.isEmpty)(FieldIssue(Field.Name, "Nome obbligatorio")),
        durationIssue.map(FieldIssue(Field.Duration, _)),
      ).flatten

    def errors: List[String] = issues.messages

    /** The request for the current state; `None` while the duration does not parse (see [[issues]]). */
    def request: Option[TaskRequest] =
      Formats.parseDuration(hours).map { minutes =>
        TaskRequest(
          name.trim.nn,
          Some(description.trim.nn).filter(_.nonEmpty),
          minutes,
          Try(UUID.fromString(defaultEmployee).nn).toOption,
        )
      }
  end State

  val empty: State = State(None, "", "", defaultHours, "")

  def fromResponse(task: TaskResponse): State =
    State(
      Some(task.id),
      task.name,
      task.description.getOrElse(""),
      Formats.duration(task.requiredMinutes),
      task.defaultEmployeeId.map(_.toString).getOrElse(""),
    )
end TaskForm
