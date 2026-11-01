package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskDescription, TaskDuration, TaskId, TaskName }
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent

import io.circe.{ Codec, Decoder, DecodingFailure, Json, JsonObject }
import io.circe.syntax.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.circe.given

/**
 * Persisted schema of the [[TaskEvent]] journal (`tasks_journal`).
 *
 * Versions:
 *   - v1: the task estimate is `requiredHours`, in whole hours (rows written before the switch to minutes).
 *   - v2: the task estimate is `requiredDuration`, in minutes.
 */
object TaskEventSchema extends EventSchema[TaskEvent]:
  import CommonCodecs.given

  val currentVersion: Int = 2

  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] =
    Map(1 -> Upcasts.hoursToMinutes(1, Map("requiredHours" -> "requiredDuration")))

  def legacyVersion(tag: String, data: JsonObject): Int = if Upcasts.hasKey(data, Set("requiredHours")) then 1 else 2

  def write(event: TaskEvent): JsonObject = event match
    case TaskEvent.TaskCreated(task) => JsonObject("TaskCreated" -> Json.obj("task" -> task.asJson))
    case TaskEvent.TaskUpdated(task) => JsonObject("TaskUpdated" -> Json.obj("task" -> task.asJson))
    case TaskEvent.TaskDeleted => JsonObject("TaskDeleted" -> Json.obj())

  def read(tag: String, data: JsonObject): Decoder.Result[TaskEvent] =
    val cursor = Json.fromJsonObject(data).hcursor
    tag match
      case "TaskCreated" => cursor.get[Task]("task").map(TaskEvent.TaskCreated(_))
      case "TaskUpdated" => cursor.get[Task]("task").map(TaskEvent.TaskUpdated(_))
      case "TaskDeleted" => Right(TaskEvent.TaskDeleted)
      case other => Left(DecodingFailure(s"Unknown task event type: $other", cursor.history))

  /** The v2 task record, with its field names pinned so that renaming a `Task` field does not change the stored shape. */
  private given Codec.AsObject[Task] =
    Codec.forProduct5[Task, TaskId, String :| TaskName, Option[String :| TaskDescription], TaskDuration, Option[EmployeeId]](
      "id",
      "name",
      "taskDescription",
      "requiredDuration",
      "defaultEmployeeId",
    )(Task(_, _, _, _, _))(task => (task.id, task.name, task.taskDescription, task.requiredDuration, task.defaultEmployeeId))
end TaskEventSchema
