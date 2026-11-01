package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent

import io.circe.Json
import org.scalatest.matchers.should.Matchers.*

class TaskEventSchemaTest extends JournalGoldenSuite[TaskEvent]("task-events", TaskEventSchema, JournalGoldenSuite.caseNames[TaskEvent]):
  def current: List[String] = List("v2-task-created", "v2-task-created-no-employee", "v2-task-updated", "v2-task-deleted")

  /** Pre-version rows, with the estimate in hours (v1, before and after default employees existed) or in minutes (v2). */
  def legacy: List[(String, String)] = List(
    "legacy-v1-task-created" -> "v2-task-created-no-employee",
    "legacy-v1-task-updated" -> "v2-task-updated",
    "legacy-v2-task-created" -> "v2-task-created",
    "legacy-v2-task-deleted" -> "v2-task-deleted",
  )

  it should "reject an unknown event type" in:
    decodingError(Json.obj("version" -> Json.fromInt(2), "event" -> Json.obj("TaskArchived" -> Json.obj()))) should include("TaskArchived")
end TaskEventSchemaTest
