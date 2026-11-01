package io.github.nicolasfara.rstmanager.planning.service

import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.planning.service.PlanningCodecs.given
import io.github.nicolasfara.rstmanager.service.codecs.JournalGoldenSuite

class PlanningEventSchemaTest
    extends JournalGoldenSuite[PlanningEvent]("planning-events", PlanningEventSchema, JournalGoldenSuite.caseNames[PlanningEvent]):
  def current: List[String] = List(
    "v2-planning-requested",
    "v2-task-slice-assigned",
    "v2-order-delayed",
    "v2-manufacturing-delayed",
    "v2-order-marked-unplanned",
    "v2-warning-raised",
    "v2-schedule-computed",
    "v2-schedule-rejected",
  )

  /** Pre-version rows: efforts in hours (v1), and a per-fact row in minutes from before planning runs wrote a single event (v2). */
  def legacy: List[(String, String)] = List(
    "legacy-v1-task-slice-assigned" -> "v2-task-slice-assigned",
    "legacy-v1-order-marked-unplanned" -> "v2-order-marked-unplanned",
    "legacy-v1-schedule-computed" -> "v2-schedule-computed",
    "legacy-v2-task-slice-assigned" -> "v2-task-slice-assigned",
    "legacy-v2-planning-requested" -> "v2-planning-requested",
  )
end PlanningEventSchemaTest
