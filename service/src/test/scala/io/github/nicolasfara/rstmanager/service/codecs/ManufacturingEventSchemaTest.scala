package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent

class ManufacturingEventSchemaTest
    extends JournalGoldenSuite[ManufacturingEvent](
      "manufacturing-events",
      ManufacturingEventSchema,
      JournalGoldenSuite.caseNames[ManufacturingEvent],
    ):
  def current: List[String] =
    List("v2-manufacturing-created", "v2-manufacturing-created-no-overrides", "v2-manufacturing-updated", "v2-manufacturing-deleted")

  /** Pre-version rows: with hour overrides (v1), and from before the overrides and default employees existed (v2). */
  def legacy: List[(String, String)] = List(
    "legacy-v1-manufacturing-created" -> "v2-manufacturing-created",
    "legacy-v2-manufacturing-created-before-overrides" -> "v2-manufacturing-created-no-overrides",
    "legacy-v2-manufacturing-deleted" -> "v2-manufacturing-deleted",
  )
end ManufacturingEventSchemaTest
