package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.service.codecs.WorkCodecs.given
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent

import org.scalatest.matchers.should.Matchers.*

class ManufacturingEventSchemaTest
    extends JournalGoldenSuite[ManufacturingEvent](
      "manufacturing-events",
      ManufacturingEventSchema,
      JournalGoldenSuite.caseNames[ManufacturingEvent],
    ):
  private def v2Counterparts: List[String] =
    List("manufacturing-created", "manufacturing-created-no-overrides", "manufacturing-updated", "manufacturing-deleted")

  def current: List[String] = v2Counterparts.map("v3-" + _) :+ "v3-manufacturing-created-with-materials"

  /**
   * Pre-version rows: with hour overrides (v1), and from before the overrides and default employees existed (v2); then the versioned v2 rows, written
   * before the per-unit materials existed.
   */
  def legacy: List[(String, String)] = List(
    "legacy-v1-manufacturing-created" -> "v3-manufacturing-created",
    "legacy-v2-manufacturing-created-before-overrides" -> "v3-manufacturing-created-no-overrides",
    "legacy-v2-manufacturing-deleted" -> "v3-manufacturing-deleted",
  ) ++ v2Counterparts.map(name => s"v2-$name" -> s"v3-$name")

  "The manufacturing journal codec" should "read a v2 template with no materials, and the per-unit materials of a v3 one" in:
    decode("v2-manufacturing-created") match
      case ManufacturingEvent.ManufacturingCreated(manufacturing) => manufacturing.materials shouldBe empty
      case other => fail(s"Unexpected event $other")
    decode("v3-manufacturing-created-with-materials") match
      case ManufacturingEvent.ManufacturingCreated(manufacturing) => manufacturing.materials.map(_.quantity.value) shouldEqual List(2500L, 4000L)
      case other => fail(s"Unexpected event $other")
end ManufacturingEventSchemaTest
