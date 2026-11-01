package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent

import io.circe.{ Decoder, Json, JsonObject }

/**
 * Persisted schema of the [[ManufacturingEvent]] journal (`manufacturings_journal`).
 *
 * Versions:
 *   - v1: per-task effort overrides are `taskHours`, in whole hours.
 *   - v2: they are `taskDurations`, in minutes.
 *
 * Rows written before the overrides (or the default employees) existed lack the field and are read as v2 with no overrides.
 */
object ManufacturingEventSchema extends DerivedEventSchema[ManufacturingEvent](using WorkCodecs.manufacturingEventData):
  val currentVersion: Int = 2

  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = Map(1 -> taskHoursToMinutes)

  def legacyVersion(tag: String, data: JsonObject): Int =
    if data("manufacturing").flatMap(_.asObject).exists(_.contains("taskHours")) then 1 else 2

  /** v1 → v2: the `taskHours` map becomes `taskDurations`, with every override in minutes. */
  private def taskHoursToMinutes(envelope: EventEnvelope): Decoder.Result[EventEnvelope] =
    val next = envelope.copy(version = 2)
    envelope.data("manufacturing") match
      case None => Right(next)
      case Some(manufacturing) =>
        manufacturing.hcursor.get[Map[String, Int]]("taskHours").map { hours =>
          val durations = Json.fromFields(hours.toList.map((taskId, h) => taskId -> Json.fromInt(h * 60)))
          next.copy(data = envelope.data.add("manufacturing", manufacturing.mapObject(_.remove("taskHours").add("taskDurations", durations))))
        }
end ManufacturingEventSchema
