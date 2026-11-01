package io.github.nicolasfara.rstmanager.planning.service

import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.service.codecs.{ DerivedEventSchema, EventEnvelope, Upcasts }

import io.circe.{ Decoder, JsonObject }

/**
 * Persisted schema of the [[PlanningEvent]] journal (`planning_journal`).
 *
 * Versions:
 *   - v1: task efforts are in whole hours: `assignedHours` / `remainingHoursAfterSlice` in the slices, `requiredHours` in
 *     `NoFutureCapacity` and `InvalidEmployeeAssignment`.
 *   - v2: they are `assignedDuration` / `remainingDurationAfterSlice` / `requiredDuration`, in minutes. `availableHours` stays in hours.
 *
 * The pre-deployment shape (a planning `window`, no `unplannedOrders`) is not supported and fails to decode.
 */
object PlanningEventSchema extends DerivedEventSchema[PlanningEvent](using PlanningCodecs.planningEventData):
  private val hourFields: Map[String, String] = Map(
    "assignedHours" -> "assignedDuration",
    "remainingHoursAfterSlice" -> "remainingDurationAfterSlice",
    "requiredHours" -> "requiredDuration",
  )

  val currentVersion: Int = 2

  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = Map(1 -> Upcasts.hoursToMinutes(1, hourFields))

  def legacyVersion(tag: String, data: JsonObject): Int = if Upcasts.hasKey(data, hourFields.keySet) then 1 else 2
end PlanningEventSchema
