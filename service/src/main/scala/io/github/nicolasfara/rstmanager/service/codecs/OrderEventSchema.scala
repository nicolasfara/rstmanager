package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent

import io.circe.{ Decoder, JsonObject }

/**
 * Persisted schema of the [[OrderEvent]] journal (`orders_journal`).
 *
 * Versions:
 *   - v1: task estimates and progress are in whole hours: `expectedHours` / `completedHours` in the scheduled tasks, `withHours` on
 *     completion, `completedHours` / `expectedHours` on the progress and estimate changes.
 *   - v2: they are `expectedDuration` / `completedDuration` / `withDuration`, in minutes.
 *
 * `ManufacturingTaskAdvanced` / `ManufacturingTaskRolledBack` kept their field names across the switch, so their version cannot be told
 * from the data; no release ever wrote them, and they are read as v2. Rows predating the optional fields added along the way
 * (descriptions, preferred employees, dependency graphs, `occurredOn`) decode through the tolerant codecs of [[WorkCodecs]].
 */
object OrderEventSchema extends DerivedEventSchema[OrderEvent](using WorkCodecs.orderEventData):
  private val hourFields: Map[String, String] =
    Map("expectedHours" -> "expectedDuration", "completedHours" -> "completedDuration", "withHours" -> "withDuration")

  val currentVersion: Int = 2

  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = Map(1 -> Upcasts.hoursToMinutes(1, hourFields))

  def legacyVersion(tag: String, data: JsonObject): Int = if Upcasts.hasKey(data, hourFields.keySet) then 1 else 2
end OrderEventSchema
