package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent

import io.circe.{ Decoder, Json, JsonObject }

/**
 * Persisted schema of the [[OrderEvent]] journal (`orders_journal`).
 *
 * Versions:
 *   - v1: task estimates and progress are in whole hours: `expectedHours` / `completedHours` in the scheduled tasks, `withHours` on completion,
 *     `completedHours` / `expectedHours` on the progress and estimate changes.
 *   - v2: they are `expectedDuration` / `completedDuration` / `withDuration`, in minutes.
 *   - v3: every scheduled manufacturing `info` (in `OrderCreated`, `ManufacturingAdded`) carries its `materials` (product, quantity in thousandths,
 *     frozen unit price in 1e-4 euro); `ManufacturingMaterialsChanged` replaces them. Older rows get `"materials": []`.
 *
 * Rows without a version predate materials, so they are v1 or v2. `ManufacturingTaskAdvanced` / `ManufacturingTaskRolledBack` kept their field names
 * across the switch to minutes, so their version cannot be told from the data; no release ever wrote them, and they are read as v2. Rows predating
 * the optional fields added along the way (descriptions, preferred employees, dependency graphs, `occurredOn`) decode through the tolerant codecs of
 * [[WorkCodecs]].
 */
object OrderEventSchema extends DerivedEventSchema[OrderEvent](using WorkCodecs.orderEventData):
  private val hourFields: Map[String, String] =
    Map("expectedHours" -> "expectedDuration", "completedHours" -> "completedDuration", "withHours" -> "withDuration")

  /** A scheduled manufacturing `info` object: the only one with both its tasks and its completion date. */
  private def isManufacturingInfo(data: JsonObject): Boolean = data.contains("tasks") && data.contains("completionDate")

  val currentVersion: Int = 3

  val upcasters: Map[Int, EventEnvelope => Decoder.Result[EventEnvelope]] = Map(
    1 -> Upcasts.hoursToMinutes(1, hourFields),
    2 -> Upcasts.addFieldWhere(2, isManufacturingInfo, "materials", Json.arr()),
  )

  def legacyVersion(tag: String, data: JsonObject): Int = if Upcasts.hasKey(data, hourFields.keySet) then 1 else 2
