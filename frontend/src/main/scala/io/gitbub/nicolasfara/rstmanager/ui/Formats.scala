package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.scalajs.js

/** Small presentation helpers: date formatting, id shortening and a stable colour palette per order. */
object Formats:

  private def pad2(value: Int): String = if value < 10 then s"0$value" else value.toString

  /** Formats an ISO-8601 string as `dd/MM/yyyy`; falls back to the raw value if unparseable. */
  def date(iso: String): String =
    val parsed = new js.Date(iso)
    if parsed.getTime().isNaN then iso
    else s"${pad2(parsed.getDate().toInt)}/${pad2(parsed.getMonth().toInt + 1)}/${parsed.getFullYear().toInt}"

  /** Formats an ISO-8601 string as `dd/MM/yyyy HH:mm`. */
  def dateTime(iso: String): String =
    val parsed = new js.Date(iso)
    if parsed.getTime().isNaN then iso
    else s"${date(iso)} ${pad2(parsed.getHours().toInt)}:${pad2(parsed.getMinutes().toInt)}"

  /** First segment of a UUID, handy for compact labels when no human name is available. */
  def shortId(id: UUID): String = id.toString.take(8)

  /** Whole calendar days from `fromIso` to `toIso` (negative when `toIso` is earlier). `None` if either is unparseable. */
  def daysUntil(fromIso: String, toIso: String): Option[Int] =
    val from = new js.Date(fromIso)
    val to = new js.Date(toIso)
    if from.getTime().isNaN || to.getTime().isNaN then None
    else Some(math.floor((to.getTime() - from.getTime()) / 86400000.0).toInt)
end Formats
