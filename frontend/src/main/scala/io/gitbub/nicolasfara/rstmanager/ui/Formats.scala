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

  /** Extracts the local calendar date as `YYYY-MM-DD` from any ISO datetime string (ignores time and timezone offset). */
  def dateKey(iso: String): String =
    val d = new js.Date(iso)
    if d.getTime().isNaN then iso
    else s"${d.getFullYear().toInt}-${pad2(d.getMonth().toInt + 1)}-${pad2(d.getDate().toInt)}"

  /** Returns a stable `YYYY-MM-DD` key for the Monday of the week containing `iso`. Used to group planning days by week. */
  def weekKey(iso: String): String =
    val d = new js.Date(iso)
    val day = d.getDay().toInt // 0=Sun, 1=Mon, …, 6=Sat
    val diffToMonday = if day == 0 then -6 else 1 - day
    val monday = new js.Date(d.getTime() + diffToMonday.toDouble * 86400000.0)
    s"${monday.getFullYear().toInt}-${pad2(monday.getMonth().toInt + 1)}-${pad2(monday.getDate().toInt)}"

  /** Returns the five `YYYY-MM-DD` keys for Mon–Fri of the week whose Monday is `mondayKey`. */
  def weekDays(mondayKey: String): List[String] =
    val monday = new js.Date(mondayKey)
    (0 until 5).map { offset =>
      val d = new js.Date(monday.getTime() + offset.toDouble * 86400000.0)
      s"${d.getFullYear().toInt}-${pad2(d.getMonth().toInt + 1)}-${pad2(d.getDate().toInt)}"
    }.toList
end Formats
