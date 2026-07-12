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

  /** Tailwind colour classes (background + text + border) assigned deterministically from an id. */
  private val palette: Vector[String] = Vector(
    "bg-rose-50 text-rose-700 border-rose-200",
    "bg-amber-50 text-amber-700 border-amber-200",
    "bg-emerald-50 text-emerald-700 border-emerald-200",
    "bg-sky-50 text-sky-700 border-sky-200",
    "bg-violet-50 text-violet-700 border-violet-200",
    "bg-teal-50 text-teal-700 border-teal-200",
    "bg-fuchsia-50 text-fuchsia-700 border-fuchsia-200",
    "bg-lime-50 text-lime-700 border-lime-200",
  )

  def colorFor(id: UUID): String =
    val index = math.floorMod(id.hashCode, palette.size)
    palette(index)
end Formats
