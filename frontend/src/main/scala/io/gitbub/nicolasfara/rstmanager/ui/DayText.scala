package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js

/** Calendar-day text helpers for the order forms: `yyyy-MM-dd` date-input values, today's day, and their compact Italian display forms. */
private[ui] object DayText:

  private def pad2(value: Int): String = if value < 10 then s"0$value" else value.toString

  /** Extracts the `yyyy-MM-dd` day part of an ISO-8601 instant for a date input's initial value. */
  def dayOf(iso: String): String = iso.take(10)

  /** Today's local calendar day as `yyyy-MM-dd`. */
  def today(): String =
    val now = new js.Date()
    s"${now.getFullYear().toInt}-${pad2(now.getMonth().toInt + 1)}-${pad2(now.getDate().toInt)}"

  /** `(year, month, day)` of a `yyyy-MM-dd` text; `None` when it is not such a day. */
  private def parts(day: String): Option[(Int, Int, Int)] =
    day.trim.nn.split("-").nn.toList.map(_.nn.toIntOption) match
      case Some(year) :: Some(month) :: Some(dayOfMonth) :: Nil if month >= 1 && month <= 12 && dayOfMonth >= 1 && dayOfMonth <= 31 =>
        Some((year, month, dayOfMonth))
      case _ => None

  /** `yyyy-MM-dd` as `dd/MM` (e.g. in "Vuota = deadline lavorazione 12/03"); empty when unreadable. */
  def short(day: String): String = parts(day).fold("")((_, month, dayOfMonth) => s"${pad2(dayOfMonth)}/${pad2(month)}")

  /** `yyyy-MM-dd` as `dd/MM/yyyy`; empty when unreadable. */
  def long(day: String): String = parts(day).fold("")((year, month, dayOfMonth) => s"${pad2(dayOfMonth)}/${pad2(month)}/$year")
end DayText
