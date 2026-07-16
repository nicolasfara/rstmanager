package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js

/** Helpers for pre-filling editable business codes in create forms. */
object GeneratedCodes:
  private def currentYear(): Int =
    new js.Date().getFullYear().toInt

  private def pad3(value: Int): String =
    val raw = value.toString
    "0" * math.max(0, 3 - raw.length) + raw

  private def sequenceFor(prefix: String, year: Int, value: String): Option[Int] =
    val expectedPrefix = s"$prefix-$year-"
    if value.startsWith(expectedPrefix) then value.drop(expectedPrefix.length).toIntOption else None

  def next(prefix: String, existing: Iterable[String]): String =
    val year = currentYear()
    val nextSequence = existing.flatMap(sequenceFor(prefix, year, _)).foldLeft(0)(math.max) + 1
    s"$prefix-$year-${pad3(nextSequence)}"
end GeneratedCodes
