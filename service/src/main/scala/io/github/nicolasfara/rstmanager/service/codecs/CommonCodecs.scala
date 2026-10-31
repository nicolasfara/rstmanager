package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.{ DailyHours, WeeklyHours }
import io.github.nicolasfara.rstmanager.work.domain.task.TaskDuration

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import io.circe.{ Codec, Decoder, Encoder }

/**
 * Circe codecs shared by every entity codec: `DateTime`, calendar days (`LocalDate`), and the opaque refined-int budget/effort types.
 *
 * The opaque `RefinedType` companions (`TaskDuration`, `WeeklyHours`, `DailyHours`) are not visible as `Int :| C` outside their definition, so
 * `iron-circe` cannot derive them; they get explicit codecs that re-validate on read. Direct `A :| C` fields are handled by the `iron-circe` givens
 * imported where needed.
 */
object CommonCodecs:
  given Codec[DateTime] = Codec.from(
    Decoder.decodeString.emap(raw => Either.catchNonFatal(DateTime.parse(raw)).leftMap(_ => s"Invalid ISO-8601 date-time: $raw")),
    Encoder.encodeString.contramap(_.toString),
  )

  /**
   * Reads a calendar day written either as `YYYY-MM-DD` or as an ISO-8601 date-time, in which case its date is taken in the offset it was written
   * with (no time-zone guess). The date-time form covers data persisted before days were modelled as dates, and the `…T00:00:00.000Z` strings the
   * frontend sends.
   */
  def parseCalendarDay(raw: String): Either[String, LocalDate] =
    Either
      .catchNonFatal(LocalDate.parse(raw))
      .orElse(Either.catchNonFatal(DateTime.parse(raw).toLocalDate))
      .leftMap(_ => s"Invalid ISO-8601 date: $raw")

  /** Calendar days are written as `YYYY-MM-DD` and read with [[parseCalendarDay]]. */
  given Codec[LocalDate] = Codec.from(Decoder.decodeString.emap(parseCalendarDay), Encoder.encodeString.contramap(_.toString))

  given Codec[TaskDuration] = Codec.from(Decoder.decodeInt.emap(TaskDuration.either), Encoder.encodeInt.contramap(_.value))
  given Codec[WeeklyHours] = Codec.from(Decoder.decodeInt.emap(WeeklyHours.either), Encoder.encodeInt.contramap(_.value))
  given Codec[DailyHours] = Codec.from(Decoder.decodeInt.emap(DailyHours.either), Encoder.encodeInt.contramap(_.value))
end CommonCodecs
