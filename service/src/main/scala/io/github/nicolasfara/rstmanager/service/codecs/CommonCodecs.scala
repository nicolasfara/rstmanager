package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.{ DailyHours, WeeklyHours }
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import io.circe.{ Codec, Decoder, Encoder }

/**
 * Circe codecs shared by every entity codec: `DateTime`, joda `Interval`, and the opaque refined-int budget/effort types.
 *
 * The opaque `RefinedType` companions (`TaskHours`, `WeeklyHours`, `DailyHours`) are not visible as `Int :| C` outside
 * their definition, so `iron-circe` cannot derive them; they get explicit codecs that re-validate on read. Direct
 * `A :| C` fields are handled by the `iron-circe` givens imported where needed.
 */
object CommonCodecs:
  given Codec[DateTime] = Codec.from(
    Decoder.decodeString.emap(raw => Either.catchNonFatal(DateTime.parse(raw).nn).leftMap(_ => s"Invalid ISO-8601 date-time: $raw")),
    Encoder.encodeString.contramap(_.toString),
  )

  given Codec[Interval] = Codec.from(
    Decoder.forProduct2("start", "end")((start: DateTime, end: DateTime) => start.to(end)),
    Encoder.forProduct2("start", "end")((interval: Interval) => (interval.getStart.nn, interval.getEnd.nn)),
  )

  given Codec[TaskHours] = Codec.from(Decoder.decodeInt.emap(TaskHours.either), Encoder.encodeInt.contramap(_.value))
  given Codec[WeeklyHours] = Codec.from(Decoder.decodeInt.emap(WeeklyHours.either), Encoder.encodeInt.contramap(_.value))
  given Codec[DailyHours] = Codec.from(Decoder.decodeInt.emap(DailyHours.either), Encoder.encodeInt.contramap(_.value))
end CommonCodecs
