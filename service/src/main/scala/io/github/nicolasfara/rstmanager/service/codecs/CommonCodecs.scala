package io.github.nicolasfara.rstmanager.service.codecs

import java.util.UUID

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.{ DailyHours, EmployeeId, WeeklyHours }
import io.github.nicolasfara.rstmanager.planning.PlanningRequestId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingId
import io.github.nicolasfara.rstmanager.work.domain.order.OrderId
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId }
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.*
import io.circe.{ Codec, Decoder, Encoder, KeyDecoder, KeyEncoder }

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

  /** Identifiers are written as their plain UUID string, exactly as before they became distinct types. */
  private def idCodec[Id](wrap: UUID => Id, unwrap: Id => UUID): Codec[Id] =
    Codec.from(Decoder.decodeUUID.map(wrap), Encoder.encodeUUID.contramap(unwrap))
  private def idKeyDecoder[Id](wrap: UUID => Id): KeyDecoder[Id] = KeyDecoder.decodeKeyUUID.map(wrap)
  private def idKeyEncoder[Id](unwrap: Id => UUID): KeyEncoder[Id] = KeyEncoder.encodeKeyUUID.contramap(unwrap)

  given Codec[CustomerId] = idCodec(CustomerId(_), _.value)
  given Codec[EmployeeId] = idCodec(EmployeeId(_), _.value)
  given Codec[OrderId] = idCodec(OrderId(_), _.value)
  given Codec[TaskId] = idCodec(TaskId(_), _.value)
  given Codec[ManufacturingId] = idCodec(ManufacturingId(_), _.value)
  given Codec[ScheduledTaskId] = idCodec(ScheduledTaskId(_), _.value)
  given Codec[ScheduledManufacturingId] = idCodec(ScheduledManufacturingId(_), _.value)
  given Codec[PlanningRequestId] = idCodec(PlanningRequestId(_), _.value)

  // Identifiers used as JSON object keys (per-task maps).
  given KeyDecoder[TaskId] = idKeyDecoder(TaskId(_))
  given KeyEncoder[TaskId] = idKeyEncoder(_.value)
  given KeyDecoder[ScheduledTaskId] = idKeyDecoder(ScheduledTaskId(_))
  given KeyEncoder[ScheduledTaskId] = idKeyEncoder(_.value)

  given Codec[TaskDuration] = Codec.from(Decoder.decodeInt.emap(TaskDuration.either), Encoder.encodeInt.contramap(_.value))
  given Codec[WeeklyHours] = Codec.from(Decoder.decodeInt.emap(WeeklyHours.either), Encoder.encodeInt.contramap(_.value))
  given Codec[DailyHours] = Codec.from(Decoder.decodeInt.emap(DailyHours.either), Encoder.encodeInt.contramap(_.value))
end CommonCodecs
