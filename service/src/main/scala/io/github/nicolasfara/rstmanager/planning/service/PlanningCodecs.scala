package io.github.nicolasfara.rstmanager.planning.service

import io.github.nicolasfara.rstmanager.hr.domain.DailyHours
import io.github.nicolasfara.rstmanager.planning.*
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.work.domain.task.TaskHours

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import io.circe.{ Codec, Decoder, Encoder }
import io.circe.generic.semiauto.deriveCodec

/**
 * Circe codecs used to persist planning events and notifications in the edomata journal and outbox.
 *
 * Dates are serialized as ISO-8601 strings and refined hour types as plain integers re-validated on read. The remaining codecs are derived from the
 * planning model case classes and enums.
 */
object PlanningCodecs:
  given Codec[DateTime] = Codec.from(
    Decoder.decodeString.emap(raw => Either.catchNonFatal(DateTime.parse(raw).nn).leftMap(_ => s"Invalid ISO-8601 date-time: $raw")),
    Encoder.encodeString.contramap(_.toString),
  )

  given Codec[TaskHours] = Codec.from(Decoder.decodeInt.emap(TaskHours.either), Encoder.encodeInt.contramap(_.value))

  given Codec[DailyHours] = Codec.from(Decoder.decodeInt.emap(DailyHours.either), Encoder.encodeInt.contramap(_.value))

  given Codec[PlanningTrigger] = deriveCodec
  given Codec[PlanningRequest] = deriveCodec
  given Codec[CandidateEmployee] = deriveCodec
  given Codec[ScheduledTaskSlice] = deriveCodec
  given Codec[DailySchedule] = deriveCodec
  given Codec[DelayedOrder] = deriveCodec
  given Codec[DelayedManufacturing] = deriveCodec
  given Codec[UnplannedReason] = deriveCodec
  given Codec[UnplannedTask] = deriveCodec
  given Codec[UnplannedOrder] = deriveCodec
  given Codec[PlanningWarning] = deriveCodec
  given Codec[PlanningResult] = deriveCodec
  given Codec[PlanningError] = deriveCodec
  given Codec[PlanningEvent] = deriveCodec
  given Codec[PlanningService.Notification] = deriveCodec
end PlanningCodecs
