package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.hr.domain.calendar.{ CompanyCalendarService, CompanyClosure }
import io.github.nicolasfara.rstmanager.hr.domain.calendar.events.CompanyCalendarEvent
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent

import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.{ Codec, Decoder, DecodingFailure, Encoder, Json }
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given

/** Circe codecs for the HR aggregate graphs, used to persist [[EmployeeEvent]] and [[CompanyCalendarEvent]] in their journals. */
object HrCodecs:
  import CommonCodecs.given

  given Codec[EmployeeInfo] = deriveCodec
  given Codec[Contract] = deriveCodec
  given Codec[WorkingDayOverride] = deriveCodec

  /**
   * Vacations are written as an inclusive `{firstDay, lastDay}` range. Events persisted before that stored a joda interval `{interval: {start, end}}`
   * whose `end` held the last vacation day entered in the UI; it is read back as `lastDay`, honouring the inclusive meaning the user intended.
   */
  given Codec[VacationOverride] = Codec.from(
    Decoder
      .forProduct2("firstDay", "lastDay")(VacationOverride.apply)
      .or(Decoder.instance { cursor =>
        val interval = cursor.downField("interval")
        (interval.get[LocalDate]("start"), interval.get[LocalDate]("end")).mapN(VacationOverride.apply)
      }),
    Encoder.forProduct2("firstDay", "lastDay")(vacation => (vacation.firstDay, vacation.lastDay)),
  )

  /**
   * Same wrapper-object layout as circe's derived sum codec (`{"WorkingDayOverride": …}` / `{"VacationOverride": …}`), so stored events still read.
   */
  given Codec[HoursOverride] = Codec.from(
    Decoder.instance { cursor =>
      cursor
        .get[WorkingDayOverride]("WorkingDayOverride")
        .orElse(cursor.get[VacationOverride]("VacationOverride"))
        .leftMap(_ => DecodingFailure("Expected a WorkingDayOverride or VacationOverride object", cursor.history))
    },
    Encoder.instance {
      case workingDay: WorkingDayOverride => Json.obj("WorkingDayOverride" -> workingDay.asJson)
      case vacation: VacationOverride => Json.obj("VacationOverride" -> vacation.asJson)
    },
  )

  given Codec[BudgetHours] = deriveCodec
  given Codec[Employee] = deriveCodec

  /** Current-version data of the employee journal; it is persisted through [[EmployeeEventSchema]]. */
  val employeeEventData: Codec.AsObject[EmployeeEvent] = deriveCodec
  given Codec[EmployeeEvent] = EventSchema.codec(EmployeeEventSchema)

  given Codec[EmployeeService.Notification] = deriveCodec

  /** Circe's derived sum layout: `{"Period": {id, firstDay, lastDay, reason}}` or `{"Annual": {id, month, dayOfMonth, reason}}`. */
  given Codec[CompanyClosure] = deriveCodec

  /** Current-version data of the company calendar journal; it is persisted through [[CompanyCalendarEventSchema]]. */
  val companyCalendarEventData: Codec.AsObject[CompanyCalendarEvent] = deriveCodec
  given Codec[CompanyCalendarEvent] = EventSchema.codec(CompanyCalendarEventSchema)

  given companyCalendarNotificationCodec: Codec[CompanyCalendarService.Notification] = deriveCodec
end HrCodecs
