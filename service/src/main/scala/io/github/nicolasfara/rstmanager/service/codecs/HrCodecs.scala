package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.circe.given

/** Circe codecs for the employee aggregate graph, used to persist [[EmployeeEvent]] in the journal. */
object HrCodecs:
  import CommonCodecs.given

  given Codec[EmployeeInfo] = deriveCodec
  given Codec[Contract] = deriveCodec
  given Codec[HoursOverride] = deriveCodec
  given Codec[BudgetHours] = deriveCodec
  given Codec[Employee] = deriveCodec
  given Codec[EmployeeEvent] = deriveCodec
  given Codec[EmployeeService.Notification] = deriveCodec
