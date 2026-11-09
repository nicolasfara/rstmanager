package io.github.nicolasfara.rstmanager.hr.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.calendar.{ ClosureId, CompanyClosure }
import io.github.nicolasfara.rstmanager.hr.service.CompanyCalendarHttpApi.{ ClosureRequest, ClosureResponse }

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class CompanyCalendarHttpDtosTest extends AnyFlatSpecLike:
  private val id: UUID = UUID.fromString("00000000-0000-0000-0000-0000000000c9").nn

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def errors[A](value: ValidatedNec[String, A]): List[String] =
    value.fold(_.toChain.toList, valid => fail(s"Unexpectedly valid: $valid"))

  "A closure request" should "read a period from ISO dates or date-times, both bounds included" in:
    val request = ClosureRequest("period", "Chiusura estiva", Some("2026-08-10T00:00:00.000Z"), Some("2026-08-21"), None, None)

    validOrFail(request.toDomain(id)) shouldEqual
      CompanyClosure.Period(ClosureId(id), LocalDate.parse("2026-08-10"), LocalDate.parse("2026-08-21"), "Chiusura estiva".refineUnsafe)

  it should "read an annual closure, accepting 29 February" in:
    validOrFail(ClosureRequest("annual", "Natale", None, None, Some(12), Some(25)).toDomain(id)) shouldEqual
      CompanyClosure.Annual(ClosureId(id), 12.refineUnsafe, 25.refineUnsafe, "Natale".refineUnsafe)
    ClosureRequest("Annual", "Bisestile", None, None, Some(2), Some(29)).toDomain(id).isValid shouldBe true

  it should "report every missing or invalid field" in:
    errors(ClosureRequest("period", "", None, Some("not a date"), None, None).toDomain(id)) shouldEqual
      List("startDate is required.", "endDate must be an ISO-8601 date.")
    errors(ClosureRequest("annual", "Aprile", None, None, Some(4), None).toDomain(id)) shouldEqual List("day is required.")
    errors(ClosureRequest("annual", "Aprile", None, None, Some(4), Some(31)).toDomain(id)) shouldEqual List("Day 31 does not exist in month 4")
    errors(ClosureRequest("weekly", "Mai", None, None, None, None).toDomain(id)) shouldEqual
      List("kind 'weekly' is not supported. Use period or annual.")

  "A closure response" should "write periods with dates and annual closures with month and day" in:
    val period = validOrFail(ClosureRequest.example.toDomain(id))
    ClosureResponse.fromDomain(period) shouldEqual
      ClosureResponse(id, "period", "Chiusura estiva", Some("2026-08-10T00:00:00.000Z"), Some("2026-08-21T00:00:00.000Z"), None, None)
    val annual = validOrFail(ClosureRequest("annual", "Ferragosto", None, None, Some(8), Some(15)).toDomain(id))
    ClosureResponse.fromDomain(annual) shouldEqual ClosureResponse(id, "annual", "Ferragosto", None, None, Some(8), Some(15))
end CompanyCalendarHttpDtosTest
