package io.github.nicolasfara.rstmanager.service.codecs

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.*
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent
import io.github.nicolasfara.rstmanager.service.codecs.HrCodecs.given

import com.github.nscala_time.time.Imports.LocalDate
import io.circe.parser.decode
import io.circe.syntax.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class HrCodecsTest extends AnyFlatSpecLike:
  /**
   * An `EmployeeCreated` event exactly as the journal stored it before calendar days were modelled as `LocalDate` (joda date-times, interval
   * vacation).
   */
  private val legacyEmployeeCreated =
    """{"EmployeeCreated":{"employee":{"id":"00000000-0000-0000-0000-0000000000e1","info":{"name":"Mario","surname":"Rossi"},"contract":{"FixedTerm":{"startDate":"2026-01-01T00:00:00.000Z","endDate":"2026-12-31T00:00:00.000Z"}},"budgetHours":{"default":40,"overrides":[{"WorkingDayOverride":{"hours":6,"reason":"Reduced shift","day":"2026-06-15T00:00:00.000Z"}},{"VacationOverride":{"interval":{"start":"2026-08-03T00:00:00.000Z","end":"2026-08-07T00:00:00.000Z"}}}]}}}}"""

  private def day(iso: String): LocalDate = LocalDate.parse(iso)

  private val employee = Employee(
    EmployeeId(UUID.fromString("00000000-0000-0000-0000-0000000000e1").nn),
    EmployeeInfo("Mario".refineUnsafe[Name], "Rossi".refineUnsafe[Surname]),
    Contract.FixedTerm(day("2026-01-01"), day("2026-12-31")),
    BudgetHours(
      WeeklyHours.applyUnsafe(40),
      List(
        WorkingDayOverride(DailyHours.applyUnsafe(6), Some("Reduced shift".refineUnsafe[OverrideReason]), day("2026-06-15")),
        VacationOverride(day("2026-08-03"), day("2026-08-07")),
      ),
    ),
  )

  "HrCodecs" should "decode employee events persisted with date-times, reading the vacation end as its last day" in:
    decode[EmployeeEvent](legacyEmployeeCreated) shouldEqual Right(EmployeeEvent.EmployeeCreated(employee))

  it should "round-trip employee events with calendar days" in:
    val event: EmployeeEvent = EmployeeEvent.EmployeeCreated(employee)
    val json = event.asJson

    decode[EmployeeEvent](json.noSpaces) shouldEqual Right(event)
    json.hcursor.downField("EmployeeCreated").downField("employee").downField("contract").downField("FixedTerm").get[String]("endDate") shouldEqual
      Right("2026-12-31")

  it should "read a legacy date-time in the offset it was written with, whatever the server time zone" in:
    CommonCodecs.parseCalendarDay("2026-06-15T00:00:00.000+02:00") shouldEqual Right(day("2026-06-15"))
    CommonCodecs.parseCalendarDay("2026-06-15T23:30:00.000-05:00") shouldEqual Right(day("2026-06-15"))
    CommonCodecs.parseCalendarDay("2026-06-15") shouldEqual Right(day("2026-06-15"))
    CommonCodecs.parseCalendarDay("not a date").isLeft shouldBe true
end HrCodecsTest
