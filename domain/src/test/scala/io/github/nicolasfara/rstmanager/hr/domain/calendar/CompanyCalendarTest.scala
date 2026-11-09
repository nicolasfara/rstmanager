package io.github.nicolasfara.rstmanager.hr.domain.calendar

import java.util.UUID

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class CompanyCalendarTest extends AnyFlatSpecLike:
  private val monday: LocalDate = LocalDate.parse("2026-08-10")

  private def closureId(suffix: Int): ClosureId = ClosureId(UUID.fromString(f"00000000-0000-0000-0000-$suffix%012d").nn)

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def period(id: Int, first: LocalDate, last: LocalDate): CompanyClosure =
    validOrFail(CompanyClosure.createPeriod(closureId(id), first, last, "Chiusura"))

  private def annual(id: Int, month: Int, day: Int): CompanyClosure =
    validOrFail(CompanyClosure.createAnnual(closureId(id), month, day, "Festa"))

  private def invariantErrors(calendar: CompanyCalendar): List[CompanyCalendarError] =
    calendar.checkInvariants.fold(_.toChain.toList, _ => Nil)

  "The empty calendar" should "make Monday to Friday working days and weekends days off" in:
    (0 to 4).map(offset => CompanyCalendar.empty.isWorkingDay(monday.plusDays(offset))) shouldBe Vector.fill(5)(true)
    CompanyCalendar.empty.isWorkingDay(monday.plusDays(5)) shouldBe false
    CompanyCalendar.empty.isWorkingDay(monday.plusDays(6)) shouldBe false

  "A company calendar" should "close the weekdays covered by a closure" in:
    val ferragosto = annual(1, 8, 12)
    val calendar = CompanyCalendar(List(period(2, monday, monday.plusDays(1)), ferragosto))

    calendar.isWorkingDay(monday) shouldBe false
    calendar.isWorkingDay(monday.plusDays(1)) shouldBe false
    calendar.isWorkingDay(monday.plusDays(2)) shouldBe false
    calendar.closureOn(monday.plusDays(2)) shouldEqual Some(ferragosto)
    calendar.isWorkingDay(monday.plusDays(3)) shouldBe true
    calendar.closureOn(monday.plusDays(3)) shouldBe None

  it should "report the last closed day of the span covering a day" in:
    val calendar = CompanyCalendar(List(period(1, monday, monday.plusDays(4)), annual(2, 8, 12)))

    calendar.closedUntil(monday.plusDays(2)) shouldEqual Some(monday.plusDays(4))
    calendar.closedUntil(monday.plusDays(5)) shouldBe None

  it should "upsert and remove closures by id" in:
    val original = period(1, monday, monday)
    val moved = period(1, monday.plusDays(7), monday.plusDays(7))
    val calendar = CompanyCalendar.empty.upsert(original)

    calendar.upsert(moved).closures shouldEqual List(moved)
    calendar.without(closureId(1)) shouldEqual CompanyCalendar.empty
    calendar.without(closureId(2)) shouldEqual calendar

  it should "satisfy its invariants with disjoint periods and an annual day inside a period" in:
    val calendar = CompanyCalendar(List(period(1, monday, monday.plusDays(4)), period(2, monday.plusDays(5), monday.plusDays(6)), annual(3, 8, 12)))

    invariantErrors(calendar) shouldBe empty

  it should "reject overlapping periods and duplicate annual closures" in:
    invariantErrors(CompanyCalendar(List(period(1, monday, monday.plusDays(4)), period(2, monday.plusDays(4), monday.plusDays(6))))) shouldEqual
      List(CompanyCalendarError.OverlappingClosures)
    invariantErrors(CompanyCalendar(List(annual(1, 12, 25), annual(2, 12, 25)))) shouldEqual List(CompanyCalendarError.OverlappingClosures)

  it should "reject two closures with the same id" in:
    invariantErrors(CompanyCalendar(List(annual(1, 12, 25), annual(1, 12, 26)))) shouldEqual List(CompanyCalendarError.ClosureAlreadyExists)
end CompanyCalendarTest
