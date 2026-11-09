package io.github.nicolasfara.rstmanager.hr.domain.calendar

import java.util.UUID

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class CompanyClosureTest extends AnyFlatSpecLike:
  private val id: ClosureId = ClosureId(UUID.fromString("00000000-0000-0000-0000-0000000000c1").nn)
  private val first: LocalDate = LocalDate.parse("2026-08-10")
  private val last: LocalDate = LocalDate.parse("2026-08-21")

  private def validOrFail[A](value: ValidatedNec[String, A]): A =
    value.fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def errors[A](value: ValidatedNec[String, A]): List[String] =
    value.fold(_.toChain.toList, valid => fail(s"Unexpectedly valid: $valid"))

  "A closure period" should "cover every day between its bounds, both included" in:
    val closure = validOrFail(CompanyClosure.createPeriod(id, first, last, "Chiusura estiva"))

    closure.covers(first.minusDays(1)) shouldBe false
    closure.covers(first) shouldBe true
    closure.covers(first.plusDays(5)) shouldBe true
    closure.covers(last) shouldBe true
    closure.covers(last.plusDays(1)) shouldBe false

  it should "report its last day as the end of the span covering a day" in:
    val closure = validOrFail(CompanyClosure.createPeriod(id, first, last, "Chiusura estiva"))

    closure.lastDayCovering(first.plusDays(2)) shouldEqual Some(last)
    closure.lastDayCovering(last.plusDays(1)) shouldBe None

  it should "accept a single day and reject an end before the start" in:
    CompanyClosure.createPeriod(id, first, first, "Ponte").isValid shouldBe true
    errors(CompanyClosure.createPeriod(id, first, first.minusDays(1), "Ponte")) shouldEqual
      List("The closure end date must be on or after the start date")

  it should "require a reason, accumulating it with the other errors" in:
    errors(CompanyClosure.createPeriod(id, first, first.minusDays(1), "")) should have size 2

  "An annual closure" should "cover the same day and month in every year" in:
    val christmas = validOrFail(CompanyClosure.createAnnual(id, 12, 25, "Natale"))

    christmas.covers(LocalDate.parse("2026-12-25")) shouldBe true
    christmas.covers(LocalDate.parse("2031-12-25")) shouldBe true
    christmas.covers(LocalDate.parse("2026-12-24")) shouldBe false
    christmas.covers(LocalDate.parse("2026-11-25")) shouldBe false
    christmas.lastDayCovering(LocalDate.parse("2027-12-25")) shouldEqual Some(LocalDate.parse("2027-12-25"))

  it should "close 29 February only in leap years" in:
    val leapDay = validOrFail(CompanyClosure.createAnnual(id, 2, 29, "Giorno bisestile"))

    leapDay.covers(LocalDate.parse("2028-02-29")) shouldBe true
    leapDay.covers(LocalDate.parse("2027-02-28")) shouldBe false
    leapDay.covers(LocalDate.parse("2027-03-01")) shouldBe false

  it should "reject days that do not exist in the month, and out-of-range months and days" in:
    CompanyClosure.createAnnual(id, 4, 31, "Aprile").isValid shouldBe false
    CompanyClosure.createAnnual(id, 2, 30, "Febbraio").isValid shouldBe false
    CompanyClosure.createAnnual(id, 13, 1, "Mese").isValid shouldBe false
    CompanyClosure.createAnnual(id, 1, 0, "Giorno").isValid shouldBe false
    errors(CompanyClosure.createAnnual(id, 0, 32, "")) should have size 3

  "Closures" should "clash when two periods share a day or two annual closures share day and month" in:
    val august = validOrFail(CompanyClosure.createPeriod(id, first, last, "Estate"))
    val touching = validOrFail(CompanyClosure.createPeriod(id, last, last.plusDays(3), "Estate bis"))
    val after = validOrFail(CompanyClosure.createPeriod(id, last.plusDays(1), last.plusDays(3), "Dopo"))
    val ferragosto = validOrFail(CompanyClosure.createAnnual(id, 8, 15, "Ferragosto"))
    val ferragostoAgain = validOrFail(CompanyClosure.createAnnual(id, 8, 15, "Assunzione"))

    august.clashesWith(touching) shouldBe true
    august.clashesWith(after) shouldBe false
    ferragosto.clashesWith(ferragostoAgain) shouldBe true
    august.clashesWith(ferragosto) shouldBe false
end CompanyClosureTest
