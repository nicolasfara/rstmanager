package io.github.nicolasfara.rstmanager.planning.service

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.planning.PlanningTrigger
import io.github.nicolasfara.rstmanager.warehouse.domain.BusinessCalendar

import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningDailySchedulerTest extends AnyFlatSpecLike:
  private val recalcHour = PlanningDailyScheduler.defaultRecalculationHour // 05:00

  "PlanningDailyScheduler" should "wait until the same-day run when the current time is before it" in:
    val now = DateTime.parse("2026-06-15T03:00:00")
    PlanningDailyScheduler.durationUntilNextRun(now, recalcHour) shouldEqual 2.hours

  it should "wait for the next day when the current time is past the run hour" in:
    val now = DateTime.parse("2026-06-15T06:30:00")
    PlanningDailyScheduler.durationUntilNextRun(now, recalcHour) shouldEqual (22.hours + 30.minutes)

  it should "wait a full day when invoked exactly at the run hour, so it never fires twice in one morning" in:
    val now = DateTime.parse("2026-06-15T05:00:00")
    PlanningDailyScheduler.durationUntilNextRun(now, recalcHour) shouldEqual 24.hours

  it should "honour a custom run hour" in:
    val now = DateTime.parse("2026-06-15T00:00:00")
    PlanningDailyScheduler.durationUntilNextRun(now, atHour = 8) shouldEqual 8.hours
  it should "run at the company hour in Europe/Rome, whatever the zone of the current instant" in:
    val nowInUtc = DateTime.parse("2026-11-28T03:30:00Z") // 04:30 in Rome
    val romeNow = nowInUtc.withZone(BusinessCalendar.zone).nn
    PlanningDailyScheduler.durationUntilNextRun(romeNow, recalcHour) shouldEqual 30.minutes

  it should "keep the run at 05:00 wall-clock time across the spring daylight-saving change" in:
    val saturdayMorning = new DateTime(2027, 3, 27, 6, 0, BusinessCalendar.zone) // CET; Sunday 28 is CEST
    val next = PlanningDailyScheduler.nextRunAt(saturdayMorning, recalcHour)
    next.getHourOfDay shouldEqual 5
    next.toLocalDate shouldEqual new LocalDate(2027, 3, 28)
    PlanningDailyScheduler.durationUntilNextRun(saturdayMorning, recalcHour) shouldEqual 22.hours

  it should "fall back to the first valid instant when the run hour is skipped by daylight saving" in:
    val saturdayMorning = new DateTime(2027, 3, 27, 6, 0, BusinessCalendar.zone)
    PlanningDailyScheduler.nextRunAt(saturdayMorning, atHour = 2).toLocalDate shouldEqual new LocalDate(2027, 3, 28)

  it should "retry a daily run that reports errors with the same daily cause, then stop" in:
    val day = new LocalDate(2026, 11, 28)
    val seen = Ref.of[IO, List[PlanningRecalculationCause]](Nil).unsafeRunSync()
    val failing = new PlanningRecalculator:
      override def recalculate(trigger: PlanningTrigger): IO[PlanningRecalculationResult] =
        recalculate(PlanningRecalculationCause.manual(trigger))
      override def recalculate(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult] =
        seen.update(_ :+ cause).as(PlanningRecalculationResult(None, List("boom")))

    PlanningDailyScheduler.runDaily(failing, day, List(1.milli, 1.milli)).unsafeRunSync()

    seen.get.unsafeRunSync() shouldEqual List.fill(3)(PlanningFreshness.dailyCause(day))

  it should "not retry a daily run that succeeds" in:
    val day = new LocalDate(2026, 11, 28)
    val seen = Ref.of[IO, Int](0).unsafeRunSync()
    val ok = new PlanningRecalculator:
      override def recalculate(trigger: PlanningTrigger): IO[PlanningRecalculationResult] =
        recalculate(PlanningRecalculationCause.manual(trigger))
      override def recalculate(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult] =
        seen.update(_ + 1).as(PlanningRecalculationResult(cause.commandId, Nil))

    PlanningDailyScheduler.runDaily(ok, day, List(1.milli)).unsafeRunSync()

    seen.get.unsafeRunSync() shouldEqual 1
end PlanningDailySchedulerTest
