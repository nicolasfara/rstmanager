package io.github.nicolasfara.rstmanager.planning.service

import scala.concurrent.duration.*

import com.github.nscala_time.time.Imports.DateTime
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningDailySchedulerTest extends AnyFlatSpecLike:
  private val recalcHour = PlanningDailyScheduler.defaultRecalculationHour // 05:00

  "PlanningDailyScheduler" should "wait until the same-day run when the current time is before it" in:
    val now = DateTime.parse("2026-06-15T03:00:00").nn
    PlanningDailyScheduler.durationUntilNextRun(now, recalcHour) shouldEqual 2.hours

  it should "wait for the next day when the current time is past the run hour" in:
    val now = DateTime.parse("2026-06-15T06:30:00").nn
    PlanningDailyScheduler.durationUntilNextRun(now, recalcHour) shouldEqual (22.hours + 30.minutes)

  it should "wait a full day when invoked exactly at the run hour, so it never fires twice in one morning" in:
    val now = DateTime.parse("2026-06-15T05:00:00").nn
    PlanningDailyScheduler.durationUntilNextRun(now, recalcHour) shouldEqual 24.hours

  it should "honour a custom run hour" in:
    val now = DateTime.parse("2026-06-15T00:00:00").nn
    PlanningDailyScheduler.durationUntilNextRun(now, atHour = 8) shouldEqual 8.hours
end PlanningDailySchedulerTest
