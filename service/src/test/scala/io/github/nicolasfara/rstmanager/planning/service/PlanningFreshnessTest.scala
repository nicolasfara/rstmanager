package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.{ Planning, PlanningRequest, PlanningRequestId, PlanningTrigger }
import io.github.nicolasfara.rstmanager.warehouse.domain.BusinessCalendar

import cats.effect.{ IO, Ref }
import cats.effect.unsafe.implicits.global
import com.github.nscala_time.time.Imports.{ DateTime, DateTimeZone, LocalDate }
import edomata.backend.eventsourcing.AggregateState
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class PlanningFreshnessTest extends AnyFlatSpecLike:
  private val friday = new LocalDate(2026, 11, 27)
  private val saturday = new LocalDate(2026, 11, 28)

  private def planFrom(startOn: DateTime): PlanningFreshness.State =
    val request = PlanningRequest(PlanningRequestId(UUID.randomUUID().nn), startOn, PlanningTrigger.DailyPlanning, startOn, Nil)
    AggregateState.Valid(Planning.InProgressPlanning(request, Nil, Nil, Nil, Nil, Nil), 2L)

  private def romeMidnight(day: LocalDate): DateTime = day.toDateTimeAtStartOfDay(BusinessCalendar.zone).nn

  "PlanningFreshness" should "consider a plan computed on Friday stale on Saturday" in:
    PlanningFreshness.isStale(planFrom(romeMidnight(friday)), saturday) shouldBe true

  it should "consider a plan computed today fresh" in:
    PlanningFreshness.isStale(planFrom(romeMidnight(saturday)), saturday) shouldBe false

  it should "consider a missing plan stale" in:
    PlanningFreshness.isStale(AggregateState.Valid(Planning.NewPlanning, 0L), saturday) shouldBe true

  it should "read a legacy plan anchored at UTC midnight as the same business day" in:
    val utcMidnight = saturday.toDateTimeAtStartOfDay(DateTimeZone.UTC).nn
    PlanningFreshness.isStale(planFrom(utcMidnight), saturday) shouldBe false

  it should "give the daily consolidation a command id stable for the day" in:
    val cause = PlanningFreshness.dailyCause(saturday)
    cause.trigger shouldEqual PlanningTrigger.DailyPlanning
    cause.commandId shouldEqual Some("planning-daily-2026-11-28")
    cause.requestId shouldEqual PlanningFreshness.dailyCause(saturday).requestId
    cause.requestId should not equal PlanningFreshness.dailyCause(friday).requestId

  "PlanningFreshnessGuard" should "recompute a stale plan once, with the daily cause of today" in:
    val seen = Ref.of[IO, List[PlanningRecalculationCause]](Nil).unsafeRunSync()
    val clock = PlanningClock.fixed(new DateTime(2026, 11, 28, 10, 0, BusinessCalendar.zone))
    val guard = PlanningFreshnessGuard(IO.pure(planFrom(romeMidnight(friday))), RecordingRecalculator(seen), clock)

    guard.ensureFresh("test").unsafeRunSync()

    seen.get.unsafeRunSync() shouldEqual List(PlanningFreshness.dailyCause(saturday))

  it should "leave a plan anchored to today untouched" in:
    val seen = Ref.of[IO, List[PlanningRecalculationCause]](Nil).unsafeRunSync()
    val clock = PlanningClock.fixed(new DateTime(2026, 11, 28, 10, 0, BusinessCalendar.zone))
    val guard = PlanningFreshnessGuard(IO.pure(planFrom(romeMidnight(saturday))), RecordingRecalculator(seen), clock)

    guard.ensureFresh("test").unsafeRunSync()

    seen.get.unsafeRunSync() shouldBe empty

  it should "never fail, even when reading the plan fails" in:
    val seen = Ref.of[IO, List[PlanningRecalculationCause]](Nil).unsafeRunSync()
    val guard = PlanningFreshnessGuard(IO.raiseError(new RuntimeException("db down")), RecordingRecalculator(seen), PlanningClock.business)

    noException should be thrownBy guard.ensureFresh("test").unsafeRunSync()
    seen.get.unsafeRunSync() shouldBe empty

  private final class RecordingRecalculator(seen: Ref[IO, List[PlanningRecalculationCause]]) extends PlanningRecalculator:
    override def recalculate(trigger: PlanningTrigger): IO[PlanningRecalculationResult] =
      recalculate(PlanningRecalculationCause.manual(trigger))
    override def recalculate(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult] =
      seen.update(_ :+ cause).as(PlanningRecalculationResult.skipped)
end PlanningFreshnessTest
