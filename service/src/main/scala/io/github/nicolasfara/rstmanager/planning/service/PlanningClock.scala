package io.github.nicolasfara.rstmanager.planning.service

import java.nio.charset.StandardCharsets
import java.util.UUID

import io.github.nicolasfara.rstmanager.planning.{ Planning, PlanningError, PlanningTrigger }
import io.github.nicolasfara.rstmanager.planning.events.PlanningEvent
import io.github.nicolasfara.rstmanager.warehouse.domain.BusinessCalendar

import cats.effect.IO
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import edomata.backend.eventsourcing.AggregateState
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Wall clock of the planning service, in the company's time zone (`Europe/Rome`) whatever the zone of the server: production days start at the
 * Italian midnight, the 05:00 consolidation is Italian time, and the remaining hours of today follow the Italian 09:00-18:00 workday.
 */
trait PlanningClock:
  def now: IO[DateTime]

  /** The business day the clock is on. */
  def today: IO[LocalDate] = now.map(BusinessCalendar.day)

object PlanningClock:
  /** The system clock, expressed in the company's time zone. */
  val business: PlanningClock = new PlanningClock:
    override def now: IO[DateTime] = IO(DateTime.now(BusinessCalendar.zone))

  /** A clock stopped at `instant`, for tests. */
  def fixed(instant: DateTime): PlanningClock = new PlanningClock:
    override def now: IO[DateTime] = IO.pure(instant)

/** Whether the persisted plan is still anchored to today. */
object PlanningFreshness:
  type State = AggregateState[Planning, PlanningEvent, PlanningError]

  /**
   * A plan is stale when it was computed from a business day before `today` (it may still assign work to past days), or when no plan exists yet. A
   * conflicted stream is never stale: recomputing would not heal it.
   */
  def isStale(state: State, today: LocalDate): Boolean = state match
    case AggregateState.Valid(planning, _) => planning.plannedFrom.forall(day => BusinessCalendar.day(day).isBefore(today))
    case _ => false

  /**
   * The cause of the daily consolidation of `day`. Its command id is stable for the day, so the morning run, the startup catch-up, and the read guard
   * produce at most one `DailyPlanning` attempt per day even when they race: edomata skips a command whose id it already handled.
   */
  def dailyCause(day: LocalDate): PlanningRecalculationCause =
    val stableId = s"planning-daily-$day"
    PlanningRecalculationCause(
      PlanningTrigger.DailyPlanning,
      Some(stableId),
      Some(UUID.nameUUIDFromBytes(stableId.getBytes(StandardCharsets.UTF_8)).nn),
    )
end PlanningFreshness

/**
 * Re-anchors the plan to today when it is stale: run at startup and before serving the board, so a plan computed on a previous day (the service was
 * down at the morning run, or the run failed) is never shown.
 */
final class PlanningFreshnessGuard(current: IO[PlanningFreshness.State], recalculator: PlanningRecalculator, clock: PlanningClock):
  private val logger = Slf4jLogger.getLogger[IO]

  /** Recomputes the plan from today when it is stale; never fails (problems are logged). */
  def ensureFresh(reason: String): IO[Unit] =
    val check =
      for
        today <- clock.today
        state <- current
        _ <-
          if !PlanningFreshness.isStale(state, today) then IO.unit
          else
            logger.info(s"Planning is not anchored to $today ($reason): recomputing it from today.") >>
              recalculator.recalculate(PlanningFreshness.dailyCause(today)).flatMap { result =>
                if result.errors.isEmpty then IO.unit
                else logger.warn(s"Planning catch-up for $today ($reason) reported issues: ${result.errors.mkString("; ")}")
              }
      yield ()
    check.handleErrorWith(error => logger.error(error)(s"Planning freshness check ($reason) failed."))
end PlanningFreshnessGuard
