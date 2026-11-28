package io.github.nicolasfara.rstmanager.planning.service

import scala.concurrent.duration.*

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.{ DateTime, LocalDate }
import org.joda.time.LocalTime
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * Keeps the plan anchored to the current business day without periodic recomputation.
 *
 *   - **Morning consolidation**: every day at `atHour:00` (company time, `Europe/Rome`), before the shift starts, a
 *     [[io.github.nicolasfara.rstmanager.planning.PlanningTrigger.DailyPlanning]] recalculation re-plans the remaining work from today onwards, with
 *     the full day's capacity. Work planned on a past day and not completed moves to the next production days (on Saturday, to Monday).
 *   - **Startup catch-up**: when the service starts with a plan computed on a previous day (it was down or suspended at the morning run), the plan is
 *     recomputed from today immediately.
 *   - **Retries**: a morning run reporting errors is retried a few times; the read guard on the board ([[PlanningFreshnessGuard]]) covers whatever
 *     still fails.
 *
 * The daily runs share one command id per day ([[PlanningFreshness.dailyCause]]), so they never produce more than one attempt per day. During the day
 * the plan changes only on real operational changes (see [[PlanningDependencyConsumer]]). The wait for the next run re-reads the wall clock at least
 * every [[maxSleepStep]], so a suspended host or a clock change never skips a day; it reads nothing else.
 */
object PlanningDailyScheduler:
  private val logger = Slf4jLogger.getLogger[IO]

  /** Company-time hour (0-23) at which the morning consolidation runs, before the shift begins. */
  val defaultRecalculationHour: Int = 5

  /** Longest single sleep while waiting for a run: the wall clock is re-read at least this often. */
  private[rstmanager] val maxSleepStep: FiniteDuration = 15.minutes

  /** Delays of the retries of a morning run that reported errors. */
  private val retryDelays: List[FiniteDuration] = List(5.minutes, 10.minutes, 20.minutes)

  /** Runs the startup catch-up, then the daily consolidation loop, as a background fiber for the lifetime of the resource. */
  def resource(recalculator: PlanningRecalculator, guard: PlanningFreshnessGuard, clock: PlanningClock, atHour: Int): Resource[IO, Unit] =
    (guard.ensureFresh("startup") >> loop(recalculator, clock, atHour)).background.void

  private def loop(recalculator: PlanningRecalculator, clock: PlanningClock, atHour: Int): IO[Unit] =
    (sleepUntilNextRun(clock, atHour, "daily planning consolidation") >> clock.today.flatMap(runDaily(recalculator, _, retryDelays))).foreverM

  /** Runs the consolidation of `day`, retrying after each delay of `retries` while it reports errors. Never fails. */
  private[service] def runDaily(recalculator: PlanningRecalculator, day: LocalDate, retries: List[FiniteDuration]): IO[Unit] =
    logger.info(s"Running daily planning consolidation for $day.") >>
      recalculator.recalculate(PlanningFreshness.dailyCause(day)).flatMap { result =>
        if result.errors.isEmpty then
          logger.info(s"Daily planning consolidation for $day completed (command ${result.commandId.getOrElse("no-op")}).")
        else
          val issues = result.errors.mkString("; ")
          retries match
            case delay :: rest =>
              logger.warn(s"Daily planning consolidation for $day reported issues, retrying in ${humanize(delay)}: $issues") >>
                IO.sleep(delay) >> runDaily(recalculator, day, rest)
            case Nil => logger.warn(s"Daily planning consolidation for $day reported issues, giving up until tomorrow: $issues")
      }

  /**
   * Waits until the next `atHour:00` of `clock` (company time), re-reading the wall clock at least every [[maxSleepStep]]. Shared with the daily
   * warehouse reconciliation.
   */
  private[rstmanager] def sleepUntilNextRun(clock: PlanningClock, atHour: Int, what: String): IO[Unit] =
    clock.now.flatMap { now =>
      val target = nextRunAt(now, atHour)
      logger.info(s"Next $what scheduled in ${humanize(durationBetween(now, target))} (at $atHour:00 company time).") >>
        sleepUntil(clock, target)
    }

  private def sleepUntil(clock: PlanningClock, target: DateTime): IO[Unit] =
    clock.now.flatMap { now =>
      if !now.isBefore(target) then IO.unit
      else IO.sleep(durationBetween(now, target).min(maxSleepStep)) >> sleepUntil(clock, target)
    }

  /**
   * The next occurrence of `atHour:00` wall-clock time in the zone of `now`, strictly in the future (a run exactly at `atHour:00` waits for the
   * following day). On a daylight-saving day the run stays at `atHour:00`; an hour skipped by the change falls back to the first instant after it.
   */
  private[rstmanager] def nextRunAt(now: DateTime, atHour: Int): DateTime =
    val todayRun = runOn(now.toLocalDate, atHour, now)
    if todayRun.isAfter(now) then todayRun else runOn(now.toLocalDate.plusDays(1), atHour, now)

  private def runOn(day: LocalDate, atHour: Int, reference: DateTime): DateTime =
    val zone = reference.getZone
    Either
      .catchNonFatal[DateTime](day.toLocalDateTime(new LocalTime(atHour, 0)).toDateTime(zone).nn)
      .getOrElse(day.toDateTimeAtStartOfDay(zone).plusHours(atHour).nn)

  /** Time until the next `atHour:00` of the zone of `now` (see [[nextRunAt]]). */
  private[rstmanager] def durationUntilNextRun(now: DateTime, atHour: Int): FiniteDuration =
    durationBetween(now, nextRunAt(now, atHour))

  private def durationBetween(from: DateTime, to: DateTime): FiniteDuration = math.max(0L, to.getMillis - from.getMillis).millis

  /** `duration` as hours and minutes, for the logs (`5h 7m`). */
  private[rstmanager] def humanize(duration: FiniteDuration): String =
    val totalMinutes = duration.toMinutes
    s"${totalMinutes / 60}h ${totalMinutes % 60}m"
end PlanningDailyScheduler
