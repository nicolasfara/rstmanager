package io.github.nicolasfara.rstmanager.planning.service

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.planning.PlanningTrigger

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import org.slf4j.LoggerFactory

/**
 * Fires a single [[PlanningTrigger.DailyPlanning]] recalculation early each morning, before the shift starts, so operators open a plan re-anchored to
 * the current day with the full day's capacity available.
 *
 * This is the only *time-based* recalculation in the system. During the day the plan stays consolidated: it is recomputed only when a real operational
 * change (a task completed early, task hours added or removed, order or workforce edits) emits a recalculation notification, which the planner uses to
 * fill or push work so production never stalls. See [[PlanningDependencyConsumer]]. There is deliberately no hourly or otherwise periodic recompute.
 */
object PlanningDailyScheduler:
  private val logger = LoggerFactory.getLogger(getClass).nn

  /** Local-time hour (0-23) at which the morning consolidation runs, before the shift begins. */
  val defaultRecalculationHour: Int = 5

  /** Runs the daily consolidation loop as a background fiber for the lifetime of the resource. */
  def resource(recalculator: PlanningRecalculator, atHour: Int = defaultRecalculationHour): Resource[IO, Unit] =
    loop(recalculator, atHour).background.void

  private def loop(recalculator: PlanningRecalculator, atHour: Int): IO[Unit] =
    (sleepUntilNextRun(atHour) >> runDailyRecalculation(recalculator)).foreverM

  private def sleepUntilNextRun(atHour: Int): IO[Unit] =
    IO(DateTime.now().nn).flatMap { now =>
      val wait = durationUntilNextRun(now, atHour)
      IO(logger.info(s"Next daily planning consolidation scheduled in ${humanize(wait)} (at $atHour:00 local time).")) >> IO.sleep(wait)
    }

  private def runDailyRecalculation(recalculator: PlanningRecalculator): IO[Unit] =
    IO(logger.info("Running daily planning consolidation (DailyPlanning trigger).")) >>
      recalculator.recalculate(PlanningTrigger.DailyPlanning).flatMap { result =>
        if result.errors.isEmpty then
          IO(logger.info(s"Daily planning consolidation completed (command ${result.commandId.getOrElse("no-op")})."))
        else IO(logger.warn(s"Daily planning consolidation reported issues: ${result.errors.mkString("; ")}"))
      }

  /** Time until the next occurrence of `atHour:00` local time, strictly in the future (a run exactly at `atHour:00` waits for the following day). */
  private[service] def durationUntilNextRun(now: DateTime, atHour: Int): FiniteDuration =
    val todayRun = now.withTimeAtStartOfDay().nn.plusHours(atHour).nn
    val nextRun = if todayRun.isAfter(now) then todayRun else todayRun.plusDays(1).nn
    (nextRun.getMillis - now.getMillis).millis

  private def humanize(duration: FiniteDuration): String =
    val totalMinutes = duration.toMinutes
    s"${totalMinutes / 60}h ${totalMinutes % 60}m"
end PlanningDailyScheduler
