package io.github.nicolasfara.rstmanager.warehouse.service

import io.github.nicolasfara.rstmanager.planning.service.PlanningDailyScheduler

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * The daily consolidation of the warehouse consumptions: [[WarehouseConsumptionReconciler.reconcileAll]] once at startup, then once a day at a fixed
 * local hour.
 *
 * During the day the consumptions follow the order notifications (see
 * [[io.github.nicolasfara.rstmanager.planning.service.PlanningDependencyConsumer]]); the daily run only heals what a failed step left behind. The
 * startup run catches up with orders completed while the reconciliation did not exist or was failing. There is no other time-based run.
 */
object WarehouseDailyReconciler:
  private val logger = Slf4jLogger.getLogger[IO]

  /** Local-time hour (0-23) of the daily run: before the planning consolidation, so the day opens with the warehouse in line with the orders. */
  val defaultReconcileHour: Int = 4

  /** Runs the startup reconciliation and then the daily loop as a background fiber for the lifetime of the resource. */
  def resource(reconciler: WarehouseConsumptionReconciler, atHour: Int): Resource[IO, Unit] =
    (run(reconciler, "startup") >> loop(reconciler, atHour)).background.void

  private def loop(reconciler: WarehouseConsumptionReconciler, atHour: Int): IO[Unit] =
    (sleepUntilNextRun(atHour) >> run(reconciler, "daily")).foreverM

  private def sleepUntilNextRun(atHour: Int): IO[Unit] =
    IO(DateTime.now()).flatMap { now =>
      val wait = PlanningDailyScheduler.durationUntilNextRun(now, atHour)
      logger.info(s"Next daily warehouse consumption reconciliation in ${PlanningDailyScheduler.humanize(wait)} (at $atHour:00 local time).") >>
        IO.sleep(wait)
    }

  /** The reconciliation never fails; the guard only keeps the loop alive should an unexpected error escape. */
  private def run(reconciler: WarehouseConsumptionReconciler, label: String): IO[Unit] =
    logger.info(s"Running $label warehouse consumption reconciliation.") >>
      reconciler.reconcileAll.void.handleErrorWith(error => logger.error(error)(s"The $label warehouse consumption reconciliation failed."))
end WarehouseDailyReconciler
