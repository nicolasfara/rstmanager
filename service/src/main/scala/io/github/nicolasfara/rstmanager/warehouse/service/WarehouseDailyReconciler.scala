package io.github.nicolasfara.rstmanager.warehouse.service

import io.github.nicolasfara.rstmanager.planning.service.PlanningDailyScheduler

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.DateTime
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * The daily consolidation of the warehouse: [[WarehouseConsumptionReconciler.reconcileAll]] and [[WarehouseDeliveryNoteReconciler.reconcileAll]] once
 * at startup, then once a day at a fixed local hour.
 *
 * During the day the consumptions follow the order notifications (see
 * [[io.github.nicolasfara.rstmanager.planning.service.PlanningDependencyConsumer]]) and the delivery note purchases follow the note commands (see
 * [[DeliveryNoteHttpApi]]); the daily run only heals what a failed step left behind. The startup run catches up with orders completed and notes
 * changed while the reconciliation did not exist or was failing. There is no other time-based run.
 */
object WarehouseDailyReconciler:
  private val logger = Slf4jLogger.getLogger[IO]

  /** Local-time hour (0-23) of the daily run: before the planning consolidation, so the day opens with the warehouse in line with the orders. */
  val defaultReconcileHour: Int = 4

  /** Runs the startup reconciliation and then the daily loop as a background fiber for the lifetime of the resource. */
  def resource(consumptions: WarehouseConsumptionReconciler, deliveryNotes: WarehouseDeliveryNoteReconciler, atHour: Int): Resource[IO, Unit] =
    (run(consumptions, deliveryNotes, "startup") >> loop(consumptions, deliveryNotes, atHour)).background.void

  private def loop(consumptions: WarehouseConsumptionReconciler, deliveryNotes: WarehouseDeliveryNoteReconciler, atHour: Int): IO[Unit] =
    (sleepUntilNextRun(atHour) >> run(consumptions, deliveryNotes, "daily")).foreverM

  private def sleepUntilNextRun(atHour: Int): IO[Unit] =
    IO(DateTime.now()).flatMap { now =>
      val wait = PlanningDailyScheduler.durationUntilNextRun(now, atHour)
      logger.info(s"Next daily warehouse reconciliation in ${PlanningDailyScheduler.humanize(wait)} (at $atHour:00 local time).") >>
        IO.sleep(wait)
    }

  /** The reconciliations never fail; the guards only keep the loop alive should an unexpected error escape. */
  private def run(consumptions: WarehouseConsumptionReconciler, deliveryNotes: WarehouseDeliveryNoteReconciler, label: String): IO[Unit] =
    logger.info(s"Running $label warehouse consumption reconciliation.") >>
      consumptions.reconcileAll.void.handleErrorWith(error => logger.error(error)(s"The $label warehouse consumption reconciliation failed.")) >>
      logger.info(s"Running $label warehouse delivery note reconciliation.") >>
      deliveryNotes.reconcileAll.void.handleErrorWith(error => logger.error(error)(s"The $label warehouse delivery note reconciliation failed."))
end WarehouseDailyReconciler
