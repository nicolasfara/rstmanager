package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.LocalDate
import io.github.iltotore.iron.cats.*
import monocle.syntax.all.*

/**
 * Budgeted availability for an employee.
 *
 * The budget is defined by a weekly default and optional day or vacation overrides, all keyed by calendar day.
 *
 * @param default
 *   Default weekly working hours.
 * @param overrides
 *   Exceptions applied on top of the weekly default.
 */
final case class BudgetHours(default: WeeklyHours, overrides: List[HoursOverride]):
  /**
   * Returns the working hours available on a specific calendar day.
   *
   * A working-day override for that day takes precedence over a vacation covering it, which takes precedence over the derived default.
   */
  def getWorkingHoursForDay(day: LocalDate): DailyHours =
    val dayOverride = overrides.collectFirst { case WorkingDayOverride(hours, _, overrideDay) if overrideDay.isEqual(day) => hours }
    def onVacation = overrides.exists {
      case vacation: VacationOverride => vacation.covers(day)
      case _: WorkingDayOverride => false
    }
    dayOverride.getOrElse(if onVacation then DailyHours(0) else defaultDailyHours)

  /**
   * Daily hours derived from the weekly default over a five-day week, capped at the 24 hours a day can hold.
   *
   * This is the single definition shared with the scheduler: a weekly default above 120 hours yields 24, never an out-of-range value.
   */
  def defaultDailyHours: DailyHours = DailyHours.applyUnsafe(math.min(default.value / 5, 24))

  /** Returns a copy with a different weekly default. */
  def updateDefault(default: WeeklyHours): BudgetHours = this.focus(_.default).replace(default)

  /** Appends a new override to the budget. */
  def addOverride(hoursOverride: HoursOverride): BudgetHours = this.focus(_.overrides).modify(_ :+ hoursOverride)

  /** Inserts or replaces an override depending on its type. */
  def setOverride(hoursOverride: HoursOverride): BudgetHours =
    hoursOverride match
      case WorkingDayOverride(_, _, newDay) =>
        this
          .focus(_.overrides)
          .modify(_.filter {
            case WorkingDayOverride(_, _, day) => !day.isEqual(newDay)
            case _: VacationOverride => true
          } :+ hoursOverride)
      case _: VacationOverride =>
        addOverride(hoursOverride)

  /** Checks whether a new override conflicts with existing overrides (overlapping vacations, or a working day inside a vacation). */
  def hasConflict(hoursOverride: HoursOverride): Boolean =
    hoursOverride match
      case vacation: VacationOverride =>
        overrides.exists {
          case existing: VacationOverride => existing.overlaps(vacation)
          case WorkingDayOverride(_, _, day) => vacation.covers(day)
        }
      case WorkingDayOverride(_, _, day) =>
        overrides.exists {
          case vacation: VacationOverride => vacation.covers(day)
          case _: WorkingDayOverride => false
        }

  /** Whether two of the overrides contradict each other: overlapping vacations, a working day inside a vacation, or two overrides for one day. */
  def hasConflictingOverrides: Boolean =
    overrides.tails.exists {
      case head :: rest => rest.exists(BudgetHours.contradict(head, _))
      case Nil => false
    }
end BudgetHours

object BudgetHours:
  private def contradict(first: HoursOverride, second: HoursOverride): Boolean = (first, second) match
    case (firstVacation: VacationOverride, secondVacation: VacationOverride) => firstVacation.overlaps(secondVacation)
    case (vacation: VacationOverride, WorkingDayOverride(_, _, day)) => vacation.covers(day)
    case (WorkingDayOverride(_, _, day), vacation: VacationOverride) => vacation.covers(day)
    case (WorkingDayOverride(_, _, firstDay), WorkingDayOverride(_, _, secondDay)) => firstDay.isEqual(secondDay)

  /** Creates `BudgetHours` from a raw weekly value and a list of overrides. */
  def createBudgetHours(default: Int, overrides: List[HoursOverride]): ValidatedNec[String, BudgetHours] =
    WeeklyHours.validatedNec(default).map(BudgetHours(_, overrides))
