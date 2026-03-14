package io.github.nicolasfara.rstmanager.hr.domain

import cats.data.ValidatedNec
import com.github.nscala_time.time.Imports.*
import io.github.iltotore.iron.cats.*
import monocle.syntax.all.*

final case class BudgetHours(default: WeeklyHours, overrides: List[HoursOverride]):
  given CanEqual[DateTime, DateTime] = CanEqual.derived
  private val zeroDailyHours: DailyHours = DailyHours.applyUnsafe(0)

  def getWorkingHoursForDay(day: DateTime): DailyHours =
    overrides.collectFirst {
      case WorkingDayOverride(hours, _, dayOfWeek) if dayOfWeek == day => hours
      case VacationOverride(interval) if interval.contains(day) => zeroDailyHours
    }.getOrElse {
      DailyHours.validatedNec(default.value / 5).fold(_ => zeroDailyHours, identity)
    }

  def updateDefault(default: WeeklyHours): BudgetHours = this.focus(_.default).replace(default)

  def addOverride(hoursOverride: HoursOverride): BudgetHours = this.focus(_.overrides).modify(_ :+ hoursOverride)

  def setOverride(hoursOverride: HoursOverride): BudgetHours =
    hoursOverride match
      case WorkingDayOverride(_, _, newDay) =>
        this.focus(_.overrides).modify(_.filter {
          case WorkingDayOverride(_, _, day) => day != newDay
          case _ => true
        } :+ hoursOverride)
      case _ =>
        addOverride(hoursOverride)

  def hasConflict(hoursOverride: HoursOverride): Boolean =
    hoursOverride match
      case VacationOverride(interval) =>
        overrides.exists {
          case VacationOverride(existingInterval) => existingInterval.overlaps(interval)
          case WorkingDayOverride(_, _, day) => interval.contains(day)
        }
      case WorkingDayOverride(_, _, day) =>
        overrides.exists {
          case VacationOverride(interval) => interval.contains(day)
          case _ => false
        }

object BudgetHours:
  def createBudgetHours(default: Int, overrides: List[HoursOverride]): ValidatedNec[String, BudgetHours] =
    WeeklyHours.validatedNec(default).map(BudgetHours(_, overrides))
