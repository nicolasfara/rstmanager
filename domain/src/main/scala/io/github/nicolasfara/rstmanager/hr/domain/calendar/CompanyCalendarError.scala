package io.github.nicolasfara.rstmanager.hr.domain.calendar

/** Errors raised while deciding changes to the [[CompanyCalendar]]. */
enum CompanyCalendarError derives CanEqual:
  /** An update or removal addressed a closure id that is not in the calendar. */
  case ClosureNotFound

  /** An addition used a closure id that is already in the calendar, or the calendar would hold two closures with the same id. */
  case ClosureAlreadyExists

  /** The calendar would hold two periods sharing at least one day, or two annual closures on the same day and month. */
  case OverlappingClosures
