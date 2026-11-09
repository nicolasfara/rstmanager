package io.github.nicolasfara.rstmanager.hr.domain.calendar

import io.github.nicolasfara.rstmanager.hr.domain.calendar.CompanyCalendarAggregate.*

import cats.Monad

/** Edomata service for the singleton [[CompanyCalendarAggregate]]. */
object CompanyCalendarService extends CompanyCalendarAggregate.Service[CompanyCalendarService.Command, CompanyCalendarService.Notification]:
  enum Command derives CanEqual:
    case Add(closure: CompanyClosure)
    case Update(closure: CompanyClosure)
    case Remove(id: ClosureId)

  enum Notification derives CanEqual:
    /** A closure was added, changed, or removed, so the workforce capacity changed and dependent planning may need recomputation. */
    case CompanyCalendarChanged

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Add(closure) => App.state.decide(_.add(closure)).void >> App.publish(Notification.CompanyCalendarChanged)
    case Command.Update(closure) => App.state.decide(_.update(closure)).void >> App.publish(Notification.CompanyCalendarChanged)
    case Command.Remove(id) => App.state.decide(_.remove(id)).void >> App.publish(Notification.CompanyCalendarChanged)
  }
end CompanyCalendarService
