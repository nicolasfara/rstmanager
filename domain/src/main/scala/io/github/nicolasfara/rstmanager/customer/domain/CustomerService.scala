package io.github.nicolasfara.rstmanager.customer.domain

import cats.Monad

/** Edomata service for the event-sourced [[CustomerAggregate]]. */
object CustomerService extends CustomerAggregate.Service[CustomerService.Command, CustomerService.Notification]:
  enum Command derives CanEqual:
    case Create(customer: Customer)
    case Update(customer: Customer)
    case Delete

  enum Notification derives CanEqual:
    /** The customer record changed. */
    case CustomerChanged(customerId: CustomerId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(customer) =>
      App.state.decide(_.create(customer)).void >> App.publish(Notification.CustomerChanged(customer.id))
    case Command.Update(customer) =>
      App.state.decide(_.update(customer)).void >> App.publish(Notification.CustomerChanged(customer.id))
    case Command.Delete =>
      App.state.decide(_.delete).void
  }
end CustomerService
