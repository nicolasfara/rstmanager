package io.github.nicolasfara.rstmanager.customer.domain

import cats.Monad

/** Edomata service for the event-sourced [[CustomerAggregate]]. */
object CustomerService extends CustomerAggregate.Service[CustomerService.Command, CustomerService.Notification]:
  enum Command derives CanEqual:
    case Create(customer: Customer)
    case Update(customer: Customer)
    case Delete

  enum Notification derives CanEqual:
    /** The customer record was created, updated, or deleted. */
    case CustomerChanged(customerId: CustomerId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Create(customer) =>
      mustTargetOwnStream(customer.id) >> App.state.decide(_.create(customer)).void >> App.publish(Notification.CustomerChanged(customer.id))
    case Command.Update(customer) =>
      mustTargetOwnStream(customer.id) >> App.state.decide(_.update(customer)).void >> App.publish(Notification.CustomerChanged(customer.id))
    case Command.Delete =>
      App.state.decide(_.delete).flatMap {
        case CustomerAggregate.Deleted(customer) => App.publish(Notification.CustomerChanged(customer.id))
        case CustomerAggregate.Empty | CustomerAggregate.Active(_) => App.unit
      }
  }

  /** Rejects a command whose customer belongs to another stream, so the stream id and the stored customer id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](customerId: CustomerId): App[F, Unit] =
    App.aggregateId.flatMap(streamId => if streamId == customerId.toString then App.unit else App.reject(CustomerError.CustomerIdMismatch))
end CustomerService
