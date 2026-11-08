package io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote

import io.github.nicolasfara.rstmanager.warehouse.domain.BusinessCalendar
import io.github.nicolasfara.rstmanager.warehouse.domain.deliverynote.events.DeliveryNoteEvent

import cats.Monad
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.Decision
import io.github.iltotore.iron.:|

/** Edomata service for the event-sourced [[DeliveryNoteAggregate]]. */
object DeliveryNoteService extends DeliveryNoteAggregate.Service[DeliveryNoteService.Command, DeliveryNoteService.Notification]:
  enum Command derives CanEqual:
    case Register(info: DeliveryNoteInfo)
    case Change(info: DeliveryNoteInfo)
    case Cancel(reason: String :| CancellationReason)

  enum Notification derives CanEqual:
    /** The delivery note was registered, changed, or cancelled: the purchases it brings to the materials must be reconciled. */
    case DeliveryNoteChanged(deliveryNoteId: DeliveryNoteId)

  def apply[F[_]: Monad]: App[F, Unit] = App.router {
    case Command.Register(info) =>
      mustTargetOwnStream(info.id) >> decideOnDay(_.register(info, _)).void >> App.publish(Notification.DeliveryNoteChanged(info.id))
    case Command.Change(info) =>
      mustTargetOwnStream(info.id) >> App.state.flatMap(before =>
        decideOnDay(_.change(info, _)).flatMap(after => if after == before then App.unit else App.publish(Notification.DeliveryNoteChanged(info.id))),
      )
    case Command.Cancel(reason) =>
      decideOnDay(_.cancel(reason, _)).flatMap(_.record.fold(App.unit)(note => App.publish(Notification.DeliveryNoteChanged(note.info.id))))
  }

  /** Decides on the current state for the business day of the command time, so events never read the clock. */
  private def decideOnDay[F[_]: Monad](
      decision: (DeliveryNoteAggregate, LocalDate) => Decision[DeliveryNoteError, DeliveryNoteEvent, DeliveryNoteAggregate],
  ): App[F, DeliveryNoteAggregate] =
    App.read.flatMap(context => App.decide(decision(context.state, BusinessCalendar.day(context.command.time))))

  /** Rejects a command whose note belongs to another stream, so the stream id and the stored note id always agree. */
  private def mustTargetOwnStream[F[_]: Monad](deliveryNoteId: DeliveryNoteId): App[F, Unit] =
    App.aggregateId.flatMap(streamId =>
      if streamId == deliveryNoteId.toString then App.unit else App.reject(DeliveryNoteError.DeliveryNoteIdMismatch),
    )
end DeliveryNoteService
