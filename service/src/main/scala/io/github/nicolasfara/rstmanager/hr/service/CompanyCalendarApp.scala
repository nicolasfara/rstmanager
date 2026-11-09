package io.github.nicolasfara.rstmanager.hr.service

import java.time.Instant
import java.util.UUID

import scala.concurrent.duration.*

import io.github.nicolasfara.rstmanager.hr.domain.calendar.*
import io.github.nicolasfara.rstmanager.hr.domain.calendar.events.CompanyCalendarEvent
import io.github.nicolasfara.rstmanager.service.codecs.HrCodecs.given

import cats.effect.{ IO, Resource }
import cats.syntax.all.*
import edomata.backend.Backend
import edomata.backend.eventsourcing.{ AggregateState, Backend as EventSourcedBackend }
import edomata.core.CommandMessage
import edomata.skunk.{ BackendCodec, CirceCodec, SkunkDriver }
import skunk.Session

/**
 * Application backend for the company calendar API: the singleton [[CompanyCalendarAggregate]] on one fixed stream, [[calendarAddress]].
 *
 * There is a single calendar, so no id registry is needed: [[get]] reads the stream directly.
 */
object CompanyCalendarApp:
  given BackendCodec[CompanyCalendarEvent] = CirceCodec.jsonb
  given BackendCodec[CompanyCalendarService.Notification] = CirceCodec.jsonb

  /** Address of the singleton company calendar aggregate: every calendar change is serialized on this stream. */
  val calendarAddress: String = "company-calendar"

  type EntityBackend = EventSourcedBackend[IO, CompanyCalendar, CompanyCalendarEvent, CompanyCalendarError, CompanyCalendarService.Notification]

  final case class Store(entity: EntityBackend)

  def build(pool: Resource[IO, Session[IO]]): Resource[IO, Store] =
    Backend
      .builder(CompanyCalendarService)
      .use(SkunkDriver("company_calendar", pool))
      .inMemSnapshot(10)
      .withRetryConfig(retryInitialDelay = 2.seconds)
      .build
      .map(Store(_))

  def add(store: Store, closure: CompanyClosure): IO[Either[CompanyCalendarError, Unit]] =
    dispatch(store, CompanyCalendarService.Command.Add(closure))

  def update(store: Store, closure: CompanyClosure): IO[Either[CompanyCalendarError, Unit]] =
    dispatch(store, CompanyCalendarService.Command.Update(closure))

  def remove(store: Store, id: ClosureId): IO[Either[CompanyCalendarError, Unit]] =
    dispatch(store, CompanyCalendarService.Command.Remove(id))

  /** The current calendar: [[CompanyCalendar.empty]] while the stream does not exist yet. */
  def get(store: Store): IO[CompanyCalendar] =
    store.entity.repository.get(calendarAddress).map {
      case AggregateState.Valid(calendar, _) => calendar
      // The fold accepts every event, so a conflict cannot happen; the last folded calendar is still the best answer.
      case AggregateState.Conflicted(last, _, _) => last
    }

  private def dispatch(store: Store, command: CompanyCalendarService.Command): IO[Either[CompanyCalendarError, Unit]] =
    val service = store.entity.compile(CompanyCalendarService[IO])
    for
      commandId <- IO(UUID.randomUUID().nn.toString)
      now <- IO(Instant.now().nn)
      result <- service(CommandMessage(commandId, now, calendarAddress, command))
    yield result.bimap(_.head, _ => ())
end CompanyCalendarApp
