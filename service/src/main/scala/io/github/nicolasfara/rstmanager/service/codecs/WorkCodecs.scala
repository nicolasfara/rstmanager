package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ Manufacturing, ManufacturingDependencies, ManufacturingService }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{
  ManufacturingStatus,
  ScheduledManufacturing,
  ScheduledManufacturingInfo,
}
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskId, TaskService }
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask

import io.circe.{ Codec, Decoder, Encoder }
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.circe.given

/** Circe codecs for the task/manufacturing catalogs and order graph, used to persist their event streams. */
object WorkCodecs:
  import CommonCodecs.given

  /** The dependency graph is persisted as its directed `(source, target)` edge pairs. */
  given Codec[ManufacturingDependencies] = Codec.from(
    Decoder[List[(TaskId, TaskId)]].map(ManufacturingDependencies.fromEdgePairs),
    Encoder[List[(TaskId, TaskId)]].contramap(_.toEdgePairs),
  )

  // A scheduled manufacturing's `code` (`String :| ManufacturingCode`) is persisted as a plain string via iron-circe.

  given Codec[ManufacturingStatus] = deriveCodec
  given Codec[ScheduledTask] = deriveCodec
  given Codec[ScheduledManufacturingInfo] = deriveCodec
  given Codec[ScheduledManufacturing] = deriveCodec
  given Codec[OrderPriority] = deriveCodec
  given Codec[OrderData] = deriveCodec
  given Codec[OrderEvent] = deriveCodec
  given orderNotificationCodec: Codec[OrderService.Notification] = deriveCodec

  given Codec[Task] = deriveCodec
  given Codec[TaskEvent] = deriveCodec
  given taskNotificationCodec: Codec[TaskService.Notification] = deriveCodec

  given Codec[Manufacturing] = deriveCodec
  given Codec[ManufacturingEvent] = deriveCodec
  given manufacturingNotificationCodec: Codec[ManufacturingService.Notification] = deriveCodec
end WorkCodecs
