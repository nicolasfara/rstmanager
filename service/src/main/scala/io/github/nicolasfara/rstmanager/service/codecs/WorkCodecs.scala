package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{ ManufacturingCode, ManufacturingDependencies }
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{ ScheduledManufacturing, ScheduledManufacturingInfo }
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.task.{ Task, TaskId, TaskService }
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask

import io.circe.{ Codec, Decoder, Encoder }
import io.circe.generic.semiauto.deriveCodec
import io.github.iltotore.iron.circe.given
import io.github.iltotore.iron.constraint.all.Empty
import io.github.iltotore.iron.constraint.any.{ DescribedAs, Not }

/** Circe codecs for the task catalog and order/manufacturing graph, used to persist [[TaskEvent]] and [[OrderEvent]]. */
object WorkCodecs:
  import CommonCodecs.given

  /** The dependency graph is persisted as its directed `(source, target)` edge pairs. */
  given Codec[ManufacturingDependencies] = Codec.from(
    Decoder[List[(TaskId, TaskId)]].map(ManufacturingDependencies.fromEdgePairs),
    Encoder[List[(TaskId, TaskId)]].contramap(_.toEdgePairs),
  )

  /**
   * A scheduled manufacturing's `code` is a phantom `ManufacturingCode` marker carrying no data, so it is persisted as a
   * constant and rebuilt on read.
   */
  private val manufacturingCodeMarker: ManufacturingCode =
    new DescribedAs[Not[Empty], "The code manufacturing should be not empty"]()
  given Codec[ManufacturingCode] = Codec.from(
    Decoder.const(manufacturingCodeMarker),
    Encoder.encodeString.contramap(_ => "manufacturing"),
  )

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
end WorkCodecs
