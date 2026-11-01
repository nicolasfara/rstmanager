package io.github.nicolasfara.rstmanager.service.codecs

import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.hr.domain.EmployeeId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{
  Manufacturing,
  ManufacturingCode,
  ManufacturingDependencies,
  ManufacturingDescription,
  ManufacturingId,
  ManufacturingName,
  ManufacturingService,
}
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.{
  ManufacturingStatus,
  ScheduledManufacturing,
  ScheduledManufacturingId,
  ScheduledManufacturingInfo,
}
import io.github.nicolasfara.rstmanager.work.domain.order.*
import io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent
import io.github.nicolasfara.rstmanager.work.domain.task.{ TaskDuration, TaskId, TaskService }
import io.github.nicolasfara.rstmanager.work.domain.task.events.TaskEvent
import io.github.nicolasfara.rstmanager.work.domain.task.scheduled.{ ScheduledTask, ScheduledTaskId }

import cats.data.NonEmptyList
import com.github.nscala_time.time.Imports.DateTime
import io.circe.{ Codec, Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveCodec, deriveEncoder }
import io.github.iltotore.iron.*
import io.github.iltotore.iron.circe.given

/** Circe codecs for the task/manufacturing catalogs and order graph, used to persist their event streams. */
object WorkCodecs:
  import CommonCodecs.given

  /** The dependency graph is persisted as its directed `(source, target)` edge pairs. */
  given Codec[ManufacturingDependencies] = Codec.from(
    Decoder[List[(TaskId, TaskId)]].map(ManufacturingDependencies.fromEdgePairs),
    Encoder[List[(TaskId, TaskId)]].contramap(_.toEdgePairs),
  )

  /** The order-level manufacturing dependency graph is persisted as its directed `(source, target)` edge pairs. */
  given Codec[OrderDependencies] = Codec.from(
    Decoder[List[(ScheduledManufacturingId, ScheduledManufacturingId)]].map(OrderDependencies.fromEdgePairs),
    Encoder[List[(ScheduledManufacturingId, ScheduledManufacturingId)]].contramap(OrderDependencies.toEdgePairs(_)),
  )

  // A scheduled manufacturing's `code` (`String :| ManufacturingCode`) is persisted as a plain string via iron-circe.

  given Codec[ManufacturingStatus] = deriveCodec
  given Codec[ScheduledTask] = deriveCodec

  /** Events persisted before per-task preferred employees existed lack the `taskPreferredEmployees` field: decode it as the empty map. */
  given Codec[ScheduledManufacturingInfo] = Codec.from(
    Decoder.instance { cursor =>
      for
        id <- cursor.get[ScheduledManufacturingId]("id")
        code <- cursor.get[String :| ManufacturingCode]("code")
        completionDate <- cursor.get[DateTime]("completionDate")
        tasks <- cursor.get[NonEmptyList[ScheduledTask]]("tasks")
        dependencies <- cursor.get[ManufacturingDependencies]("dependencies")
        description <- cursor.get[Option[String]]("description")
        preferredEmployeeId <- cursor.get[Option[EmployeeId]]("preferredEmployeeId")
        taskPreferredEmployees <- cursor.get[Option[Map[ScheduledTaskId, EmployeeId]]]("taskPreferredEmployees")
      yield ScheduledManufacturingInfo(
        id,
        code,
        completionDate,
        tasks,
        dependencies,
        description,
        preferredEmployeeId,
        taskPreferredEmployees.getOrElse(Map.empty),
      )
    },
    deriveEncoder,
  )
  given Codec[ScheduledManufacturing] = deriveCodec
  given Codec[OrderPriority] = deriveCodec

  /** Events persisted before the manufacturing dependency graph existed lack the `dependencies` field: decode it as the empty graph. */
  given Codec[OrderData] = Codec.from(
    Decoder.instance { cursor =>
      for
        id <- cursor.get[OrderId]("id")
        number <- cursor.get[String :| OrderNumber]("number")
        customerId <- cursor.get[CustomerId]("customerId")
        creationDate <- cursor.get[DateTime]("creationDate")
        deliveryDate <- cursor.get[DateTime]("deliveryDate")
        priority <- cursor.get[OrderPriority]("priority")
        setOfManufacturing <- cursor.get[NonEmptyList[ScheduledManufacturing]]("setOfManufacturing")
        description <- cursor.get[Option[String]]("description")
        dependencies <- cursor.get[Option[OrderDependencies]]("dependencies")
      yield OrderData(
        id,
        number,
        customerId,
        creationDate,
        deliveryDate,
        priority,
        setOfManufacturing,
        description,
        dependencies.getOrElse(OrderDependencies.empty),
      )
    },
    deriveEncoder,
  )

  /** Current-version data of the order journal; it is persisted through [[OrderEventSchema]]. */
  val orderEventData: Codec.AsObject[OrderEvent] = deriveCodec
  given Codec[OrderEvent] = EventSchema.codec(OrderEventSchema)

  given orderNotificationCodec: Codec[OrderService.Notification] = deriveCodec

  /** Versioned journal codec: see [[TaskEventSchema]] for the stored versions and their upcasters. */
  given Codec[TaskEvent] = EventSchema.codec(TaskEventSchema)
  given taskNotificationCodec: Codec[TaskService.Notification] = deriveCodec

  /** Events persisted before per-task default employees / hours overrides existed lack those fields: decode each as the empty map. */
  given Codec[Manufacturing] = Codec.from(
    Decoder.instance { cursor =>
      for
        id <- cursor.get[ManufacturingId]("id")
        code <- cursor.get[String :| ManufacturingCode]("code")
        name <- cursor.get[String :| ManufacturingName]("name")
        description <- cursor.get[Option[String :| ManufacturingDescription]]("description")
        taskIds <- cursor.get[NonEmptyList[TaskId]]("taskIds")
        dependencies <- cursor.get[ManufacturingDependencies]("dependencies")
        defaultEmployees <- cursor.get[Option[Map[TaskId, EmployeeId]]]("defaultEmployees")
        taskDurations <- cursor.get[Option[Map[TaskId, TaskDuration]]]("taskDurations")
      yield Manufacturing(
        id,
        code,
        name,
        description,
        taskIds,
        dependencies,
        defaultEmployees.getOrElse(Map.empty),
        taskDurations.getOrElse(Map.empty),
      )
    },
    deriveEncoder,
  )

  /** Current-version data of the manufacturing catalog journal; it is persisted through [[ManufacturingEventSchema]]. */
  val manufacturingEventData: Codec.AsObject[ManufacturingEvent] = deriveCodec
  given Codec[ManufacturingEvent] = EventSchema.codec(ManufacturingEventSchema)

  given manufacturingNotificationCodec: Codec[ManufacturingService.Notification] = deriveCodec
end WorkCodecs
