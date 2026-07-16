package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingAggregate.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingError.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingService.Command
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent.*

import cats.Id
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import io.github.iltotore.iron.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class ManufacturingTest extends AnyFlatSpecLike:
  private val manufacturingId: ManufacturingId = UUID.fromString("00000000-0000-0000-0000-000000000501").nn
  private val cutting = UUID.fromString("00000000-0000-0000-0000-000000000301").nn
  private val assembly = UUID.fromString("00000000-0000-0000-0000-000000000302").nn
  private val finishing = UUID.fromString("00000000-0000-0000-0000-000000000303").nn

  private val dependencies = ManufacturingDependencies().addTaskDependencies(assembly, Set(cutting))

  private def validManufacturing(): Manufacturing =
    Manufacturing
      .createManufacturing(
        manufacturingId,
        "MFG-TEST",
        "Serramento standard",
        Some("Ciclo standard"),
        List(cutting, assembly),
        dependencies,
      )
      .toEither
      .toOption
      .get

  private def run(
      command: Command,
      state: ManufacturingAggregate,
  ): EdomatonResult[
    ManufacturingAggregate,
    io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent,
    ManufacturingError,
    ManufacturingService.Notification,
  ] =
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, manufacturingId.toString, command)
    ManufacturingService[Id].execute(RequestContext(message, state))

  "Manufacturing.createManufacturing" should "accept a valid non-empty task composition with dependencies" in:
    val result = Manufacturing.createManufacturing(manufacturingId, "MFG-TEST", "Serramento standard", None, List(cutting, assembly), dependencies)

    result.isValid shouldEqual true
    result.foreach { manufacturing =>
      manufacturing.id shouldEqual manufacturingId
      manufacturing.taskIds.toList shouldEqual List(cutting, assembly)
      manufacturing.dependencies.toEdgePairs shouldEqual List(assembly -> cutting)
    }

  it should "reject an empty task composition" in:
    Manufacturing
      .createManufacturing(manufacturingId, "MFG-TEST", "Serramento standard", None, Nil, ManufacturingDependencies())
      .isValid shouldEqual false

  it should "reject duplicate task ids" in:
    Manufacturing
      .createManufacturing(manufacturingId, "MFG-TEST", "Serramento standard", None, List(cutting, cutting), ManufacturingDependencies())
      .isValid shouldEqual false

  it should "reject dependencies that reference tasks outside the composition" in:
    val outsideDependency = ManufacturingDependencies().addTaskDependencies(assembly, Set(finishing))

    Manufacturing
      .createManufacturing(manufacturingId, "MFG-TEST", "Serramento standard", None, List(cutting, assembly), outsideDependency)
      .isValid shouldEqual false

  it should "reject cyclic dependencies" in:
    val cycle = ManufacturingDependencies()
      .addTaskDependencies(cutting, Set(assembly))
      .addTaskDependencies(assembly, Set(cutting))

    Manufacturing
      .createManufacturing(manufacturingId, "MFG-TEST", "Serramento standard", None, List(cutting, assembly), cycle)
      .isValid shouldEqual false

  "ManufacturingService" should "create a catalog manufacturing through the event-sourced service" in:
    val manufacturing = validManufacturing()
    val result = run(Command.Create(manufacturing), Empty)

    result match
      case EdomatonResult.Accepted(newState, events, notifications) =>
        newState shouldEqual Active(manufacturing)
        events.toChain.toList shouldEqual List(ManufacturingCreated(manufacturing))
        notifications.toList shouldEqual List(ManufacturingService.Notification.ManufacturingChanged(manufacturingId))
      case other => fail(s"Unexpected result: $other")

  it should "update an active catalog manufacturing" in:
    val manufacturing = validManufacturing()
    val updated = manufacturing.copy(name = "Serramento premium".refineUnsafe[ManufacturingName])
    val result = run(Command.Update(updated), Active(manufacturing))

    result match
      case EdomatonResult.Accepted(newState, events, notifications) =>
        newState shouldEqual Active(updated)
        events.toChain.toList shouldEqual List(ManufacturingUpdated(updated))
        notifications.toList shouldEqual List(ManufacturingService.Notification.ManufacturingChanged(manufacturingId))
      case other => fail(s"Unexpected result: $other")

  it should "delete an active catalog manufacturing" in:
    val manufacturing = validManufacturing()
    val result = run(Command.Delete, Active(manufacturing))

    result match
      case EdomatonResult.Accepted(newState, events, notifications) =>
        newState shouldEqual Deleted(manufacturing)
        events.toChain.toList shouldEqual List(ManufacturingDeleted)
        notifications shouldBe empty
      case other => fail(s"Unexpected result: $other")

  it should "reject updates when the manufacturing does not exist" in:
    val result = run(Command.Update(validManufacturing()), Empty)

    result match
      case EdomatonResult.Rejected(notifications, reasons) =>
        reasons.toChain.toList shouldEqual List(ManufacturingNotFound)
        notifications shouldBe empty
      case other => fail(s"Unexpected result: $other")
end ManufacturingTest
