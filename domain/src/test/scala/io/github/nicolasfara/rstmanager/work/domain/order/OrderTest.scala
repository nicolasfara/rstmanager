package io.github.nicolasfara.rstmanager.work.domain.order

import io.github.iltotore.iron.*
import com.github.nscala_time.time.Imports.*
import io.github.nicolasfara.rstmanager.customer.domain.CustomerId
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.{Manufacturing, ManufacturingCode, ManufacturingName}
import io.github.nicolasfara.rstmanager.work.domain.task.{CompletableTask, CompletableTaskId, Hours, TaskId}
import org.scalactic.anyvals.NonEmptySet
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

import java.util.UUID

class OrderTest extends AnyFlatSpecLike:
  private val manufacturing = NonEmptySet(
    Manufacturing(
      code = ManufacturingCode("MFG-001"),
      name = ManufacturingName("Test Manufacturing"),
      description = None,
      tasks = List.empty
    ),
    Manufacturing(
      code = ManufacturingCode("MFG-002"),
      name = ManufacturingName("Another Manufacturing"),
      description = None,
      tasks = List(
        CompletableTask(
          completableTaskId = CompletableTaskId(UUID.randomUUID()),
          taskId = TaskId(UUID.randomUUID()),
          expectedHours = Hours(15),
          completedHours = Hours(5)
        ),
        CompletableTask(
          completableTaskId = CompletableTaskId(UUID.randomUUID()),
          taskId = TaskId(UUID.randomUUID()),
          expectedHours = Hours(25),
          completedHours = Hours(10)
        )
      )
    )
  )
  "An Order" should "compute the total hours" in:
    val order = Order(
      id = OrderId(UUID.randomUUID()),
      number = OrderNumber("ORD-001"),
      customerId = CustomerId(UUID.randomUUID()),
      creationDate = DateTime.now(),
      deliveryDate = DateTime.now().plusDays(7),
      priority = OrderPriority.Normal,
      setOfManufacturing = manufacturing
    )
    order.totalHours shouldEqual (Hours(40): Hours)
  it should "compute the remaining hours" in:
    val order = Order(
      id = OrderId(UUID.randomUUID()),
      number = OrderNumber("ORD-001"),
      customerId = CustomerId(UUID.randomUUID()),
      creationDate = DateTime.now(),
      deliveryDate = DateTime.now().plusDays(7),
      priority = OrderPriority.Normal,
      setOfManufacturing = manufacturing
    )
    order.remainingHours shouldEqual (Hours(25): Hours)
  it should "add a manufacturing" in:
    val order = Order(
      id = OrderId(UUID.randomUUID()),
      number = OrderNumber("ORD-001"),
      customerId = CustomerId(UUID.randomUUID()),
      creationDate = DateTime.now(),
      deliveryDate = DateTime.now().plusDays(7),
      priority = OrderPriority.Normal,
      setOfManufacturing = manufacturing
    )
    val newManufacturing = Manufacturing(
      code = ManufacturingCode("MFG-003"),
      name = ManufacturingName("New Manufacturing"),
      description = None,
      tasks = List.empty
    )
    val updatedOrder = order.addManufacturing(newManufacturing)
    updatedOrder.setOfManufacturing.toSet should contain theSameElementsAs (manufacturing + newManufacturing)
  it should "remove a manufacturing" in:
    val order = Order(
      id = OrderId(UUID.randomUUID()),
      number = OrderNumber("ORD-001"),
      customerId = CustomerId(UUID.randomUUID()),
      creationDate = DateTime.now(),
      deliveryDate = DateTime.now().plusDays(7),
      priority = OrderPriority.Normal,
      setOfManufacturing = manufacturing
    )
    val manufacturingToRemove = manufacturing.head
    val updatedOrderEither = order.removeManufacturing(manufacturingToRemove.code)
    updatedOrderEither match
      case Right(updatedOrder) =>
        updatedOrder.setOfManufacturing.toSet should contain theSameElementsAs manufacturing.tail.toSeq
      case Left(_) => fail("Expected successful removal of manufacturing")
  it should "not remove the last manufacturing" in:
    val singleManufacturingSet = NonEmptySet(
      Manufacturing(
        code = ManufacturingCode("MFG-001"),
        name = ManufacturingName("Test Manufacturing"),
        description = None,
        tasks = List.empty
      )
    )
    val order = Order(
      id = OrderId(UUID.randomUUID()),
      number = OrderNumber("ORD-001"),
      customerId = CustomerId(UUID.randomUUID()),
      creationDate = DateTime.now(),
      deliveryDate = DateTime.now().plusDays(7),
      priority = OrderPriority.Normal,
      setOfManufacturing = singleManufacturingSet
    )
    val manufacturingToRemove = singleManufacturingSet.head
    val updatedOrderEither = order.removeManufacturing(manufacturingToRemove.code)
    updatedOrderEither match
      case Left(error) => error shouldEqual OrderError.OrderWithNoManufacturing
      case Right(_)    => fail("Expected failure when removing the last manufacturing")
  it should "update a manufacturing" in:
    val order = Order(
      id = OrderId(UUID.randomUUID()),
      number = OrderNumber("ORD-001"),
      customerId = CustomerId(UUID.randomUUID()),
      creationDate = DateTime.now(),
      deliveryDate = DateTime.now().plusDays(7),
      priority = OrderPriority.Normal,
      setOfManufacturing = manufacturing
    )
    val manufacturingToUpdate = manufacturing.head
    val updatedManufacturing = manufacturingToUpdate.copy(name = ManufacturingName("Updated Name"))
    val updatedOrder = order.updateManufacturing(updatedManufacturing)
    updatedOrder.setOfManufacturing.find(_.code == updatedManufacturing.code).get.name shouldEqual (ManufacturingName(
      "Updated Name"
    ): ManufacturingName)
