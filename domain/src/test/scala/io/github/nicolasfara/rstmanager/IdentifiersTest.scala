package io.github.nicolasfara.rstmanager

import org.scalatest.flatspec.AnyFlatSpecLike

/** The identifiers are distinct types: mixing them up, or passing a bare UUID, must not compile. */
class IdentifiersTest extends AnyFlatSpecLike:
  "Identifiers" should "not be interchangeable between entities" in:
    assertTypeError(
      "val orderId: io.github.nicolasfara.rstmanager.work.domain.order.OrderId = " +
        "io.github.nicolasfara.rstmanager.customer.domain.CustomerId(java.util.UUID.randomUUID().nn)",
    )

  it should "keep catalog task ids and scheduled task ids apart" in:
    assertTypeError(
      "val taskId: io.github.nicolasfara.rstmanager.work.domain.task.TaskId = " +
        "io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTaskId(java.util.UUID.randomUUID().nn)",
    )

  it should "require an explicit wrap of a bare UUID" in:
    assertTypeError("val employeeId: io.github.nicolasfara.rstmanager.hr.domain.EmployeeId = java.util.UUID.randomUUID().nn")
    assertCompiles(
      "val employeeId: io.github.nicolasfara.rstmanager.hr.domain.EmployeeId = " +
        "io.github.nicolasfara.rstmanager.hr.domain.EmployeeId(java.util.UUID.randomUUID().nn)",
    )
end IdentifiersTest
