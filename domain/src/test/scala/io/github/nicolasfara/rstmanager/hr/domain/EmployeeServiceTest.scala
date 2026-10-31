package io.github.nicolasfara.rstmanager.hr.domain

import java.time.Instant
import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.EmployeeService.{ Command, Notification }
import io.github.nicolasfara.rstmanager.hr.domain.events.EmployeeEvent

import cats.Id
import com.github.nscala_time.time.Imports.LocalDate
import edomata.core.{ CommandMessage, EdomatonResult, RequestContext }
import edomata.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*

class EmployeeServiceTest extends AnyFlatSpecLike:
  private val employeeId: EmployeeId = UUID.fromString("00000000-0000-0000-0000-0000000000e1").nn
  private val day: LocalDate = LocalDate.parse("2026-08-03")

  private def employeeWith(overrides: List[HoursOverride]): Employee =
    Employee
      .createEmployee(employeeId, "Mario", "Rossi", Contract.createFullTime(LocalDate.parse("2026-01-01")), 40, overrides)
      .fold(errors => fail(errors.toChain.toList.mkString(", ")), identity)

  private def vacation(first: LocalDate, last: LocalDate): HoursOverride =
    VacationOverride.createVacationOverride(first, last).fold(errors => fail(errors.toChain.toList.mkString), identity)

  private def workingDay(on: LocalDate): HoursOverride =
    WorkingDayOverride.createWorkingDayOverride(4, None, on).fold(errors => fail(errors.toChain.toList.mkString), identity)

  private def run(command: Command, state: EmployeeAggregate): EdomatonResult[EmployeeAggregate, EmployeeEvent, EmployeeError, Notification] =
    run(command, state, employeeId)

  private def run(command: Command, state: EmployeeAggregate, address: EmployeeId): EdomatonResult[EmployeeAggregate, EmployeeEvent, EmployeeError, Notification] =
    val message = CommandMessage("message-1", Instant.parse("2026-06-14T12:00:00Z").nn, address.toString, command)
    EmployeeService[Id].execute(RequestContext(message, state))

  private def rejection(result: EdomatonResult[EmployeeAggregate, EmployeeEvent, EmployeeError, Notification]): List[EmployeeError] =
    result match
      case EdomatonResult.Rejected(_, reasons) => reasons.toChain.toList
      case other => fail(s"Expected a rejection, got: $other")

  "EmployeeService" should "accept non-conflicting overrides" in:
    val employee = employeeWith(List(vacation(day, day.plusDays(4)), workingDay(day.plusDays(5))))
    run(Command.Create(employee), EmployeeAggregate.Empty) should matchPattern { case EdomatonResult.Accepted(EmployeeAggregate.Active(_), _, _) => }

  it should "reject overlapping vacations, a working day inside a vacation, and two overrides for one day" in:
    val conflicting = List(
      List(vacation(day, day.plusDays(4)), vacation(day.plusDays(4), day.plusDays(6))),
      List(vacation(day, day.plusDays(4)), workingDay(day.plusDays(2))),
      List(workingDay(day), workingDay(day)),
    )
    conflicting.foreach { overrides =>
      rejection(run(Command.Create(employeeWith(overrides)), EmployeeAggregate.Empty)) shouldEqual List(EmployeeError.ConflictingHoursOverrides)
      rejection(run(Command.Update(employeeWith(overrides)), EmployeeAggregate.Active(employeeWith(Nil)))) shouldEqual
        List(EmployeeError.ConflictingHoursOverrides)
    }

  it should "reject an employee sent to another employee's stream" in:
    val otherStream: EmployeeId = UUID.fromString("00000000-0000-0000-0000-0000000000e2").nn
    rejection(run(Command.Create(employeeWith(Nil)), EmployeeAggregate.Empty, otherStream)) shouldEqual List(EmployeeError.EmployeeIdMismatch)

  it should "publish the deleted employee's id, taken from its state" in:
    run(Command.Delete, EmployeeAggregate.Active(employeeWith(Nil))) match
      case EdomatonResult.Accepted(_, _, notifications) => notifications.toList shouldEqual List(Notification.EmployeeChanged(employeeId))
      case other => fail(s"Unexpected result: $other")
end EmployeeServiceTest
