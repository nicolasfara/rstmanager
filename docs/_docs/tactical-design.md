# Tactical Design

This model follows common tactical DDD building blocks.

## Aggregate roots

- [[io.github.nicolasfara.rstmanager.customer.domain.Customer]]
  Owns customer identity, contact information, address, and fiscal classification.
- [[io.github.nicolasfara.rstmanager.hr.domain.Employee]]
  Owns employee identity, contract, and budgeted availability.
- [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.Manufacturing]]
  Owns the task list and dependency graph for a manufacturing template.
- [[io.github.nicolasfara.rstmanager.work.domain.order.Order]]
  Owns the order lifecycle and the scheduled manufacturings executed for the order.

## Entities

Entities are the parts of the model that carry identity and evolve over time.

- [[io.github.nicolasfara.rstmanager.customer.domain.Customer]] uses
  [[io.github.nicolasfara.rstmanager.customer.domain.CustomerId]].
- [[io.github.nicolasfara.rstmanager.hr.domain.Employee]] uses
  [[io.github.nicolasfara.rstmanager.hr.domain.EmployeeId]].
- [[io.github.nicolasfara.rstmanager.work.domain.task.Task]] uses
  [[io.github.nicolasfara.rstmanager.work.domain.task.TaskId]].
- [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing]]
  uses `ScheduledManufacturingId`.
- [[io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask]]
  uses `ScheduledTaskId`.

Inside the work execution model, scheduled manufacturings and scheduled tasks are entities
inside the `Order` aggregate boundary rather than independent aggregate roots.

## Value objects

The model leans heavily on immutable value objects and refined primitives.

- Customer value objects:
  [[io.github.nicolasfara.rstmanager.customer.domain.Address]],
  [[io.github.nicolasfara.rstmanager.customer.domain.ContactInfo]],
  [[io.github.nicolasfara.rstmanager.customer.domain.FiscalCode]]
- HR value objects:
  [[io.github.nicolasfara.rstmanager.hr.domain.EmployeeInfo]],
  [[io.github.nicolasfara.rstmanager.hr.domain.Contract]],
  [[io.github.nicolasfara.rstmanager.hr.domain.BudgetHours]],
  [[io.github.nicolasfara.rstmanager.hr.domain.HoursOverride]]
- Work value objects:
  [[io.github.nicolasfara.rstmanager.work.domain.task.TaskHours]],
  [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies]],
  [[io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority]]

Refined aliases such as names, emails, weekly hours, postal codes, and manufacturing codes
move simple validation rules into the type system.

## Domain services and helpers

- [[io.github.nicolasfara.rstmanager.planning.SchedulingService]] is the explicit domain
  service boundary for scheduling use cases.
- [[io.github.nicolasfara.rstmanager.work.domain.order.OrderOperations]] keeps pure helper
  logic that updates nested manufacturings during event application.

## Errors and decisions

- Constructor-style validation uses `ValidatedNec[String, A]`.
- Command-style operations return `Either[Error, A]` or `Decision[Error, Event, State]`.
- Explicit error enums such as
  [[io.github.nicolasfara.rstmanager.work.domain.order.OrderError]],
  [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturingError]],
  and [[io.github.nicolasfara.rstmanager.planning.PlanningError]]
  make failure modes part of the model.
