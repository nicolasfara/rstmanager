# Bounded Contexts

The module is organized as a set of bounded contexts expressed directly in the package
structure.

## Customer

Package: `io.github.nicolasfara.rstmanager.customer.domain`

Responsibilities:

- customer identity and classification
- contact details and postal address
- fiscal identity

Main types:

- Aggregate root: [[io.github.nicolasfara.rstmanager.customer.domain.Customer]]
- Supporting value objects: [[io.github.nicolasfara.rstmanager.customer.domain.ContactInfo]],
  [[io.github.nicolasfara.rstmanager.customer.domain.Address]],
  [[io.github.nicolasfara.rstmanager.customer.domain.FiscalCode]]
- Reference types: [[io.github.nicolasfara.rstmanager.customer.domain.CustomerId]],
  [[io.github.nicolasfara.rstmanager.customer.domain.CustomerType]]

## Human Resources

Package: `io.github.nicolasfara.rstmanager.hr.domain`

Responsibilities:

- employee identity and personal information
- contract rules
- weekly capacity and temporary overrides

Main types:

- Aggregate root: [[io.github.nicolasfara.rstmanager.hr.domain.Employee]]
- Value objects: [[io.github.nicolasfara.rstmanager.hr.domain.EmployeeInfo]],
  [[io.github.nicolasfara.rstmanager.hr.domain.Contract]],
  [[io.github.nicolasfara.rstmanager.hr.domain.BudgetHours]]
- Capacity policies: [[io.github.nicolasfara.rstmanager.hr.domain.HoursOverride]],
  [[io.github.nicolasfara.rstmanager.hr.domain.WeeklyHours]]

## Work

Packages:

- `io.github.nicolasfara.rstmanager.work.domain.manufacturing`
- `io.github.nicolasfara.rstmanager.work.domain.task`
- `io.github.nicolasfara.rstmanager.work.domain.order`

Responsibilities:

- define manufacturings and their task graph
- track scheduled execution
- manage order lifecycle and fulfillment

This context contains two tightly related sub-models.

### Manufacturing definition

- Aggregate root: [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.Manufacturing]]
- Entities: [[io.github.nicolasfara.rstmanager.work.domain.task.Task]]
- Structural value object: [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingDependencies]]

### Order execution

- Aggregate root: [[io.github.nicolasfara.rstmanager.work.domain.order.Order]]
- Nested entities: [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.scheduled.ScheduledManufacturing]],
  [[io.github.nicolasfara.rstmanager.work.domain.task.scheduled.ScheduledTask]]
- Supporting state and policies: [[io.github.nicolasfara.rstmanager.work.domain.order.OrderData]],
  [[io.github.nicolasfara.rstmanager.work.domain.order.OrderPriority]],
  [[io.github.nicolasfara.rstmanager.work.domain.order.events.OrderEvent]]

## Planning

Package: `io.github.nicolasfara.rstmanager.planning`

Responsibilities:

- scheduling and capacity allocation
- planning failures and constraint reporting
- orchestration across work and capacity models

Main types:

- Domain service boundary: [[io.github.nicolasfara.rstmanager.planning.SchedulingService]]
- Planning failures: [[io.github.nicolasfara.rstmanager.planning.PlanningError]]

## Cross-context relationships

- [[io.github.nicolasfara.rstmanager.work.domain.order.OrderData]] references
  [[io.github.nicolasfara.rstmanager.customer.domain.CustomerId]].
- Planning errors reference
  [[io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingCode]].
- The planning context is expected to combine work demand with HR capacity.
