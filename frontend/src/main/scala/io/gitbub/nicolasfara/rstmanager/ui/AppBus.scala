package io.gitbub.nicolasfara.rstmanager.ui

import scala.scalajs.js.timers.setTimeout

import com.raquo.laminar.api.L.*

/**
 * Global reactive refresh bus with per-domain granularity.
 *
 * Each data domain has its own tick signal. Pages subscribe only to the domains whose data they display, so a mutation on one domain no longer
 * cascades a full reload to every mounted page.
 *
 * Mutations to orders or employees also trigger an asynchronous planning recalculation on the backend (outbox consumer). `mutatedOrders()` /
 * `mutatedEmployees()` therefore fire extra delayed ticks on `planningTicks` (not on the other domain signals) to converge on the recomputed plan.
 * All reloads are in-place (see `Components.loadable`), so these extra ticks are visually silent.
 */
object AppBus:

  /**
   * Delays (ms) of the extra planning refreshes fired after an order/employee mutation, to catch the async plan recalculation once it has settled.
   */
  private val settleDelaysMs = List(1200, 3200)

  private val ordersVersion = Var(0)
  private val employeesVersion = Var(0)
  private val customersVersion = Var(0)
  private val tasksVersion = Var(0)
  private val manufacturingsVersion = Var(0)
  private val planningVersion = Var(0)

  val ordersTicks: Signal[Int] = ordersVersion.signal
  val employeesTicks: Signal[Int] = employeesVersion.signal
  val customersTicks: Signal[Int] = customersVersion.signal
  val tasksTicks: Signal[Int] = tasksVersion.signal
  val manufacturingsTicks: Signal[Int] = manufacturingsVersion.signal
  val planningTicks: Signal[Int] = planningVersion.signal

  /** Reports a successful order mutation: refreshes orders immediately and fires settle ticks on planning. */
  def mutatedOrders(): Unit =
    ordersVersion.update(_ + 1)
    planningVersion.update(_ + 1)
    settleDelaysMs.foreach(d => setTimeout(d)(planningVersion.update(_ + 1)))

  /** Reports a successful employee mutation: refreshes employees immediately and fires settle ticks on planning. */
  def mutatedEmployees(): Unit =
    employeesVersion.update(_ + 1)
    planningVersion.update(_ + 1)
    settleDelaysMs.foreach(d => setTimeout(d)(planningVersion.update(_ + 1)))

  /** Reports a successful customer mutation: refreshes customers immediately. */
  def mutatedCustomers(): Unit = customersVersion.update(_ + 1)

  /** Reports a successful task-catalog mutation: refreshes tasks immediately. */
  def mutatedTasks(): Unit = tasksVersion.update(_ + 1)

  /** Reports a successful manufacturing-catalog mutation: refreshes manufacturings immediately. */
  def mutatedManufacturings(): Unit = manufacturingsVersion.update(_ + 1)
end AppBus
