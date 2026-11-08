package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import WarehouseViews.*

/** Unit tests for the pure derivations of the warehouse history, flows report, and movements list. */
final class WarehouseViewsSpec extends AnyFunSuite with Matchers:

  private given CanEqual[StateRowResponse, StateRowResponse] = CanEqual.derived

  private val today = "2026-11-01" // a Sunday

  // ---- periods -----------------------------------------------------------------------------------

  test("defaultPeriod covers the last 6 whole months or the last 12 whole ISO weeks, ending today"):
    defaultPeriod(Granularity.Month, today) shouldBe Period("2026-06-01", today)
    defaultPeriod(Granularity.Month, "2026-03-31") shouldBe Period("2025-10-01", "2026-03-31")
    // Monday of the current week is 2026-10-26; 11 weeks earlier is 2026-08-10.
    defaultPeriod(Granularity.Week, today) shouldBe Period("2026-08-10", today)
    defaultPeriod(Granularity.Week, "2026-01-07") shouldBe Period("2025-10-20", "2026-01-07")

  test("periodOnToggle follows the new granularity only for an untouched default period"):
    val monthDefault = defaultPeriod(Granularity.Month, today)
    periodOnToggle(monthDefault, Granularity.Month, Granularity.Week, today) shouldBe defaultPeriod(Granularity.Week, today)
    val custom = Period("2026-01-01", "2026-03-31")
    periodOnToggle(custom, Granularity.Month, Granularity.Week, today) shouldBe custom

  test("validatePeriod mirrors the server rules"):
    validatePeriod("2026-01-01", "2026-03-31") shouldBe Right(Period("2026-01-01", "2026-03-31"))
    validatePeriod("2026-01-01", "2026-01-01").isRight shouldBe true
    validatePeriod("", "2026-03-31") shouldBe Left("Data di inizio non valida")
    validatePeriod("2026-01-01", "x") shouldBe Left("Data di fine non valida")
    validatePeriod("2026-04-01", "2026-03-31") shouldBe Left("La data di inizio è successiva alla data di fine")
    validatePeriod("2021-11-02", "2026-11-01").isRight shouldBe true
    validatePeriod("2021-11-01", "2026-11-01") shouldBe Left("Il periodo non può superare 5 anni")

  test("validateOptionalRange treats blanks as open bounds"):
    validateOptionalRange("", " ") shouldBe Right((None, None))
    validateOptionalRange("2026-10-01", "") shouldBe Right((Some("2026-10-01"), None))
    validateOptionalRange("2026-10-31", "2026-10-01") shouldBe Left("La data di inizio è successiva alla data di fine")
    validateOptionalRange("2026-10-32", "") shouldBe Left("Data di inizio non valida")

  // ---- day navigation ----------------------------------------------------------------------------

  test("stateDay accepts valid days up to today"):
    stateDay("2026-10-15", today) shouldBe Right("2026-10-15")
    stateDay(today, today) shouldBe Right(today)
    stateDay("2026-11-02", today) shouldBe Left("La data non può essere successiva a oggi")
    stateDay("", today) shouldBe Left("Seleziona una data valida")

  test("previousDay and nextDay never pass today"):
    previousDay("2026-03-01") shouldBe "2026-02-28"
    previousDay("") shouldBe ""
    nextDay("2026-10-31", today) shouldBe Some(today)
    nextDay(today, today) shouldBe None
    nextDay("garbage", today) shouldBe None

  // ---- labels ------------------------------------------------------------------------------------

  test("bucketLabel names months and ISO weeks in Italian"):
    bucketLabel("2026-10", "2026-10-01") shouldBe "Ott 2026"
    bucketLabel("2027-01", "2027-01-01") shouldBe "Gen 2027"
    bucketLabel("2026-W43", "2026-10-19") shouldBe "Sett. 43 (19/10)"
    bucketLabel("2026-W53", "2026-12-28") shouldBe "Sett. 53 (28/12)"
    bucketLabel("2026-W02", "2026-01-05") shouldBe "Sett. 2 (05/01)"
    bucketLabel("2026-13", "2026-01-01") shouldBe "2026-13"
    bucketLabel("Q4", "2026-10-01") shouldBe "Q4"

  test("bucketRange shows the counted days"):
    bucketRange("2026-10-19", "2026-10-25") shouldBe "19/10 – 25/10"
    bucketRange("2026-11-01", "2026-11-01") shouldBe "01/11"

  // ---- bars --------------------------------------------------------------------------------------

  test("scaleBars scales to the largest value of the chart"):
    scaleBars(List("100.00" -> "50.00", "25.00" -> "0.00")) shouldBe List(BarPair(100.0, 50.0), BarPair(25.0, 0.0))

  test("scaleBars keeps tiny non-zero bars visible and empty charts flat"):
    scaleBars(List("10000.00" -> "0.01")) shouldBe List(BarPair(100.0, minVisiblePercent))
    scaleBars(List("0.00" -> "0.00", "x" -> "0")) shouldBe List(BarPair(0.0, 0.0), BarPair(0.0, 0.0))
    scaleBars(Nil) shouldBe Nil

  test("scaleBars draws no bar for a negative value and scales the others without it"):
    // A bucket whose consumption reversals exceed its outflows has a negative outValue.
    scaleBars(List("50.00" -> "-80.00", "100.00" -> "25.00")) shouldBe List(BarPair(50.0, 0.0), BarPair(100.0, 25.0))
    scaleBars(List("0.00" -> "-10.00")) shouldBe List(BarPair(0.0, 0.0))

  // ---- rows --------------------------------------------------------------------------------------

  private def row(code: String, description: String, materialType: Option[String]): StateRowResponse =
    StateRowResponse(
      UUID.fromString("00000000-0000-0000-0000-0000000000f1").nn,
      code,
      description,
      materialType,
      None,
      "pieces",
      None,
      StockDto("0.000", None, None, "0.00", false),
      false,
      Nil,
      Nil,
    )

  private val rows = List(
    row("TES-001", "Tessuto acrilico blu", Some("Tessuti")),
    row("PRF-010", "Profilo alluminio", Some("Profili")),
    row("VIT-6", "Viti inox", None),
  )

  test("filterRows matches every term in code, description, or type, ignoring case"):
    filterRows(rows, "").map(_.internalCode) shouldBe rows.map(_.internalCode)
    filterRows(rows, "tes").map(_.internalCode) shouldBe List("TES-001")
    filterRows(rows, "PROFILI").map(_.internalCode) shouldBe List("PRF-010")
    filterRows(rows, " inox  vit ").map(_.internalCode) shouldBe List("VIT-6")
    filterRows(rows, "blu profilo") shouldBe Nil

  // ---- movements ---------------------------------------------------------------------------------

  test("movement kinds and signed quantities read in Italian"):
    movementKindLabel("purchase") shouldBe "Carico"
    movementKindLabel("unload") shouldBe "Scarico"
    movementKindLabel("adjustment") shouldBe "Rettifica"
    movementKindLabel("consumption") shouldBe "Scarico lavorazione"
    movementKindLabel("transfer") shouldBe "transfer"
    signedQty("2.500", "linear_meters") shouldBe "+2,5 m"
    signedQty("-1.000", "pieces") shouldBe "-1 pz"
    signedQty("0.000", "pieces") shouldBe "0 pz"

  private val orderId = UUID.fromString("0a0b0c0d-0000-0000-0000-000000000001").nn
  private val manufacturingId = UUID.fromString("0e0f0a0b-0000-0000-0000-000000000002").nn

  private val purchase: MovementResponse =
    MovementResponse(
      UUID.fromString("00000000-0000-0000-0000-0000000000d1").nn,
      1,
      "purchase",
      "2026-10-20",
      "10.000",
      "10.000",
      Some("3.5000"),
      Some("35.00"),
      Some(MaterialSupplierDto(UUID.fromString("00000000-0000-0000-0000-0000000000a1").nn, "Tessiture Rossi", true)),
      Some("AC-152"),
      None,
      None,
      false,
      None,
      None,
      "10.000",
    )

  private val consumption: MovementResponse =
    purchase.copy(
      kind = "consumption",
      quantity = "2.500",
      delta = "-2.500",
      supplier = None,
      mpnCode = None,
      orderId = Some(orderId),
      manufacturingId = Some(manufacturingId),
      orderNumber = Some("ORD-2026-007"),
      manufacturingCode = Some("COP-01"),
    )

  test("consumptions read as manufacturing outflows or reversals"):
    movementKindLabel(consumption) shouldBe "Scarico lavorazione"
    movementKindLabel(consumption.copy(delta = "2.500")) shouldBe "Storno lavorazione"
    movementKindLabel(purchase) shouldBe "Carico"
    movementKindLabel("unload", "-1.000") shouldBe "Scarico"
    isConsumption(consumption) shouldBe true
    isConsumption(purchase) shouldBe false

  test("consumptionSource names the order and the manufacturing, falling back to short ids"):
    consumptionSource(consumption) shouldBe Some("Ordine ORD-2026-007 – COP-01")
    consumptionSource(consumption.copy(manufacturingCode = None)) shouldBe Some("Ordine ORD-2026-007 – 0e0f0a0b")
    consumptionSource(consumption.copy(orderNumber = None, manufacturingCode = None)) shouldBe Some("Ordine 0a0b0c0d – 0e0f0a0b")
    consumptionSource(consumption.copy(orderNumber = None, manufacturingId = None, manufacturingCode = None)) shouldBe Some("Ordine 0a0b0c0d")
    consumptionSource(purchase) shouldBe None
end WarehouseViewsSpec
