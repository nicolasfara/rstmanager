package io.github.nicolasfara.rstmanager.warehouse.domain

import io.github.nicolasfara.rstmanager.warehouse.domain.product.{ MovementKind, ProductRecord, StockLedger }
import io.github.nicolasfara.rstmanager.warehouse.domain.product.StockFixtures.*

import com.github.nscala_time.time.Imports.LocalDate
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class WarehouseReportsTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:
  private val today: LocalDate = day(120)

  /** A product created `createdOffset` days after the base, with movements from its creation on, maybe archived. */
  private def genProduct(n: Int): Gen[ReportedProduct] =
    for
      unit <- Gen.oneOf(UnitOfMeasure.values.toList)
      createdOffset <- Gen.chooseNum(0, 30)
      raw <- Gen.listOf(genMovement(90, voids = true))
      archivedOffset <- Gen.option(Gen.chooseNum(createdOffset, 120))
    yield
      val record = ProductRecord.created(productInfo(productId(n), f"P$n%03d", unit), day(createdOffset))
      val moved = raw.map(movement => movement.copy(offset = createdOffset + movement.offset))
      ReportedProduct(record.copy(ledger = ledgerOf(moved, n * 1000, StockLedger.empty)), archivedOffset.map(day))

  private val genProducts: Gen[List[ReportedProduct]] =
    Gen.chooseNum(0, 6).flatMap(count => Gen.sequence[List[ReportedProduct], ReportedProduct]((1 to count).map(genProduct)))

  private val genPeriod: Gen[(LocalDate, LocalDate)] =
    for
      start <- Gen.chooseNum(0, 130)
      length <- Gen.chooseNum(0, 100)
    yield (day(start), day(start + length))

  private val genGranularity: Gen[FlowGranularity] = Gen.oneOf(FlowGranularity.values.toList)

  "stateAt" should "give every listed product its own stock of the day" in:
    forAll(genProducts) { products =>
      val state = WarehouseReports.stateAt(products, today)
      state.rows.foreach(row => row.stock shouldEqual row.product.record.ledger.snapshotAt(today))
      val listed = state.rows.map(_.product.record.info.id).toSet
      products.filter(product => product.archivedOn.forall(!_.isBefore(today))).foreach(product => listed should contain(product.record.info.id))
      state.totals.value shouldEqual Amount.sum(state.rows.flatMap(_.stock.value))
    }

  it should "leave out products created later, and archived products without stock" in:
    val created = ProductRecord.created(productInfo(productId(1), "A", UnitOfMeasure.Kilograms), day(10))
    val bought = created.copy(ledger =
      ledgerOf(List(RawMovement(12, purchase(1000L, 20000L, firstSupplier)._1, purchase(1000L, 20000L, firstSupplier)._2, false))),
    )
    val emptyArchived = ReportedProduct(created, Some(day(15)))
    val stockedArchived = ReportedProduct(bought, Some(day(15)))
    WarehouseReports.stateAt(List(emptyArchived, stockedArchived), day(9)).rows shouldEqual Nil
    WarehouseReports.stateAt(List(emptyArchived), day(15)).rows.map(_.archived) shouldEqual List(true)
    WarehouseReports.stateAt(List(emptyArchived), day(16)).rows shouldEqual Nil
    val later = WarehouseReports.stateAt(List(stockedArchived), day(40))
    later.rows.map(row => (row.archived, row.stock.quantity.value, row.customerPrice)) shouldEqual List(
      (true, 1000L, Some(UnitPrice.applyUnsafe(50000L))),
    )
    later.totals.value shouldEqual Amount.applyUnsafe(200L)

  it should "list a product from its first movement when stock was recorded before its creation" in:
    val created = ProductRecord.created(productInfo(productId(1), "A", UnitOfMeasure.Kilograms), day(10))
    val (delta, kind) = purchase(1000L, 20000L, firstSupplier)
    val backdated = ReportedProduct(created.copy(ledger = ledgerOf(List(RawMovement(5, delta, kind, false)))), None)
    WarehouseReports.stateAt(List(backdated), day(4)).rows shouldEqual Nil
    WarehouseReports.stateAt(List(backdated), day(5)).rows.map(row => (row.stock.quantity.value, row.customerPrice)) shouldEqual List(
      (1000L, Some(UnitPrice.applyUnsafe(50000L))),
    )

  "flows" should "sum, per unit, to the change of the balance over the period" in:
    forAll(genProducts, genPeriod, genGranularity) { case (products, (from, to), granularity) =>
      val report = WarehouseReports.flows(products, from, to, granularity)
      UnitOfMeasure.values.foreach { unit =>
        val inUnit = products.filter(_.record.info.unit == unit)
        val expected = inUnit.map(p => (p.record.ledger.balanceAt(to).value: Long) - p.record.ledger.balanceAt(from.minusDays(1).nn).value).sum
        val bucketed = report.buckets.flatMap(_.totals.quantities).filter(_.unit == unit).map(flow => flow.net.value: Long).sum
        bucketed shouldEqual expected
      }
    }

  it should "value purchases as the supplier spend, plus incoming adjustments at their cost" in:
    forAll(genProducts, genPeriod, genGranularity) { case (products, (from, to), granularity) =>
      val report = WarehouseReports.flows(products, from, to, granularity)
      val spend = WarehouseReports.supplierSpend(products, from, to)
      val costedAdjustments = Amount.sum(
        for
          product <- products
          movement <- product.record.ledger.active.toList
          if !movement.on.isBefore(from) && !movement.on.isAfter(to)
          value <- movement.kind match
            case MovementKind.Adjustment(Some(_), _) => movement.recordedValue.toList
            case _ => Nil
        yield value,
      )
      Amount.sum(report.buckets.map(_.totals.purchaseValue)) shouldEqual spend.total
      Amount.sum(report.buckets.map(_.totals.inValue)) shouldEqual spend.total.plus(costedAdjustments)
      report.totals.inValue shouldEqual Amount.sum(report.buckets.map(_.totals.inValue))
      report.totals.outValue shouldEqual Amount.sum(report.buckets.map(_.totals.outValue))
    }

  it should "cover the period with contiguous buckets that start on Mondays or on the 1st" in:
    forAll(genPeriod, genGranularity) { case ((from, to), granularity) =>
      val buckets = WarehouseReports.flows(Nil, from, to, granularity).buckets
      buckets.headOption.map(_.from) shouldEqual Some(from)
      buckets.lastOption.map(_.to) shouldEqual Some(to)
      buckets.zip(buckets.drop(1)).foreach((a, b) => b.from shouldEqual a.to.plusDays(1).nn)
      buckets.foreach(bucket => granularity.start(bucket.start) shouldEqual bucket.start)
    }

  it should "put the last days of 2026 in ISO week 53 and split them by month" in:
    val created = ProductRecord.created(productInfo(productId(1), "A", UnitOfMeasure.Pieces), LocalDate.parse("2026-12-01"))
    val movements = List(LocalDate.parse("2026-12-31"), LocalDate.parse("2027-01-01"), LocalDate.parse("2027-01-04")).map(on =>
      RawMovement(
        org.joda.time.Days.daysBetween(base, on).getDays,
        purchase(1000L, 10000L, firstSupplier)._1,
        purchase(1000L, 10000L, firstSupplier)._2,
        false,
      ),
    )
    val product = ReportedProduct(created.copy(ledger = ledgerOf(movements)), None)
    val weeks = WarehouseReports.flows(List(product), LocalDate.parse("2026-12-20"), LocalDate.parse("2027-01-10"), FlowGranularity.Week).buckets
    weeks.map(bucket => (bucket.label, bucket.start.toString, bucket.from.toString, bucket.to.toString)) shouldEqual List(
      ("2026-W51", "2026-12-14", "2026-12-20", "2026-12-20"),
      ("2026-W52", "2026-12-21", "2026-12-21", "2026-12-27"),
      ("2026-W53", "2026-12-28", "2026-12-28", "2027-01-03"),
      ("2027-W01", "2027-01-04", "2027-01-04", "2027-01-10"),
    )
    weeks.map(_.totals.inValue.value) shouldEqual List(0L, 0L, 200L, 100L)
    val months = WarehouseReports.flows(List(product), LocalDate.parse("2026-12-20"), LocalDate.parse("2027-01-10"), FlowGranularity.Month).buckets
    months.map(bucket => (bucket.label, bucket.from.toString, bucket.to.toString, bucket.totals.inValue.value)) shouldEqual List(
      ("2026-12", "2026-12-20", "2026-12-31", 100L),
      ("2027-01", "2027-01-01", "2027-01-10", 200L),
    )

  it should "use whole months, February included" in:
    val months = WarehouseReports.flows(Nil, LocalDate.parse("2028-01-15"), LocalDate.parse("2028-03-10"), FlowGranularity.Month).buckets
    months.map(bucket => (bucket.start.toString, bucket.from.toString, bucket.to.toString)) shouldEqual List(
      ("2028-01-01", "2028-01-15", "2028-01-31"),
      ("2028-02-01", "2028-02-01", "2028-02-29"),
      ("2028-03-01", "2028-03-01", "2028-03-10"),
    )

  it should "value outgoing movements at the highest cost of their day and ignore voided ones" in:
    val created = ProductRecord.created(productInfo(productId(1), "A", UnitOfMeasure.LinearMeters), base)
    val ledger = ledgerOf(
      List(
        RawMovement(0, purchase(10000L, 30000L, firstSupplier)._1, purchase(10000L, 30000L, firstSupplier)._2, false),
        RawMovement(1, purchase(1000L, 50000L, secondSupplier)._1, purchase(1000L, 50000L, secondSupplier)._2, true),
        RawMovement(2, SignedQuantity.applyUnsafe(-2000L), MovementKind.Unload(reason), false),
      ),
    )
    val report = WarehouseReports.flows(List(ReportedProduct(created.copy(ledger = ledger), None)), base, day(5), FlowGranularity.Month)
    report.totals.inValue shouldEqual Amount.applyUnsafe(3000L)
    report.totals.outValue shouldEqual Amount.applyUnsafe(600L)
    report.totals.net shouldEqual Amount.applyUnsafe(2400L)
    report.totals.quantities shouldEqual List(
      UnitFlow(UnitOfMeasure.LinearMeters, SignedQuantity.applyUnsafe(10000L), SignedQuantity.applyUnsafe(2000L)),
    )

  "supplierSpend" should "group the purchases of the period by supplier and product MPN" in:
    val created = ProductRecord.created(productInfo(productId(1), "A", UnitOfMeasure.Kilograms), base)
    val ledger = ledgerOf(
      List(
        RawMovement(1, purchase(1000L, 10000L, firstSupplier)._1, purchase(1000L, 10000L, firstSupplier)._2, false),
        RawMovement(2, purchase(2000L, 10000L, firstSupplier)._1, purchase(2000L, 10000L, firstSupplier)._2, false),
        RawMovement(3, purchase(1000L, 90000L, secondSupplier)._1, purchase(1000L, 90000L, secondSupplier)._2, false),
        RawMovement(4, purchase(5000L, 10000L, secondSupplier)._1, purchase(5000L, 10000L, secondSupplier)._2, true),
        RawMovement(40, purchase(5000L, 10000L, firstSupplier)._1, purchase(5000L, 10000L, firstSupplier)._2, false),
      ),
    )
    val spend = WarehouseReports.supplierSpend(List(ReportedProduct(created.copy(ledger = ledger), None)), day(1), day(30))
    spend.total shouldEqual Amount.applyUnsafe(1200L)
    spend.suppliers.map(supplier => (supplier.supplierId, supplier.amount.value, supplier.purchases)) shouldEqual
      List((secondSupplier, 900L, 1), (firstSupplier, 300L, 2))
    spend.suppliers.flatMap(_.mpns).map(mpn => (mpn.mpnCode, mpn.quantity.value, mpn.purchases)) shouldEqual List(
      ("AC-100", 1000L, 1),
      ("AC-100", 3000L, 2),
    )
end WarehouseReportsTest
