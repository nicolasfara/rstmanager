package io.github.nicolasfara.rstmanager.warehouse.domain

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class QuantitiesTest extends AnyFlatSpecLike, ScalaCheckPropertyChecks:
  private val genQuantity: Gen[Quantity] = Gen.chooseNum(0L, 1000000000000L).map(Quantity.applyUnsafe(_))
  private val genUnitPrice: Gen[UnitPrice] = Gen.chooseNum(0L, 10000000000L).map(UnitPrice.applyUnsafe(_))
  private val genAmount: Gen[Amount] = Gen.chooseNum(-1000000000000000000L, 1000000000000000000L).map(Amount.applyUnsafe(_))
  private val genSignedQuantity: Gen[SignedQuantity] = Gen.chooseNum(-1000000000000000L, 1000000000000000L).map(SignedQuantity.applyUnsafe(_))

  private def parsed[A](result: Either[String, A]): A = result.fold(error => fail(error), identity)

  /** Reference HALF_UP valuation on `BigDecimal`, independent of the fixed-point implementation. */
  private def referenceCents(quantity: Quantity, price: UnitPrice): Long =
    (BigDecimal(quantity.value) / 1000 * (BigDecimal(price.value) / 10000)).setScale(2, BigDecimal.RoundingMode.HALF_UP).*(100).toLongExact

  "Quantity" should "parse decimal strings with up to 3 decimals into thousandths" in:
    parsed(Quantity.fromDecimalString("2.5")).value shouldEqual 2500L
    parsed(Quantity.fromDecimalString("2.500")).value shouldEqual 2500L
    parsed(Quantity.fromDecimalString(" 12 ")).value shouldEqual 12000L
    parsed(Quantity.fromDecimalString("0.001")).value shouldEqual 1L
    parsed(Quantity.fromDecimalString("1000000000")).value shouldEqual 1000000000000L

  it should "reject more than 3 decimals, negative values, values out of range, and malformed text" in:
    List("2.5001", "-1", "1000000000.001", "", ".5", "5.", "1,5", "1e3", "+1", "1.2.3", "abc", "١", "99999999999999999999999")
      .foreach(raw => withClue(raw)(Quantity.fromDecimalString(raw).isLeft shouldEqual true))

  it should "format with exactly 3 decimals and round-trip through its decimal string" in:
    Quantity.applyUnsafe(2500L).toDecimalString shouldEqual "2.500"
    Quantity.zero.toDecimalString shouldEqual "0.000"
    forAll(genQuantity)(quantity => parsed(Quantity.fromDecimalString(quantity.toDecimalString)) shouldEqual quantity)

  it should "know whether it is a whole number of units" in:
    Quantity.applyUnsafe(3000L).isWhole shouldEqual true
    Quantity.applyUnsafe(3001L).isWhole shouldEqual false

  "SignedQuantity" should "round-trip negative and positive values through its decimal string" in:
    SignedQuantity.applyUnsafe(-500L).toDecimalString shouldEqual "-0.500"
    parsed(SignedQuantity.fromDecimalString("-2.25")).value shouldEqual -2250L
    forAll(genSignedQuantity)(quantity => parsed(SignedQuantity.fromDecimalString(quantity.toDecimalString)) shouldEqual quantity)

  it should "convert to a quantity only when non-negative" in:
    SignedQuantity.applyUnsafe(-1L).toQuantity shouldEqual None
    SignedQuantity.applyUnsafe(1L).toQuantity shouldEqual Some(Quantity.applyUnsafe(1L))
    forAll(genQuantity)(quantity => quantity.toSigned.toQuantity shouldEqual Some(quantity))

  "UnitPrice" should "parse up to 4 decimals and format with exactly 4" in:
    parsed(UnitPrice.fromDecimalString("3.7")).value shouldEqual 37000L
    UnitPrice.applyUnsafe(37000L).toDecimalString shouldEqual "3.7000"
    UnitPrice.fromDecimalString("3.70001").isLeft shouldEqual true
    UnitPrice.fromDecimalString("-0.01").isLeft shouldEqual true
    UnitPrice.fromDecimalString("1000000.0001").isLeft shouldEqual true
    forAll(genUnitPrice)(price => parsed(UnitPrice.fromDecimalString(price.toDecimalString)) shouldEqual price)

  "Amount" should "parse up to 2 signed decimals and format with exactly 2" in:
    parsed(Amount.fromDecimalString("-12.34")).value shouldEqual -1234L
    Amount.applyUnsafe(-5L).toDecimalString shouldEqual "-0.05"
    Amount.applyUnsafe(100L).toDecimalString shouldEqual "1.00"
    Amount.fromDecimalString("1.234").isLeft shouldEqual true
    forAll(genAmount)(amount => parsed(Amount.fromDecimalString(amount.toDecimalString)) shouldEqual amount)

  it should "negate within its symmetric range" in:
    forAll(genAmount)(amount => amount.negate.negate shouldEqual amount)

  "Valuation.amount" should "round half up to the cent" in:
    // 0.333 × 0.1000 € = 0.0333 € → 0.03 €
    Valuation.amount(Quantity.applyUnsafe(333L), UnitPrice.applyUnsafe(1000L)).value shouldEqual 3L
    // 1.000 × 0.0050 € = 0.005 € → 0.01 € (half rounds up)
    Valuation.amount(Quantity.applyUnsafe(1000L), UnitPrice.applyUnsafe(50L)).value shouldEqual 1L
    // 1.000 × 0.0049 € = 0.0049 € → 0.00 €
    Valuation.amount(Quantity.applyUnsafe(1000L), UnitPrice.applyUnsafe(49L)).value shouldEqual 0L
    // 2.500 × 3.7000 € = 9.25 €
    Valuation.amount(Quantity.applyUnsafe(2500L), UnitPrice.applyUnsafe(37000L)).value shouldEqual 925L

  it should "agree with a BigDecimal HALF_UP reference on every quantity and price" in:
    forAll(genQuantity, genUnitPrice)((quantity, price) => Valuation.amount(quantity, price).value shouldEqual referenceCents(quantity, price))

  it should "value the largest quantity at the largest price without overflowing" in:
    Valuation.amount(Quantity.applyUnsafe(1000000000000L), UnitPrice.applyUnsafe(10000000000L)).value shouldEqual 100000000000000000L

  it should "be zero for a zero quantity or a zero price, and monotone in the quantity" in:
    forAll(genQuantity, genUnitPrice) { (quantity, price) =>
      Valuation.amount(Quantity.zero, price) shouldEqual Amount.zero
      Valuation.amount(quantity, UnitPrice.zero) shouldEqual Amount.zero
      Quantity.option(quantity.value + 1).foreach(next => Valuation.amount(next, price).value should be >= Valuation.amount(quantity, price).value)
    }

  "UnitOfMeasure" should "read back every unit from its wire name" in:
    UnitOfMeasure.values.foreach(unit => UnitOfMeasure.fromWireName(unit.wireName) shouldEqual Right(unit))
    UnitOfMeasure.fromWireName(" Square_Meters ") shouldEqual Right(UnitOfMeasure.SquareMeters)
    UnitOfMeasure.fromWireName("litres").isLeft shouldEqual true
    UnitOfMeasure.values.map(_.wireName).toList shouldEqual List("pieces", "linear_meters", "square_meters", "kilograms")
end QuantitiesTest
