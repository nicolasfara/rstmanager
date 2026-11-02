package io.github.nicolasfara.rstmanager.warehouse.domain

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

/** Range of a stock quantity, in thousandths of the unit of measure: `0` to one billion units. */
type QuantityRange = DescribedAs[GreaterEqual[0L] & LessEqual[1000000000000L], "La quantità deve essere compresa fra 0 e 1.000.000.000"]

/**
 * A non-negative amount of a product, in thousandths of its [[UnitOfMeasure]] (`2500` is 2.5 m, 2.5 kg, …). Fixed point, so sums and comparisons are
 * exact; whole pieces are multiples of `1000`.
 */
type Quantity = Quantity.T

/** Refined type companion for [[Quantity]], with exact decimal-string conversions (3 decimals). */
object Quantity extends RefinedType[Long, QuantityRange]:
  /** Decimals of the textual form. */
  val scale: Int = 3

  /** Thousandths in one unit. */
  val unitsPerOne: Long = 1000L

  val zero: Quantity = Quantity(0L)

  given CanEqual[Quantity, Quantity] = CanEqual.derived

  /** Parses a decimal string with at most 3 decimals (`"2.5"` and `"2.500"` are both `2500`). */
  def fromDecimalString(raw: String): Either[String, Quantity] =
    FixedPoint.parse(raw, scale).flatMap(FixedPoint.toLong(_, "La quantità deve essere compresa fra 0 e 1.000.000.000")).flatMap(either)

  extension (quantity: Quantity)
    /** The quantity with exactly 3 decimals (`2500` is `"2.500"`). */
    def toDecimalString: String = FixedPoint.format(quantity.value, scale)

    /** Whether the quantity is a whole number of units, as required for [[UnitOfMeasure.Pieces]]. */
    def isWhole: Boolean = quantity.value % unitsPerOne == 0L

    /** The same quantity as a signed delta (always in range: the signed range contains the unsigned one). */
    def toSigned: SignedQuantity = SignedQuantity.applyUnsafe(quantity.value)
end Quantity

/** Range of a signed quantity (a stock delta or a balance that may go negative), in thousandths of the unit. */
type SignedQuantityRange =
  DescribedAs[
    GreaterEqual[-1000000000000000L] & LessEqual[1000000000000000L],
    "La variazione di quantità deve essere compresa fra -1.000.000.000.000 e 1.000.000.000.000",
  ]

/** A signed amount of a product, in thousandths of its unit of measure: a stock movement delta or a balance that may be negative. */
type SignedQuantity = SignedQuantity.T

/** Refined type companion for [[SignedQuantity]], with exact decimal-string conversions (3 decimals). */
object SignedQuantity extends RefinedType[Long, SignedQuantityRange]:
  val scale: Int = Quantity.scale

  val zero: SignedQuantity = SignedQuantity(0L)

  given CanEqual[SignedQuantity, SignedQuantity] = CanEqual.derived

  /** Smallest signed quantity. */
  val min: SignedQuantity = SignedQuantity(-1000000000000000L)

  /** Largest signed quantity. */
  val max: SignedQuantity = SignedQuantity(1000000000000000L)

  /**
   * `value` clamped into the signed range. Balances and totals are sums of deltas that each fit the [[Quantity]] range, so they leave the signed
   * range only after a million maximal movements: clamping keeps the sums total without a partial conversion.
   */
  def saturating(value: Long): SignedQuantity = SignedQuantity.applyUnsafe(value.max(min.value).min(max.value))

  /** Parses a decimal string with an optional `-` and at most 3 decimals. */
  def fromDecimalString(raw: String): Either[String, SignedQuantity] =
    FixedPoint
      .parse(raw, scale)
      .flatMap(FixedPoint.toLong(_, "La variazione di quantità deve essere compresa fra -1.000.000.000.000 e 1.000.000.000.000"))
      .flatMap(either)

  extension (quantity: SignedQuantity)
    /** The signed quantity with exactly 3 decimals (`-2500` is `"-2.500"`). */
    def toDecimalString: String = FixedPoint.format(quantity.value, scale)

    /** The opposite delta (always in range: the range is symmetric). */
    def negate: SignedQuantity = SignedQuantity.applyUnsafe(-quantity.value)

    /** The quantity as a non-negative [[Quantity]], when it is one. */
    def toQuantity: Option[Quantity] = Quantity.option(quantity.value)

    /** The magnitude of the delta, clamped into the [[Quantity]] range. */
    def abs: Quantity = Quantity.applyUnsafe(quantity.value.abs.min(1000000000000L))

    /** Whether the quantity is a whole number of units, as required for [[UnitOfMeasure.Pieces]]. */
    def isWhole: Boolean = quantity.value % Quantity.unitsPerOne == 0L
end SignedQuantity

/** Range of a unit price, in ten-thousandths of a euro: `0` to one million euro per unit. */
type UnitPriceRange = DescribedAs[GreaterEqual[0L] & LessEqual[10000000000L], "Il prezzo unitario deve essere compreso fra 0 € e 1.000.000 €"]

/** A non-negative price per unit of measure, in ten-thousandths of a euro (`37000` is 3.70 €), so per-metre or per-kilo prices stay exact. */
type UnitPrice = UnitPrice.T

/** Refined type companion for [[UnitPrice]], with exact decimal-string conversions (4 decimals). */
object UnitPrice extends RefinedType[Long, UnitPriceRange]:
  val scale: Int = 4

  val zero: UnitPrice = UnitPrice(0L)

  given CanEqual[UnitPrice, UnitPrice] = CanEqual.derived

  /** Parses a decimal euro string with at most 4 decimals (`"3.7"` is `37000`). */
  def fromDecimalString(raw: String): Either[String, UnitPrice] =
    FixedPoint.parse(raw, scale).flatMap(FixedPoint.toLong(_, "Il prezzo unitario deve essere compreso fra 0 € e 1.000.000 €")).flatMap(either)

  extension (price: UnitPrice)
    /** The price with exactly 4 decimals (`37000` is `"3.7000"`). */
    def toDecimalString: String = FixedPoint.format(price.value, scale)
end UnitPrice

/** Range of a money amount, in euro cents. */
type AmountRange =
  DescribedAs[
    GreaterEqual[-1000000000000000000L] & LessEqual[1000000000000000000L],
    "L'importo deve essere compreso fra -10.000.000.000.000.000 € e 10.000.000.000.000.000 €",
  ]

/** A signed money amount in euro cents (`-1234` is −12.34 €): a stock value, a purchase total, a balance of flows. */
type Amount = Amount.T

/** Refined type companion for [[Amount]], with exact decimal-string conversions (2 decimals). */
object Amount extends RefinedType[Long, AmountRange]:
  val scale: Int = 2

  val zero: Amount = Amount(0L)

  given CanEqual[Amount, Amount] = CanEqual.derived

  /** Smallest amount. */
  val min: Amount = Amount(-1000000000000000000L)

  /** Largest amount. */
  val max: Amount = Amount(1000000000000000000L)

  /** `cents` clamped into the amount range: sums of valuations leave it only beyond 10^16 euro, so clamping keeps the sums total. */
  def saturating(cents: BigInt): Amount = Amount.applyUnsafe(cents.max(BigInt(min.value)).min(BigInt(max.value)).toLong)

  /** The sum of `amounts`, computed exactly and clamped into the range. */
  def sum(amounts: Iterable[Amount]): Amount = saturating(amounts.foldLeft(BigInt(0))(_ + _.value))

  /** Parses a decimal euro string with an optional `-` and at most 2 decimals. */
  def fromDecimalString(raw: String): Either[String, Amount] =
    FixedPoint
      .parse(raw, scale)
      .flatMap(FixedPoint.toLong(_, "L'importo deve essere compreso fra -10.000.000.000.000.000 € e 10.000.000.000.000.000 €"))
      .flatMap(either)

  extension (amount: Amount)
    /** The amount with exactly 2 decimals (`-1234` is `"-12.34"`). */
    def toDecimalString: String = FixedPoint.format(amount.value, scale)

    /** The opposite amount (always in range: the range is symmetric). */
    def negate: Amount = Amount.applyUnsafe(-amount.value)

    /** `amount + other`, clamped into the range. */
    def plus(other: Amount): Amount = saturating(BigInt(amount.value) + other.value)

    /** `amount - other`, clamped into the range. */
    def minus(other: Amount): Amount = saturating(BigInt(amount.value) - other.value)
end Amount

/** Valuation of stock quantities at a unit price. */
object Valuation:
  /** Thousandths of a unit × ten-thousandths of a euro are 10^-7 euro: dividing by 10^5 gives cents. */
  private val toCents: BigInt = BigInt(100000)

  /**
   * The value of `quantity` at `unitPrice`, in cents rounded HALF_UP (`0.333 × 0.1000 €` = 0.0333 € → 3 cents; `x.xx5` rounds up).
   *
   * Computed on `BigInt`, so the product never overflows. The result is always an [[Amount]]: at most `10^12 × 10^10 / 10^5 = 10^17` cents, within
   * the amount range.
   */
  def amount(quantity: Quantity, unitPrice: UnitPrice): Amount =
    Amount.applyUnsafe(FixedPoint.divideHalfUp(BigInt(quantity.value) * BigInt(unitPrice.value), toCents).toLong)

  /**
   * The value of a signed `quantity` (a negative balance is worth a negative amount), rounded HALF_UP away from zero, so that
   * `signedAmount(-q, p) == -signedAmount(q, p)`. Clamped into the amount range, which only a balance beyond the [[Quantity]] range can exceed.
   */
  def signedAmount(quantity: SignedQuantity, unitPrice: UnitPrice): Amount =
    Amount.saturating(FixedPoint.divideHalfUp(BigInt(quantity.value) * BigInt(unitPrice.value), toCents))
end Valuation
