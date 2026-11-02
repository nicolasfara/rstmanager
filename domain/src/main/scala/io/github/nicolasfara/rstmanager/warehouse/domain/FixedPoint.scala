package io.github.nicolasfara.rstmanager.warehouse.domain

/**
 * Exact decimal text ↔ fixed-point integer conversion shared by the warehouse value types ([[Quantity]], [[SignedQuantity]], [[UnitPrice]],
 * [[Amount]]).
 *
 * A value with scale `s` is stored as an integer count of `10^-s` units. The accepted text is the canonical one the API exchanges: an optional `-`,
 * ASCII digits, and optionally `.` followed by 1 to `s` digits (`"2"`, `"2.5"`, `"2.500"`); no grouping separators, exponents, or locale variants
 * (the frontend converts `1.234,56` before sending). Formatting always writes exactly `s` decimals, so parsing a formatted value gives the same
 * integer back.
 */
private[domain] object FixedPoint:
  /** Longest text accepted: far beyond every range, but it keeps a hostile input from building a huge number. */
  private val maxLength = 40

  /** Parses `raw` into a count of `10^-scale` units, rejecting more than `scale` decimals. */
  def parse(raw: String, scale: Int): Either[String, BigInt] =
    val trimmed = raw.trim.nn
    val negative = trimmed.startsWith("-")
    val unsigned = if negative then trimmed.drop(1) else trimmed
    unsigned.span(_ != '.') match
      case (integer, rest) =>
        val fraction = rest.drop(1)
        val wellFormed =
          trimmed.length <= maxLength &&
            integer.nonEmpty && integer.forall(isAsciiDigit) &&
            (rest.isEmpty || (fraction.nonEmpty && fraction.forall(isAsciiDigit)))
        if !wellFormed then Left(s"'$raw' non è un numero decimale (usa cifre e il punto '.' come separatore decimale)")
        else if fraction.length > scale then Left(s"'$raw' ha più di $scale decimali")
        else
          val units = BigInt(integer + fraction.padTo(scale, '0'))
          Right(if negative then -units else units)
  end parse

  /** Writes a count of `10^-scale` units with exactly `scale` decimals (`2500` at scale 3 is `"2.500"`, `-5` at scale 2 is `"-0.05"`). */
  def format(units: Long, scale: Int): String =
    val magnitude = BigInt(units).abs
    val divisor = BigInt(10).pow(scale)
    val sign = if units < 0 then "-" else ""
    if scale <= 0 then s"$sign$magnitude"
    else s"$sign${magnitude / divisor}.${(magnitude % divisor).toString.reverse.padTo(scale, '0').reverse}"

  /** Narrows a parsed unit count to a `Long`, or reports it out of range with `rangeMessage`. */
  def toLong(units: BigInt, rangeMessage: String): Either[String, Long] =
    if units.isValidLong then Right(units.toLong) else Left(rangeMessage)

  /** `n / d` rounded half away from zero (HALF_UP), for any sign of `n` and a positive `d`. */
  def divideHalfUp(n: BigInt, d: BigInt): BigInt =
    n /% d match
      case (quotient, remainder) => if remainder.abs * 2 >= d then quotient + n.signum else quotient

  private def isAsciiDigit(c: Char): Boolean = c >= '0' && c <= '9'
end FixedPoint
