package io.gitbub.nicolasfara.rstmanager.ui

import scala.math.BigDecimal.RoundingMode

/**
 * Pure (DOM-free) conversions between the warehouse decimal strings of the API and the Italian text shown and typed in the UI.
 *
 * The API exchanges quantities, prices, and amounts as canonical decimal strings (`"18.5000"`, `"2.500"`): optional `-`, ASCII digits, optional `.`
 * and decimals, no grouping. Arithmetic never goes through `Double`, so no value loses precision on the way.
 */
object WarehouseFormats:

  /** Unit of measure of a material, mirroring the backend wire names. `allowsFractions` is false for units counted whole (pieces, packs). */
  enum MeasureUnit(val wireName: String, val label: String, val shortLabel: String, val allowsFractions: Boolean) derives CanEqual:
    case Pieces extends MeasureUnit("pieces", "Pezzi", "pz", false)
    case LinearMeters extends MeasureUnit("linear_meters", "Metri lineari", "m", true)
    case SquareMeters extends MeasureUnit("square_meters", "Metri quadri", "m²", true)
    case Kilograms extends MeasureUnit("kilograms", "Chilogrammi", "kg", true)
    case Packs extends MeasureUnit("packs", "Confezioni", "cf", false)

  object MeasureUnit:
    def fromWire(wireName: String): Option[MeasureUnit] = values.find(_.wireName == wireName)

    /** `(wireName, label)` pairs for a select, in display order. */
    val options: List[(String, String)] = values.toList.map(u => u.wireName -> u.label)

  /** Full Italian label of a unit wire name (`"linear_meters"` → `"Metri lineari"`); an unknown name is shown as is. */
  def unitLabel(wireName: String): String = MeasureUnit.fromWire(wireName).fold(wireName)(_.label)

  /** Short Italian label of a unit wire name for tables (`"square_meters"` → `"m²"`); an unknown name is shown as is. */
  def unitShortLabel(wireName: String): String = MeasureUnit.fromWire(wireName).fold(wireName)(_.shortLabel)

  /** Longest text accepted by [[parseDecimal]], so a pasted blob is rejected instead of being turned into a huge number. */
  private val maxInputLength = 32

  private val canonicalPattern = "^-?[0-9]+(\\.[0-9]+)?$".r

  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  /** Non-negative variant of [[parseDecimal]]: negative input is rejected. */
  def parseDecimal(text: String, maxScale: Int): Option[String] = parseDecimal(text, maxScale, false)

  /**
   * Reads a decimal typed by the user into the canonical string the API expects, padded to exactly `maxScale` decimals (`"18,5"` → `"18.5000"` with
   * `maxScale = 4`).
   *
   * Accepted: `,` or `.` as decimal separator (`"1,5"`, `"1.5"`), grouping with the other separator in groups of three (`"1.234,56"`, `"1,234.56"`,
   * `"1.234.567"`), surrounding blanks, an omitted integer part (`",5"`). A single `.` is always the decimal separator, so `"1.234"` is 1.234, not
   * 1234. Rejected (`None`): blanks, garbage, more than `maxScale` decimals, malformed groups, a trailing separator, and a `-` unless
   * `allowNegative`.
   */
  def parseDecimal(text: String, maxScale: Int, allowNegative: Boolean): Option[String] =
    val trimmed = text.trim.nn
    val negative = trimmed.startsWith("-")
    val body = if negative then trimmed.drop(1) else trimmed
    if body.isEmpty || trimmed.length > maxInputLength || (negative && !allowNegative) then None
    else
      splitNumber(body).filter((_, fraction) => fraction.length <= maxScale).map { (integerDigits, fraction) =>
        val integer = integerDigits.dropWhile(_ == '0')
        val isZero = integer.isEmpty && fraction.forall(_ == '0')
        val sign = if negative && !isZero then "-" else ""
        val decimals = if maxScale > 0 then "." + fraction.padTo(maxScale, '0') else ""
        s"$sign${if integer.isEmpty then "0" else integer}$decimals"
      }
  end parseDecimal

  /** Which separator plays the decimal role and which the grouping one in a typed number. */
  private final case class Separators(decimal: Option[Char], group: Option[Char])

  /** Decimal separator: the last of the two when both appear, otherwise a lone separator; a repeated one can only be grouping. */
  private def separatorsOf(body: String): Separators =
    val commas = body.count(_ == ',')
    val dots = body.count(_ == '.')
    if commas > 0 && dots > 0 then
      if body.lastIndexOf(',') > body.lastIndexOf('.') then Separators(Some(','), Some('.')) else Separators(Some('.'), Some(','))
    else if commas == 1 then Separators(Some(','), None)
    else if commas > 1 then Separators(None, Some(','))
    else if dots == 1 then Separators(Some('.'), None)
    else if dots > 1 then Separators(None, Some('.'))
    else Separators(None, None)

  /** Splits an unsigned typed number into its integer digits (grouping removed) and decimal digits, validating both. */
  private def splitNumber(body: String): Option[(String, String)] =
    val separators = separatorsOf(body)
    val decimalAt = separators.decimal.fold(body.length)(sep => body.lastIndexOf(sep.toInt))
    val integerRaw = body.take(decimalAt)
    val fraction = body.drop(decimalAt + 1)
    val decimalsOk = separators.decimal.forall(sep => body.count(_ == sep) == 1 && fraction.nonEmpty && fraction.forall(isDigit))
    val integerDigits = separators.group match
      case Some(sep) if integerRaw.contains(sep) => ungroup(splitOn(integerRaw, sep))
      case _ => Option.when(integerRaw.forall(isDigit))(integerRaw)
    integerDigits.filter(digits => decimalsOk && (digits.nonEmpty || fraction.nonEmpty)).map(_ -> fraction)

  /** Splits `text` at every `sep`, keeping empty pieces (`"1..2."` → `"1", "", "2", ""`). */
  private def splitOn(text: String, sep: Char): List[String] =
    text.foldRight(List("")) { (c, acc) =>
      if c == sep then "" :: acc
      else (c.toString + acc.head) :: acc.tail
    }

  /** `"1", "234", "567"` → `"1234567"`: a first group of 1 to 3 digits, then groups of exactly 3. */
  private def ungroup(groups: List[String]): Option[String] =
    groups match
      case first :: rest
          if first.nonEmpty && first.length <= 3 && first.forall(isDigit) && rest.nonEmpty && rest.forall(g => g.length == 3 && g.forall(isDigit)) =>
        Some(groups.mkString)
      case _ => None

  /**
   * Formats a canonical decimal string in Italian (`"1234.5600"` → `"1.234,56"`): `.` grouping, `,` decimals, rounded HALF_UP to `maxScale` decimals,
   * trailing zeros trimmed down to `minScale`. A string that is not a canonical decimal is returned unchanged.
   */
  def formatDecimal(value: String, minScale: Int, maxScale: Int): String =
    italian(value, minScale, maxScale, true)

  /** Like [[formatDecimal]] but without grouping, to prefill an input the user edits (`"1234.5000"` → `"1234,50"` with `minScale = 2`). */
  def decimalInputText(value: String, minScale: Int, maxScale: Int): String =
    italian(value, minScale, maxScale, false)

  private def italian(value: String, minScale: Int, maxScale: Int, grouped: Boolean): String =
    val trimmed = value.trim.nn
    if !canonicalPattern.matches(trimmed) then value
    else
      val rounded = BigDecimal(trimmed).setScale(maxScale, RoundingMode.HALF_UP)
      val plain = rounded.abs.bigDecimal.toPlainString.nn
      val integer = plain.takeWhile(_ != '.')
      val fraction = plain.drop(integer.length + 1).reverse.dropWhile(_ == '0').reverse.padTo(minScale, '0')
      val integerText = if grouped then integer.reverse.grouped(3).mkString(".").reverse else integer
      val sign = if rounded.signum < 0 then "-" else ""
      s"$sign$integerText${if fraction.isEmpty then "" else "," + fraction}"

  /** A money amount in euro with exactly 2 decimals: `"1234.56"` → `"1.234,56 €"`. */
  def formatEuro(value: String): String = s"${formatDecimal(value, 2, 2)} €"

  /** A unit price in euro with 2 to 4 decimals (trailing zeros past the cents trimmed): `"3.7000"` → `"3,70 €"`, `"0.1235"` → `"0,1235 €"`. */
  def formatUnitPrice(value: String): String = s"${formatDecimal(value, 2, 4)} €"

  /** A quantity with up to 3 decimals and the unit short label: `"2.500"`, `"linear_meters"` → `"2,5 m"`. */
  def formatQty(value: String, unitWireName: String): String = s"${formatDecimal(value, 0, 3)} ${unitShortLabel(unitWireName)}"
end WarehouseFormats
