package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ FlowBucketResponse, MovementResponse, StateRowResponse }
import io.gitbub.nicolasfara.rstmanager.ui.{ Formats, WarehouseFormats }

/**
 * Pure (DOM-free) derivations behind the warehouse history ("Storico") and flows ("Entrate/Uscite") tabs and the movements list: report periods, day
 * navigation, bucket labels, bar scaling, and row filtering. The pages only render what these return.
 */
object WarehouseViews:

  // ---- Report periods ----------------------------------------------------------------------------

  /** Bucket length of the flows report, mirroring the backend wire names. */
  enum Granularity(val wireName: String, val label: String) derives CanEqual:
    case Week extends Granularity("week", "Settimana")
    case Month extends Granularity("month", "Mese")

  /** An inclusive period of `YYYY-MM-DD` days. */
  final case class Period(from: String, to: String) derives CanEqual

  /** Weeks shown by default: the current ISO week and the 11 before it. */
  val defaultWeeks: Int = 12

  /** Months shown by default: the current month and the 5 before it. */
  val defaultMonths: Int = 6

  /** Longest period the server accepts for a report (`to` must come before `from` + 5 years). */
  val maxReportYears: Int = 5

  /**
   * The default period of the flows report ending `today`: from the Monday 11 weeks before the current one, or from the 1st of the month 5 months
   * back, so the chart shows whole buckets. An invalid `today` yields a one-day period on it, which the period check then reports.
   */
  def defaultPeriod(granularity: Granularity, today: String): Period =
    IsoDay.parse(today).fold(Period(today, today)) { day =>
      val start = granularity match
        case Granularity.Week => IsoDay.plusDays(IsoDay.mondayOf(day), -7 * (defaultWeeks - 1))
        case Granularity.Month => IsoDay.plusMonths(IsoDay.firstOfMonth(day), -(defaultMonths - 1))
      Period(IsoDay.format(start), IsoDay.format(day))
    }

  /**
   * The period after switching the granularity from `previous` to `next`: an untouched default period follows the new granularity, a period the user
   * chose is kept.
   */
  def periodOnToggle(current: Period, previous: Granularity, next: Granularity, today: String): Period =
    if current == defaultPeriod(previous, today) then defaultPeriod(next, today) else current

  /** Checks a report period like the server does; `Right` holds the normalized days. */
  def validatePeriod(from: String, to: String): Either[String, Period] =
    (IsoDay.parse(from), IsoDay.parse(to)) match
      case (None, _) => Left("Data di inizio non valida")
      case (_, None) => Left("Data di fine non valida")
      case (Some(start), Some(end)) if IsoDay.compare(start, end) > 0 => Left("La data di inizio è successiva alla data di fine")
      case (Some(start), Some(end)) if IsoDay.compare(end, IsoDay.plusMonths(start, 12 * maxReportYears)) >= 0 =>
        Left(s"Il periodo non può superare $maxReportYears anni")
      case (Some(start), Some(end)) => Right(Period(IsoDay.format(start), IsoDay.format(end)))

  /** Optional filters of the movements list: blank means unbounded; `Right` holds the normalized bounds. */
  def validateOptionalRange(from: String, to: String): Either[String, (Option[String], Option[String])] =
    def bound(text: String, error: String): Either[String, Option[IsoDay.Ymd]] =
      if text.trim.nn.isEmpty then Right(None) else IsoDay.parse(text).map(Some(_)).toRight(error)
    for
      start <- bound(from, "Data di inizio non valida")
      end <- bound(to, "Data di fine non valida")
      _ <- (start, end) match
        case (Some(s), Some(e)) if IsoDay.compare(s, e) > 0 => Left("La data di inizio è successiva alla data di fine")
        case _ => Right(())
    yield (start.map(IsoDay.format), end.map(IsoDay.format))

  // ---- Day navigation (Storico) ------------------------------------------------------------------

  /** The day whose state to load: a valid day not after `today`, or the message to show instead. */
  def stateDay(selected: String, today: String): Either[String, String] =
    (IsoDay.parse(selected), IsoDay.parse(today)) match
      case (None, _) => Left("Seleziona una data valida")
      case (Some(day), Some(now)) if IsoDay.compare(day, now) > 0 => Left("La data non può essere successiva a oggi")
      case (Some(day), _) => Right(IsoDay.format(day))

  /** The day before `selected` (unchanged when `selected` is not a valid day). */
  def previousDay(selected: String): String = IsoDay.shift(selected, -1).getOrElse(selected)

  /** The day after `selected`, or `None` when that would pass `today` (or `selected` is not a valid day). */
  def nextDay(selected: String, today: String): Option[String] =
    for
      next <- IsoDay.shift(selected, 1)
      nextDay <- IsoDay.parse(next)
      now <- IsoDay.parse(today)
      if IsoDay.compare(nextDay, now) <= 0
    yield next

  // ---- Bucket labels -----------------------------------------------------------------------------

  private val monthShortNames = Vector("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic")

  private val monthLabel = "^([0-9]{4})-([0-9]{2})$".r
  private val weekLabel = "^[0-9]{4}-W([0-9]{2})$".r

  /**
   * The Italian name of a flows bucket from the server label and start day: `"2026-10"` → `"Ott 2026"`, `"2026-W43"` starting `"2026-10-19"` →
   * `"Sett. 43 (19/10)"`. A label of another shape is shown as is.
   */
  def bucketLabel(label: String, start: String): String =
    label.trim.nn match
      case monthLabel(year, month) =>
        monthShortNames.lift(month.nn.toInt - 1).fold(label)(name => s"$name $year")
      case weekLabel(week) =>
        val startText = IsoDay.parse(start).fold("")(day => f" (${day.day}%02d/${day.month}%02d)")
        s"Sett. ${week.nn.toInt}$startText"
      case _ => label

  def bucketLabel(bucket: FlowBucketResponse): String = bucketLabel(bucket.label, bucket.start)

  /** The days a bucket counts, as `DD/MM – DD/MM` (one day: `DD/MM`); text that is not a valid day is shown as is. */
  def bucketRange(from: String, to: String): String =
    def short(text: String): String = IsoDay.parse(text).fold(text)(day => f"${day.day}%02d/${day.month}%02d")
    if from == to then short(from) else s"${short(from)} – ${short(to)}"

  // ---- Bars --------------------------------------------------------------------------------------

  /** Lengths of the two bars of a bucket, in percent of the largest value of the chart. */
  final case class BarPair(inPercent: Double, outPercent: Double) derives CanEqual

  /** Smallest length of a bar with a non-zero value, so it stays visible next to much larger ones. */
  val minVisiblePercent: Double = 1.0

  /** The length a value is drawn with: a negative value (e.g. a bucket whose consumption reversals exceed its outflows) draws no bar. */
  private def magnitude(value: String): BigDecimal = Try(BigDecimal(value.trim.nn)).toOption.fold(BigDecimal(0))(_.max(BigDecimal(0)))

  /**
   * Scales `(inValue, outValue)` amounts to bar lengths relative to the largest of all of them (100%). A zero or negative value gets no bar (the
   * amount is still shown in the tooltip and the table), any positive one at least [[minVisiblePercent]]; when no value is positive all bars are
   * empty. Text that is not a decimal counts as zero.
   */
  def scaleBars(values: List[(String, String)]): List[BarPair] =
    val parsed = values.map((in, out) => (magnitude(in), magnitude(out)))
    val max = parsed.flatMap((in, out) => List(in, out)).maxOption.getOrElse(BigDecimal(0))
    def percent(value: BigDecimal): Double =
      if max.signum == 0 || value.signum == 0 then 0.0 else (value * 100 / max).toDouble.max(minVisiblePercent)
    parsed.map((in, out) => BarPair(percent(in), percent(out)))

  // ---- Storico rows ------------------------------------------------------------------------------

  /**
   * The rows matching a free-text search: every blank-separated term must appear (case-insensitive) in the internal code, the description, or the
   * product type. A blank query keeps every row.
   */
  def filterRows(rows: List[StateRowResponse], query: String): List[StateRowResponse] =
    val terms = query.trim.nn.toLowerCase.nn.split("\\s+").nn.toList.map(_.nn).filter(_.nonEmpty)
    if terms.isEmpty then rows
    else
      rows.filter { row =>
        val haystack = List(row.internalCode, row.description, row.productType.getOrElse("")).map(_.toLowerCase.nn)
        terms.forall(term => haystack.exists(_.contains(term)))
      }

  // ---- Movements ---------------------------------------------------------------------------------

  /** Italian label of a movement kind wire name; an unknown kind is shown as is. */
  def movementKindLabel(kind: String): String = kind match
    case "purchase" => "Carico"
    case "unload" => "Scarico"
    case "adjustment" => "Rettifica"
    case "consumption" => "Scarico lavorazione"
    case other => other

  /** Like [[movementKindLabel]], telling a consumption reversal (positive `delta`: the manufacturing was reopened) from the consumption itself. */
  def movementKindLabel(kind: String, delta: String): String =
    if kind == "consumption" && signOf(delta) > 0 then "Storno lavorazione" else movementKindLabel(kind)

  def movementKindLabel(movement: MovementResponse): String = movementKindLabel(movement.kind, movement.delta)

  /** True for an automatic consumption by an order manufacturing: it follows the order, so it cannot be voided by hand. */
  def isConsumption(movement: MovementResponse): Boolean = movement.kind == "consumption"

  /**
   * Where a consumption comes from: `Ordine <number> – <manufacturing code>`, falling back to the short ids when the order or the manufacturing can
   * no longer be resolved; `None` for the other kinds.
   */
  def consumptionSource(movement: MovementResponse): Option[String] =
    Option.when(isConsumption(movement)) {
      val order = movement.orderNumber.orElse(movement.orderId.map(Formats.shortId)).getOrElse("?")
      movement.manufacturingCode.orElse(movement.manufacturingId.map(Formats.shortId)).fold(s"Ordine $order")(code => s"Ordine $order – $code")
    }

  /** Sign of a canonical decimal (0 for text that is not one). */
  def signOf(value: String): Int = Try(BigDecimal(value.trim.nn).signum).getOrElse(0)

  /** A signed quantity with an explicit `+` when positive: `"2.500"`, `"linear_meters"` → `"+2,5 m"`, `"-1.000"`, `"pieces"` → `"-1 pz"`. */
  def signedQty(value: String, unit: String): String =
    val text = WarehouseFormats.formatQty(value, unit)
    if signOf(value) > 0 then s"+$text" else text
end WarehouseViews
