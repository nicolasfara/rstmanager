package io.gitbub.nicolasfara.rstmanager.ui

/**
 * Pure (DOM-free) column sorting of the list tables: a click on a column header sorts the loaded rows by that column, ascending first, then
 * descending, then back to the order of the server. Rows without a value in the sorted column always go last, whatever the direction.
 */
object Sorting:

  enum Direction derives CanEqual:
    case Ascending, Descending

  /** The sorted `column` (its key in the table's column map) and the `direction`. */
  final case class SortBy(column: String, direction: Direction) derives CanEqual

  /** How a column orders the rows: `compare` orders two rows that both have a value; `hasValue` tells the rows without one apart. */
  final class Column[A](val compare: (A, A) => Int, val hasValue: A => Boolean)

  object Column:

    /** Orders the rows by `key`, which every row has. */
    def by[A, K](key: A => K)(using ord: Ordering[K]): Column[A] = Column((x, y) => ord.compare(key(x), key(y)), _ => true)

    /** Orders the rows by `key`; the rows where it is `None` go last. */
    def optional[A, K](key: A => Option[K])(using ord: Ordering[K]): Column[A] =
      Column(
        (x, y) =>
          (key(x), key(y)) match
            case (Some(a), Some(b)) => ord.compare(a, b)
            case _ => 0,
        key(_).isDefined,
      )

    /** Orders the rows by the text `key` (see [[naturalText]]); blank texts go last. */
    def text[A](key: A => String): Column[A] = optionalText(row => Some(key(row)))

    /** Orders the rows by the text `key` (see [[naturalText]]); missing or blank texts go last. */
    def optionalText[A](key: A => Option[String]): Column[A] =
      optional[A, String]((row: A) => key(row).map(_.trim.nn).filter(_.nonEmpty))(using naturalText)

    /** Orders the rows by the decimal `key` (an API amount such as `"18.5000"`, possibly signed); unparsable amounts go last. */
    def decimal[A](key: A => String): Column[A] = optionalDecimal(row => Some(key(row)))

    /** Orders the rows by the decimal `key`; missing or unparsable amounts go last. */
    def optionalDecimal[A](key: A => Option[String]): Column[A] = optional[A, Double]((row: A) => key(row).flatMap(_.trim.nn.toDoubleOption))
  end Column

  /**
   * Case-insensitive text order where runs of digits compare as numbers, so codes sort as people expect (`"ORD-9"` before `"ORD-10"`). ISO dates keep
   * their chronological order.
   */
  val naturalText: Ordering[String] = new Ordering[String]:
    def compare(x: String, y: String): Int =
      val byChunks = compareChunks(chunks(x), chunks(y))
      if byChunks != 0 then byChunks else x.compareTo(y)

  private val chunkPattern = "\\d+|\\D+".r

  private def chunks(text: String): List[String] = chunkPattern.findAllIn(text.toLowerCase.nn).toList

  private def compareChunks(xs: List[String], ys: List[String]): Int = (xs, ys) match
    case (x :: xt, y :: yt) =>
      val here = if x.head.isDigit && y.head.isDigit then compareDigits(x, y) else x.compareTo(y)
      if here != 0 then here else compareChunks(xt, yt)
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => 1

  /** Compares two runs of digits by numeric value, without overflowing on long runs. */
  private def compareDigits(x: String, y: String): Int =
    val a = x.dropWhile(_ == '0')
    val b = y.dropWhile(_ == '0')
    if a.length != b.length then a.length.compareTo(b.length) else a.compareTo(b)

  /** The next sort after a click on the header of `column`: a new column starts ascending; the same one goes descending, then back to unsorted. */
  def toggle(current: Option[SortBy], column: String): Option[SortBy] = current match
    case Some(SortBy(`column`, Direction.Ascending)) => Some(SortBy(column, Direction.Descending))
    case Some(SortBy(`column`, Direction.Descending)) => None
    case _ => Some(SortBy(column, Direction.Ascending))

  /** `rows` sorted by `current` (stable: ties keep their order); unchanged when unsorted or when the column is not in `columns`. */
  def sort[A](rows: List[A], current: Option[SortBy], columns: Map[String, Column[A]]): List[A] =
    current.flatMap(sortBy => columns.get(sortBy.column).map(sortBy.direction -> _)) match
      case None => rows
      case Some((direction, column)) =>
        val (valued, missing) = rows.partition(column.hasValue)
        val ordering = new Ordering[A]:
          def compare(x: A, y: A): Int = if direction == Direction.Ascending then column.compare(x, y) else column.compare(y, x)
        valued.sorted(using ordering) ++ missing
end Sorting
