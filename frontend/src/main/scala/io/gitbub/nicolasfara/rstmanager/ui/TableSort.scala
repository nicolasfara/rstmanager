package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.ui.Sorting.{ Column, Direction, SortBy }

/**
 * The column sort of one list table (see [[Sorting]]): sortable headers, a compact picker for the mobile card views, and the sorted rows. Create it
 * next to the list's [[Pagination]], outside `renderResult`, so the sort survives the in-place reloads; changing the sort runs `onSort` (for a
 * paginated list, back to the first page). `columns` maps each sortable column key to its order, and is a signal when the order depends on other
 * loaded data (e.g. customer names).
 */
final class TableSort[A](columns: Signal[Map[String, Column[A]]], onSort: () => Unit):
  val current: Var[Option[SortBy]] = Var(None)

  /** `rows` in the current sort. */
  def sorted(rows: Signal[List[A]]): Signal[List[A]] = rows.combineWith(current.signal, columns).map(Sorting.sort)

  /** Variant of [[sorted]] for a list already loaded. */
  def sorted(rows: List[A]): Signal[List[A]] = sorted(Signal.fromValue(rows))

  private def choose(next: Option[SortBy]): Unit =
    current.set(next)
    onSort()

  /** A header cell (classes `thCls`) whose label sorts the table by `column` on click, showing the sort direction. */
  def header(column: String, text: String, thCls: String): HtmlElement = th(cls := thCls, ariaSort(Set(column)), sortButton(column, text))

  /**
   * A header cell (classes `thCls`) of a column that merges several sortable values (e.g. code and description in one "Materiale" cell): one sort
   * button per `(column, label)`, separated by " / ". The cell's `aria-sort` follows whichever of them the table is sorted by.
   */
  def header(thCls: String, columns: (String, String)*): HtmlElement =
    th(
      cls := thCls,
      ariaSort(columns.map(_._1).toSet),
      div(
        cls := "inline-flex flex-wrap items-center gap-x-1",
        columns.toList.zipWithIndex.map { case ((column, text), index) =>
          span(
            cls := "inline-flex items-center gap-1",
            Option.when(index > 0)(span(aria.hidden := true, cls := "text-slate-300", "/")),
            sortButton(column, text),
          )
        },
      ),
    )
  end header

  /** The `aria-sort` of a header cell sorting by any of `keys`. */
  private def ariaSort(keys: Set[String]): Modifier[HtmlElement] =
    aria.sort <-- current.signal.map {
      case Some(SortBy(column, Direction.Ascending)) if keys.contains(column) => "ascending"
      case Some(SortBy(column, Direction.Descending)) if keys.contains(column) => "descending"
      case _ => "none"
    }

  private def sortButton(column: String, text: String): HtmlElement =
    val direction = current.signal.map(_.collect { case SortBy(`column`, d) => d })
    button(
      tpe := "button",
      cls := "group inline-flex items-center gap-1 uppercase tracking-wide hover:text-slate-800",
      title := s"Ordina per ${text.toLowerCase}",
      text,
      span(
        aria.hidden := true,
        cls <-- direction.map(d => if d.isDefined then "text-slate-700" else "text-slate-300 group-hover:text-slate-400"),
        child.text <-- direction.map {
          case Some(Direction.Ascending) => "↑"
          case Some(Direction.Descending) => "↓"
          case None => "↕"
        },
      ),
      onClick --> (_ => choose(Sorting.toggle(current.now(), column))),
    )
  end sortButton

  /** The sort picker of the mobile card views, where the table headers are hidden: one entry per direction of each `(column, label)`. */
  def picker(labels: List[(String, String)]): HtmlElement =
    def key(sortBy: SortBy): String = s"${sortBy.column}:${if sortBy.direction == Direction.Ascending then "asc" else "desc"}"
    val entries = labels.flatMap { (column, text) =>
      List(SortBy(column, Direction.Ascending) -> s"$text ↑", SortBy(column, Direction.Descending) -> s"$text ↓")
    }
    label(
      cls := "flex items-center gap-2 text-xs text-slate-500",
      "Ordina",
      select(
        // 40px touch target: the picker only shows on the cards, below the breakpoint of the table headers.
        cls := "min-h-10 min-w-0 flex-1 rounded-md border border-slate-300 px-2 py-1 text-xs text-slate-700 focus:border-slate-500 focus:outline-none",
        controlled(
          value <-- current.signal.map(_.fold("")(key)),
          onChange.mapToValue --> (picked => choose(entries.collectFirst { case (sortBy, _) if key(sortBy) == picked => sortBy })),
        ),
        option(value := "", "Predefinito"),
        entries.map((sortBy, text) => option(value := key(sortBy), text)),
      ),
    )
  end picker
end TableSort

object TableSort:

  /** The sort of a paginated list whose column orders don't depend on other loaded data. */
  def apply[A](pager: Pagination)(columns: (String, Column[A])*): TableSort[A] = apply(Signal.fromValue(columns.toMap), pager)

  /** The sort of a paginated list whose column orders depend on other loaded data. */
  def apply[A](columns: Signal[Map[String, Column[A]]], pager: Pagination): TableSort[A] = new TableSort(columns, () => pager.reset())

  /** The sort of a list shown whole, without a pager. */
  def unpaged[A](columns: (String, Column[A])*): TableSort[A] = new TableSort(Signal.fromValue(columns.toMap), () => ())
