package io.gitbub.nicolasfara.rstmanager.ui

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.ui.Components.btnSmall

/**
 * The client-side pagination state of one list (see [[Paging]]) and its pager bar. Create it next to the list's `loadable`, outside `renderResult`,
 * so the page and the page size survive the in-place reloads.
 */
final class Pagination(initialSize: Int):
  val page: Var[Int] = Var(0)
  val size: Var[Int] = Var(initialSize)

  /** Back to the first page, e.g. when the filter of the list changes. */
  def reset(): Unit = page.set(0)

  /** The current page of `items`. */
  def slice[A](items: Signal[List[A]]): Signal[Paging.PageSlice[A]] =
    items.combineWith(page.signal, size.signal).map(Paging.slice)

  /** Renders the current page of `items` with `render`, followed by the pager bar. */
  def view[A](items: Signal[List[A]])(render: List[A] => HtmlElement): HtmlElement =
    val current = slice(items)
    div(child <-- current.map(shown => render(shown.items)), controls(current))

  /** Variant of [[view]] for a list already loaded. */
  def view[A](items: List[A])(render: List[A] => HtmlElement): HtmlElement = view(Signal.fromValue(items))(render)

  /**
   * The pager bar: shown rows, page size, previous/next. Hidden while the whole list fits in the smallest page, so short lists stay uncluttered. The
   * arrows move from the page actually shown, which may differ from `page` after clamping.
   */
  def controls(current: Signal[Paging.PageSlice[?]]): HtmlElement =
    div(
      child <-- current.map { shown =>
        if shown.total <= Paging.pageSizes.min then emptyNode
        else
          div(
            cls := "flex flex-wrap items-center justify-between gap-2 border-t border-slate-100 px-4 py-2 text-xs text-slate-500",
            span(s"Mostrati ${shown.from}–${shown.to} di ${shown.total}"),
            div(
              cls := "flex items-center gap-2",
              label(
                cls := "flex items-center gap-1.5",
                "Righe per pagina",
                select(
                  cls := "rounded-md border border-slate-300 px-1.5 py-0.5 text-xs text-slate-700 focus:border-slate-500 focus:outline-none",
                  controlled(
                    value <-- size.signal.map(_.toString),
                    onChange.mapToValue --> { picked => picked.toIntOption.foreach { n => size.set(n); reset() } },
                  ),
                  Paging.pageSizes.map(n => option(value := n.toString, n.toString)),
                ),
              ),
              button(
                tpe := "button",
                cls := s"$btnSmall disabled:opacity-40",
                aria.label := "Pagina precedente",
                "‹",
                disabled := !shown.hasPrevious,
                onClick --> (_ => page.set(shown.page - 1)),
              ),
              span(s"Pagina ${shown.page + 1} di ${shown.pageCount}"),
              button(
                tpe := "button",
                cls := s"$btnSmall disabled:opacity-40",
                aria.label := "Pagina successiva",
                "›",
                disabled := !shown.hasNext,
                onClick --> (_ => page.set(shown.page + 1)),
              ),
            ),
          )
      },
    )
  end controls
end Pagination
