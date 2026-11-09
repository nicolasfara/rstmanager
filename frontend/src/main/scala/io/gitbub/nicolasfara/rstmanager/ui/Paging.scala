package io.gitbub.nicolasfara.rstmanager.ui

/**
 * Pure (DOM-free) client-side pagination shared by the list pages: a loaded (and possibly filtered) list is cut into pages of `size` rows. The
 * requested page is clamped into range, so a filter that shrinks the list never leaves the user on an empty page past the last one.
 */
object Paging:

  /** The page sizes offered by the pager. */
  val pageSizes: List[Int] = List(10, 25, 50, 100)

  /** The page size a list starts with. */
  val defaultPageSize: Int = 25

  /**
   * One page of a list: its `items`, the 0-based `page` actually shown (after clamping), how many pages there are (at least 1), the `total` number of
   * rows, and the 1-based positions `from`–`to` of the shown rows (both 0 when the list is empty).
   */
  final case class PageSlice[A](items: List[A], page: Int, pageCount: Int, total: Int, from: Int, to: Int) derives CanEqual:
    def hasPrevious: Boolean = page > 0
    def hasNext: Boolean = page < pageCount - 1

  /** The `page`-th (0-based) page of `all`, with `size` rows per page (at least 1). */
  def slice[A](all: List[A], page: Int, size: Int): PageSlice[A] =
    val perPage = size.max(1)
    val total = all.size
    val pageCount = ((total + perPage - 1) / perPage).max(1)
    val shown = page.max(0).min(pageCount - 1)
    val items = all.slice(shown * perPage, (shown + 1) * perPage)
    val from = if items.isEmpty then 0 else shown * perPage + 1
    val to = if items.isEmpty then 0 else shown * perPage + items.size
    PageSlice(items, shown, pageCount, total, from, to)
end Paging
