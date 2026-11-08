package io.gitbub.nicolasfara.rstmanager.ui

/**
 * Pure (DOM-free) multi-term text search shared by the searchable selects and the client-side filters: the query is split at blanks, and a text
 * matches when every term appears in it, case-insensitively. A blank query matches everything.
 */
object SearchTerms:

  /** The lower-case, non-empty terms of `query` (`" Tes  BLU "` → `List("tes", "blu")`). */
  def terms(query: String): List[String] =
    query.trim.nn.toLowerCase.nn.split("\\s+").nn.toList.map(_.nn).filter(_.nonEmpty)

  /** True when every term of `query` appears in at least one of `fields` (case-insensitive). */
  def matchesAny(query: String, fields: List[String]): Boolean =
    val haystack = fields.map(_.toLowerCase.nn)
    terms(query).forall(term => haystack.exists(_.contains(term)))

  /** True when every term of `query` appears in `text` (case-insensitive). */
  def matches(query: String, text: String): Boolean = matchesAny(query, List(text))
end SearchTerms
