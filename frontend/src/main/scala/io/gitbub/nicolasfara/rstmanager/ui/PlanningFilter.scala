package io.gitbub.nicolasfara.rstmanager.ui

/**
 * Filters of the planning board: by assigned employee, catalog task and manufacturing template (by code). Each value is the select value of its
 * control; an empty value matches any slice, and the filters combine in AND.
 */
final case class PlanningFilter(employeeId: String, taskId: String, manufacturingCode: String) derives CanEqual:

  def isEmpty: Boolean = employeeId.isEmpty && taskId.isEmpty && manufacturingCode.isEmpty

  def matches(keys: PlanningFilter.Keys): Boolean =
    (employeeId.isEmpty || employeeId == keys.employeeId) &&
      (taskId.isEmpty || taskId == keys.taskId) &&
      (manufacturingCode.isEmpty || manufacturingCode == keys.manufacturingCode)

object PlanningFilter:

  val empty: PlanningFilter = PlanningFilter("", "", "")

  /** What a planned slice is filtered on, resolved from its scheduled ids (empty when not resolvable yet). */
  final case class Keys(employeeId: String, taskId: String, manufacturingCode: String)

  /**
   * Select options for one filter: "any" first, then the distinct `values` found in the plan sorted by label. A `selected` value that is no longer in
   * the plan stays listed, so the control keeps showing the active filter instead of silently falling back to "any".
   */
  def options(anyLabel: String, values: List[String], selected: String, label: String => String): List[(String, String)] =
    val listed = (if selected.isEmpty then values else selected :: values).filter(_.nonEmpty).distinct
    ("" -> anyLabel) :: listed.map(value => value -> label(value)).sortBy(_._2.toLowerCase.nn)
end PlanningFilter
