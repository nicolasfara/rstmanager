package io.github.nicolasfara.rstmanager.work.domain.order

/** Priority assigned to an order for planning and execution purposes. */
enum OrderPriority derives CanEqual:
  case Normal
  case Urgent
