package io.github.nicolasfara.rstmanager.work.domain.order

/** Errors that can occur during Order aggregate operations */
enum OrderError derives CanEqual:
  case OrderAlreadyCreated
  
  case InvalidTransition(cause: String)

