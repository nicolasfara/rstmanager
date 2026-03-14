package io.github.nicolasfara.rstmanager.customer.domain

/** Classifies whether a customer is a private individual or a company. */
enum CustomerType derives CanEqual:
  case Individual, Company
