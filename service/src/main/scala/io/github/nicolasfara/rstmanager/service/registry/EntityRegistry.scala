package io.github.nicolasfara.rstmanager.service.registry

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.registry.EntityRegistryError.*
import io.github.nicolasfara.rstmanager.service.registry.EntityRegistryEvent.*

import cats.data.ValidatedNec
import cats.syntax.all.*
import edomata.core.*
import edomata.syntax.all.*

/** Errors raised while updating an [[EntityRegistry]]. */
enum EntityRegistryError derives CanEqual:
  case AlreadyRegistered(id: UUID)
  case NotRegistered(id: UUID)

/** Events recording membership changes of an [[EntityRegistry]]. */
enum EntityRegistryEvent derives CanEqual:
  case Registered(id: UUID)
  case Deregistered(id: UUID)

/**
 * Durable event-sourced index of the identifiers that exist for one entity type.
 *
 * A single instance of this aggregate lives per entity type (one stream namespace each) and powers the collection `GET` endpoints, since edomata
 * reads one aggregate at a time and has no native "list all streams" query.
 */
final case class EntityRegistry(ids: Set[UUID]) derives CanEqual:
  /** Records that `id` now exists. */
  def register(id: UUID): Decision[EntityRegistryError, EntityRegistryEvent, EntityRegistry] = this.decide { current =>
    if current.ids.contains(id) then Decision.reject(AlreadyRegistered(id)) else Decision.accept(Registered(id))
  }

  /** Records that `id` no longer exists. */
  def deregister(id: UUID): Decision[EntityRegistryError, EntityRegistryEvent, EntityRegistry] = this.decide { current =>
    if current.ids.contains(id) then Decision.accept(Deregistered(id)) else Decision.reject(NotRegistered(id))
  }

/** `DomainModel` instance for the event-sourced [[EntityRegistry]]. */
object EntityRegistry extends DomainModel[EntityRegistry, EntityRegistryEvent, EntityRegistryError]:
  override def initial: EntityRegistry = EntityRegistry(Set.empty)

  override def transition: EntityRegistryEvent => EntityRegistry => ValidatedNec[EntityRegistryError, EntityRegistry] = {
    case Registered(id) => registry => registry.copy(ids = registry.ids + id).validNec
    case Deregistered(id) => registry => registry.copy(ids = registry.ids - id).validNec
  }
