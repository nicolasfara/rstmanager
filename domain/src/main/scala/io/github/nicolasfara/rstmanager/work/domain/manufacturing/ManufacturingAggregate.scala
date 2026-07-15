package io.github.nicolasfara.rstmanager.work.domain.manufacturing

import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingAggregate.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.ManufacturingError.*
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent
import io.github.nicolasfara.rstmanager.work.domain.manufacturing.events.ManufacturingEvent.*

import cats.data.ValidatedNec
import cats.syntax.all.*
import edomata.core.*
import edomata.syntax.all.*

/** Event-sourced aggregate holding one manufacturing catalog template. */
enum ManufacturingAggregate derives CanEqual:
  case Empty
  case Active(manufacturing: Manufacturing)
  case Deleted(manufacturing: Manufacturing)

  /** Creates the template from the empty state. */
  def create(manufacturing: Manufacturing): Decision[ManufacturingError, ManufacturingEvent, ManufacturingAggregate] = this.decide {
    case Empty => Decision.accept(ManufacturingCreated(manufacturing))
    case _ => Decision.reject(ManufacturingAlreadyExists)
  }.validate(_.mustBeActive)

  /** Replaces the template data when it is active. */
  def update(manufacturing: Manufacturing): Decision[ManufacturingError, ManufacturingEvent, ManufacturingAggregate] = this.decide {
    case Active(_) => Decision.accept(ManufacturingUpdated(manufacturing))
    case _ => Decision.reject(ManufacturingNotFound)
  }.validate(_.mustBeActive)

  /** Deletes the template when it is active. */
  def delete: Decision[ManufacturingError, ManufacturingEvent, ManufacturingAggregate] = this.decide {
    case Active(_) => Decision.accept(ManufacturingDeleted)
    case _ => Decision.reject(ManufacturingNotFound)
  }.validate(_.mustBeDeleted)

  private def mustBeActive: ValidatedNec[ManufacturingError, Active] = this match
    case active: Active => active.validNec
    case _ => ManufacturingNotFound.invalidNec

  private def mustBeDeleted: ValidatedNec[ManufacturingError, Deleted] = this match
    case deleted: Deleted => deleted.validNec
    case _ => ManufacturingNotFound.invalidNec
end ManufacturingAggregate

/** `DomainModel` instance for the event-sourced manufacturing catalog aggregate. */
object ManufacturingAggregate extends DomainModel[ManufacturingAggregate, ManufacturingEvent, ManufacturingError]:
  override def initial: ManufacturingAggregate = Empty

  override def transition: ManufacturingEvent => ManufacturingAggregate => ValidatedNec[ManufacturingError, ManufacturingAggregate] = {
    case ManufacturingCreated(manufacturing) => _ => Active(manufacturing).validNec
    case ManufacturingUpdated(manufacturing) => _.mustBeActive.map(_ => Active(manufacturing))
    case ManufacturingDeleted => _.mustBeActive.map(active => Deleted(active.manufacturing))
  }
