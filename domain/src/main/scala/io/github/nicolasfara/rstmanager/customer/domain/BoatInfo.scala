package io.github.nicolasfara.rstmanager.customer.domain

/**
 * Optional boat details attached to a customer record.
 *
 * @param model
 *   Boat model.
 * @param name
 *   Boat name.
 * @param berth
 *   Berth assigned to the boat.
 * @param port
 *   Port where the boat is moored.
 */
final case class BoatInfo(
    model: Option[String],
    name: Option[String],
    berth: Option[String],
    port: Option[String],
) derives CanEqual

object BoatInfo:
  /** Boat details with every field unset. */
  val empty: BoatInfo = BoatInfo(None, None, None, None)

  /** Builds `BoatInfo` from raw optional values, discarding blank strings. */
  def createBoatInfo(model: Option[String], name: Option[String], berth: Option[String], port: Option[String]): BoatInfo =
    BoatInfo(normalized(model), normalized(name), normalized(berth), normalized(port))

  private def normalized(value: Option[String]): Option[String] = value.map(_.trim.nn).filter(_.nonEmpty)
