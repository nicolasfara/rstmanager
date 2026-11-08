package io.github.nicolasfara.rstmanager.warehouse.domain

import java.util.Locale

/**
 * Unit in which a material is stocked, bought, and priced. Quantities are always thousandths of it ([[Quantity]]).
 *
 * @param wireName
 *   Stable name used by the API and the journal; never rename it.
 */
enum UnitOfMeasure(val wireName: String) derives CanEqual:
  /** Whole pieces: quantities must be whole numbers. */
  case Pieces extends UnitOfMeasure("pieces")

  /** Linear metres. */
  case LinearMeters extends UnitOfMeasure("linear_meters")

  /** Square metres. */
  case SquareMeters extends UnitOfMeasure("square_meters")

  /** Kilograms. */
  case Kilograms extends UnitOfMeasure("kilograms")

  /** Whether quantities in this unit may have decimals. */
  def allowsFractions: Boolean = this match
    case Pieces => false
    case LinearMeters | SquareMeters | Kilograms => true
end UnitOfMeasure

object UnitOfMeasure:
  /** Reads a unit from its [[UnitOfMeasure.wireName]] (case and surrounding blanks ignored). */
  def fromWireName(raw: String): Either[String, UnitOfMeasure] =
    val normalized = raw.trim.nn.toLowerCase(Locale.ROOT).nn
    values
      .find(_.wireName == normalized)
      .toRight(s"L'unità di misura '$raw' non è supportata. Usa ${values.map(_.wireName).mkString(", ")}.")
