package io.gitbub.nicolasfara.rstmanager.ui

import java.util.UUID

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ MaterialRequirementResponse, MaterialRequirementsResponse, MaterialShortageResponse }

/**
 * Pure (DOM-free) reading of the material requirements of the open orders for the planning board: which manufacturings are short of material (the
 * cart icon on their slices) and what has to be ordered per material (the "Materiali da ordinare" panel).
 */
object MaterialShortages:

  /** One material a manufacturing is short of: its code, unit (wire name, empty when unknown) and the missing quantity (canonical). */
  final case class SliceShortage(internalCode: String, unit: String, missing: String) derives CanEqual

  /** What has to be ordered of one material: the total missing quantity and the numbers of the orders involved (in allocation order). */
  final case class ToOrder(materialId: UUID, internalCode: String, description: String, unit: String, missing: String, orders: List[String])
      derives CanEqual

  private def canonical(value: BigDecimal): String = value.setScale(MaterialDrafts.quantityScale, RoundingMode.HALF_UP).bigDecimal.toPlainString.nn

  private def decimal(value: String): BigDecimal = Try(BigDecimal(value.trim.nn)).getOrElse(BigDecimal(0))

  private def materialsById(response: MaterialRequirementsResponse): Map[UUID, MaterialRequirementResponse] =
    response.materials.map(material => material.materialId -> material).toMap

  private def quantityText(value: String, unit: String): String =
    if unit.isEmpty then WarehouseFormats.formatDecimal(value, 0, MaterialDrafts.quantityScale) else WarehouseFormats.formatQty(value, unit)

  /** The shortages by `(orderId, manufacturingId)`, materials in the order the server lists them. */
  def bySlice(response: MaterialRequirementsResponse): Map[(UUID, UUID), List[SliceShortage]] =
    val materials = materialsById(response)
    response.shortages
      .groupBy(shortage => (shortage.orderId, shortage.manufacturingId))
      .view
      .mapValues { list =>
        list.map { shortage =>
          val material = materials.get(shortage.materialId)
          SliceShortage(material.fold(Formats.shortId(shortage.materialId))(_.internalCode), material.fold("")(_.unit), shortage.missing)
        }
      }
      .toMap

  /** Title / accessible label of the cart icon: "Materiale da ordinare: CODICE (mancano Z unità)", materials separated by `;`. */
  def sliceTitle(shortages: List[SliceShortage]): String =
    "Materiale da ordinare: " + shortages.map(s => s"${s.internalCode} (mancano ${quantityText(s.missing, s.unit)})").mkString("; ")

  /** What to order per material, by internal code: the missing quantities summed over its shortages, with the orders involved (each once). */
  def toOrder(response: MaterialRequirementsResponse): List[ToOrder] =
    val materials = materialsById(response)
    response.shortages
      .groupBy(_.materialId)
      .toList
      .map { (materialId, list) =>
        val material = materials.get(materialId)
        ToOrder(
          materialId,
          material.fold(Formats.shortId(materialId))(_.internalCode),
          material.fold("")(_.description),
          material.fold("")(_.unit),
          canonical(list.map(shortage => decimal(shortage.missing)).sum),
          ordersOf(list),
        )
      }
      .sortBy(_.internalCode)

  private def ordersOf(shortages: List[MaterialShortageResponse]): List[String] = shortages.map(_.orderNumber).distinct

  /** One line of the panel: "CODICE — Descrizione: mancano Z (ordini A, B)". */
  def toOrderText(entry: ToOrder): String =
    val name = if entry.description.isEmpty then entry.internalCode else s"${entry.internalCode} — ${entry.description}"
    val orders = if entry.orders.size == 1 then s"ordine ${entry.orders.mkString}" else s"ordini ${entry.orders.mkString(", ")}"
    s"$name: mancano ${quantityText(entry.missing, entry.unit)} ($orders)"

  /** The lines naming a material the warehouse does not know: "Materiale sconosciuto X: richiesti Y (ordine N, lavorazione C)". */
  def unknownTexts(response: MaterialRequirementsResponse): List[String] =
    response.unknownMaterials.map { demand =>
      s"Materiale non in magazzino ${Formats.shortId(demand.materialId)}: richiesti ${quantityText(demand.required, "")} " +
        s"(ordine ${demand.orderNumber}, lavorazione ${demand.manufacturingCode})"
    }
end MaterialShortages
