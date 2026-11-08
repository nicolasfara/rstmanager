package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.{
  DeliveryNoteLineRequest,
  DeliveryNoteRequest,
  DeliveryNoteResponse,
  MaterialResponse,
  SupplierResponse,
}
import io.gitbub.nicolasfara.rstmanager.ui.WarehouseFormats

/**
 * Pure (DOM-free) state of the delivery note ("DDT") form: the header (day, number, supplier, note) and the lines, each a material bought under an
 * MPN of the selected supplier, with quantity and unit cost; validation messages, totals, and request building.
 *
 * What a line may pick depends on the supplier: one option per MPN of that supplier, plus a "nuovo MPN" option for each active material that has no
 * MPN of it yet (the code is then typed in the line and the server adds the MPN). Choosing an option prefills the unit cost with the expected price
 * of the MPN, unless the user already typed one; changing the supplier moves every line onto the MPNs of the new supplier for the same material.
 *
 * The page keeps one [[State]] in a `Var` and passes the [[Context]] of the loaded materials to the transformations below, so every rule is
 * unit-testable without Laminar. The server re-checks everything (and the stock, which needs the full ledger).
 */
object DeliveryNoteForm:

  /** Decimals of a unit cost on the API (ten-thousandths of a euro). */
  val costScale: Int = MovementForm.costScale

  // ---- Line options ------------------------------------------------------------------------------

  /**
   * What a line can be: `materialId` bought under `mpnCode` of the selected supplier, or — `mpnCode` absent — under a new MPN typed in the line.
   * `label` is shown in the select, `searchText` (internal code, description, and every MPN code of the material) is what the search matches.
   */
  final case class LineOption(
      key: String,
      materialId: UUID,
      internalCode: String,
      unit: String,
      mpnCode: Option[String],
      expectedUnitCost: Option[String],
      label: String,
      searchText: String,
  ) derives CanEqual:
    def isNewMpn: Boolean = mpnCode.isEmpty

  /** The select key of `materialId` bought under its existing MPN `code`. */
  def existingKey(materialId: UUID, code: String): String = s"$materialId|mpn|$code"

  /** The select key of `materialId` bought under a new MPN of the selected supplier. */
  def newMpnKey(materialId: UUID): String = s"$materialId|new"

  private def materialIdOf(key: String): Option[UUID] = parseUuid(key.takeWhile(_ != '|'))

  /** The options of `material` for `supplierId`: one per MPN of that supplier, else (active materials only) one "nuovo MPN". */
  def optionsOf(material: MaterialResponse, supplierId: UUID): List[LineOption] =
    val archivedSuffix = if material.archived then " (archiviato)" else ""
    val base = s"${material.internalCode} — ${material.description}"
    val searchText = (material.internalCode :: material.description :: material.mpns.map(_.code)).mkString(" ")
    val own = material.mpns.filter(_.supplierId == supplierId)
    if own.isEmpty then
      Option
        .when(!material.archived)(
          LineOption(newMpnKey(material.id), material.id, material.internalCode, material.unit, None, None, s"$base · nuovo MPN", searchText),
        )
        .toList
    else
      own.map { mpn =>
        LineOption(
          existingKey(material.id, mpn.code),
          material.id,
          material.internalCode,
          material.unit,
          Some(mpn.code),
          mpn.expectedUnitCost,
          s"$base · MPN ${mpn.code}$archivedSuffix",
          searchText,
        )
      }
    end if
  end optionsOf

  /** The materials the lines can name, sorted by internal code. */
  final case class Context(materials: List[MaterialResponse]):
    private val byId: Map[UUID, MaterialResponse] = materials.map(material => material.id -> material).toMap

    def material(id: UUID): Option[MaterialResponse] = byId.get(id)

    /** Every option for the supplier `supplierId` (none while no supplier is selected). */
    def options(supplierId: String): List[LineOption] = parseUuid(supplierId).fold(Nil)(supplier => materials.flatMap(optionsOf(_, supplier)))

    /** The option `key` for the supplier `supplierId`, when it is one of its options. */
    def option(supplierId: String, key: String): Option[LineOption] =
      for
        supplier <- parseUuid(supplierId)
        material <- materialIdOf(key).flatMap(material)
        found <- optionsOf(material, supplier).find(_.key == key)
      yield found

    /** The `(value, label, searchText)` triples of the line select. */
    def selectOptions(supplierId: String): List[(String, String, String)] = options(supplierId).map(o => (o.key, o.label, o.searchText))
  end Context

  object Context:
    val empty: Context = Context(Nil)

    /** The active `materials`, plus the archived ones in `kept` (the materials of the delivery note being edited), sorted by internal code. */
    def of(materials: List[MaterialResponse], kept: Set[UUID]): Context =
      Context(materials.filter(material => !material.archived || kept.contains(material.id)).sortBy(_.internalCode))

  // ---- Rows and state ----------------------------------------------------------------------------

  /**
   * One editable line: `key` keeps the row identity across edits, `choice` the selected [[LineOption]] key (`""` = none yet), `newMpnCode` the code
   * typed for a new MPN, `quantity`/`unitCost` the typed text. `costTouched` is true once the user typed the cost: choosing an option then keeps it.
   */
  final case class Row(key: Int, choice: String, newMpnCode: String, quantity: String, unitCost: String, costTouched: Boolean) derives CanEqual:
    /** A row nothing was entered in: it is ignored (neither validated nor sent). */
    def isBlank: Boolean = choice.trim.nn.isEmpty && newMpnCode.trim.nn.isEmpty && quantity.trim.nn.isEmpty && unitCost.trim.nn.isEmpty

  object Row:
    def empty(key: Int): Row = Row(key, "", "", "", "", false)

  /** The text a cost is prefilled with: Italian, with at least the cents (`"3.5000"` → `"3,50"`). */
  def costText(cost: String): String = WarehouseFormats.decimalInputText(cost, 2, costScale)

  /** True when two canonical amounts are the same number (`"3.5"` and `"3.5000"`); false for text that is not a number. */
  private def sameAmount(a: String, b: String): Boolean = Try(BigDecimal(a.trim.nn).compare(BigDecimal(b.trim.nn)) == 0).getOrElse(false)

  /** `row` with its cost set from `option`'s expected price (cleared when there is none), unless the user typed the cost. */
  private def prefill(row: Row, option: Option[LineOption]): Row =
    if row.costTouched then row else row.copy(unitCost = option.flatMap(_.expectedUnitCost).fold("")(costText))

  private def normalizedCode(code: String): String = code.trim.nn.toUpperCase.nn

  final case class State(
      editingId: Option[UUID],
      today: String,
      date: String,
      number: String,
      supplierId: String,
      note: String,
      rows: List[Row],
      nextKey: Int,
  ) derives CanEqual:

    /** True on a fresh form nothing was entered in: validation messages stay hidden. */
    def pristine: Boolean =
      editingId.isEmpty && number.trim.nn.isEmpty && supplierId.trim.nn.isEmpty && note.trim.nn.isEmpty && rows.forall(_.isBlank)

    def addRow: State = copy(rows = rows :+ Row.empty(nextKey), nextKey = nextKey + 1)

    def removeRow(key: Int): State = copy(rows = rows.filterNot(_.key == key))

    def updateRow(key: Int)(patch: Row => Row): State = copy(rows = rows.map(row => if row.key == key then patch(row) else row))

    /** The cost typed by the user: from now on choosing an option keeps it (clearing it lets the next choice prefill again). */
    def setCost(key: Int, text: String): State = updateRow(key)(_.copy(unitCost = text, costTouched = text.trim.nn.nonEmpty))

    /** Picks the option `optionKey` for the row `key`, prefilling the cost with its expected price unless the user typed one. */
    def choose(key: Int, optionKey: String, context: Context): State =
      updateRow(key)(row => prefill(row.copy(choice = optionKey), context.option(supplierId, optionKey)))

    /**
     * Switches to the supplier `next`, moving every line onto an option of the new supplier for the same material: the same MPN code when the new
     * supplier has it, else its first MPN, else the "nuovo MPN" option; a line whose material has none (archived) loses its choice. Untouched costs
     * follow the new option's expected price.
     */
    def withSupplier(next: String, context: Context): State =
      if next == supplierId then this
      else
        def remap(row: Row): Row =
          if row.choice.trim.nn.isEmpty then row
          else
            val previousCode = context.option(supplierId, row.choice).flatMap(_.mpnCode)
            val candidates = for
              supplier <- parseUuid(next).toList
              material <- materialIdOf(row.choice).flatMap(context.material).toList
              candidate <- optionsOf(material, supplier)
            yield candidate
            val chosen = candidates.find(o => previousCode.exists(code => o.mpnCode.contains(code))).orElse(candidates.headOption)
            prefill(row.copy(choice = chosen.fold("")(_.key)), chosen)
        copy(supplierId = next, rows = rows.map(remap))

    /** The option selected in `row`, when it is one of the current supplier. */
    def optionOf(row: Row, context: Context): Option[LineOption] = context.option(supplierId, row.choice)

    /** The unit of the material selected in `row`. */
    def unitOf(row: Row, context: Context): Option[String] = optionOf(row, context).map(_.unit)

    /** The unit cost of `row`: the typed one, or — left blank — the expected price of its MPN; required when there is none. */
    private def costOf(row: Row, option: Option[LineOption]): Either[String, String] =
      if row.unitCost.trim.nn.isEmpty then option.flatMap(_.expectedUnitCost).toRight("Costo unitario obbligatorio")
      else MovementForm.parseCost(row.unitCost)

    /**
     * The note under the cost input of `row`: whether it is the expected price of the MPN or a different one, that the cost of a new MPN becomes its
     * expected price, or that no expected price is known; `None` while no option is selected.
     */
    def priceNote(row: Row, context: Context): Option[String] =
      optionOf(row, context).map { option =>
        option.expectedUnitCost match
          case _ if option.isNewMpn => "Nuovo MPN: il costo diventa il suo prezzo previsto"
          case None => "Nessun prezzo previsto: indica il costo"
          case Some(expected) =>
            if row.unitCost.trim.nn.isEmpty || MovementForm.parseCost(row.unitCost).exists(sameAmount(_, expected)) then "Prezzo previsto"
            else s"Modificato (previsto ${WarehouseFormats.formatUnitPrice(expected)})"
      }

    /** `quantity × unit cost` of `row` in euro (2 decimals), once both are readable. */
    def lineTotal(row: Row, context: Context): Option[String] =
      val option = optionOf(row, context)
      for
        quantity <- MovementForm.parseQuantity(row.quantity, option.fold("")(_.unit)).toOption
        cost <- costOf(row, option).toOption
      yield MovementForm.total(quantity, cost)

    /** The total of the delivery note: the sum of the readable line totals (2 decimals), like the server. */
    def total(context: Context): String =
      rows.flatMap(lineTotal(_, context)).map(BigDecimal(_)).sum.setScale(2, RoundingMode.HALF_UP).bigDecimal.toPlainString.nn

    /** The code of the new MPN of `row`, checked against the MPNs the supplier already has on other materials. */
    private def newMpnCode(row: Row, option: LineOption, context: Context): Either[String, String] =
      val code = row.newMpnCode.trim.nn
      if code.isEmpty then Left("Indica il codice del nuovo MPN")
      else
        val owner = parseUuid(supplierId).flatMap { supplier =>
          context.materials.find(material =>
            material.id != option.materialId && material.mpns.exists(mpn =>
              mpn.supplierId == supplier && normalizedCode(mpn.code) == normalizedCode(code),
            ),
          )
        }
        owner.fold(Right(code))(material => Left(s"L'MPN ${normalizedCode(code)} di questo fornitore è già del materiale ${material.internalCode}"))

    /** One non-blank row as a request line, or its messages (without the row prefix). */
    private def resolve(row: Row, context: Context): Either[List[String], DeliveryNoteLineRequest] =
      val option: Either[String, LineOption] =
        if row.choice.trim.nn.isEmpty then Left("Seleziona il materiale")
        else optionOf(row, context).toRight("Il materiale selezionato non è disponibile per questo fornitore")
      val code: Either[String, String] = option.flatMap(o => o.mpnCode.fold(newMpnCode(row, o, context))(Right(_)))
      val quantity = MovementForm.parseQuantity(row.quantity, option.fold(_ => "", _.unit))
      val cost = costOf(row, option.toOption)
      (option, code, quantity, cost) match
        case (Right(o), Right(c), Right(q), Right(uc)) => Right(DeliveryNoteLineRequest(o.materialId, c, o.isNewMpn, q, uc))
        case _ =>
          Left((option.left.toOption ++ option.toOption.flatMap(_ => code.left.toOption) ++ quantity.left.toOption ++ cost.left.toOption).toList)

    /** The request lines (blank rows skipped, order kept), or every message prefixed with the 1-based row number (`Riga 2: …`). */
    private def lines(context: Context): Either[List[String], List[DeliveryNoteLineRequest]] =
      val checked = rows.zipWithIndex.filterNot((row, _) => row.isBlank).map { (row, index) =>
        resolve(row, context).left.map(_.map(message => s"Riga ${index + 1}: $message"))
      }
      if checked.isEmpty then Left(List("Aggiungi almeno una riga"))
      else
        val errors = checked.flatMap(_.left.getOrElse(Nil))
        if errors.isEmpty then Right(checked.flatMap(_.toOption)) else Left(errors)

    private def header: Either[List[String], (String, String, UUID)] =
      val day = MovementForm.parseDay(date, today)
      val numberCheck = Some(number.trim.nn).filter(_.nonEmpty).toRight("Numero DDT obbligatorio")
      val supplier = parseUuid(supplierId).toRight("Seleziona il fornitore")
      (day, numberCheck, supplier) match
        case (Right(d), Right(n), Right(s)) => Right((d, n, s))
        case _ => Left((day.left.toOption ++ numberCheck.left.toOption ++ supplier.left.toOption).toList)

    /** The payload to send, or every validation message: header first, then the lines. */
    def validated(context: Context): Either[List[String], DeliveryNoteRequest] =
      (header, lines(context)) match
        case (Right((day, n, supplier)), Right(requestLines)) => Right(DeliveryNoteRequest(n, day, supplier, SupplierForm.opt(note), requestLines))
        case (h, l) => Left(h.left.getOrElse(Nil) ++ l.left.getOrElse(Nil))

    def errors(context: Context): List[String] = validated(context).left.getOrElse(Nil)

    def request(context: Context): Option[DeliveryNoteRequest] = validated(context).toOption
  end State

  /** A fresh delivery note dated `today`, with one empty line. */
  def empty(today: String): State = State(None, today, today, "", "", "", List(Row.empty(0)), 1)

  /**
   * The form prefilled with `note` for editing: each line picks its MPN option (or, for an MPN no longer on the material, the "nuovo MPN" option with
   * its code), quantities and costs as Italian text. A cost equal to the expected price counts as untouched, so picking another option updates it.
   */
  def fromResponse(note: DeliveryNoteResponse, context: Context, today: String): State =
    val supplier = note.supplierId.toString
    val rows = note.lines.zipWithIndex.map { (line, index) =>
      val option = context.option(supplier, existingKey(line.materialId, line.mpnCode)).orElse(context.option(supplier, newMpnKey(line.materialId)))
      Row(
        index,
        option.fold("")(_.key),
        if option.exists(_.isNewMpn) then line.mpnCode else "",
        WarehouseFormats.decimalInputText(line.quantity, 0, MovementForm.quantityScale),
        costText(line.unitCost),
        !option.flatMap(_.expectedUnitCost).exists(sameAmount(_, line.unitCost)),
      )
    }
    State(Some(note.id), today, note.date, note.number, supplier, note.note.getOrElse(""), rows, rows.size)

  /** `(id, name)` pairs of the supplier select: the registry, plus `current` (the supplier of the note being edited) marked when deleted. */
  def supplierOptions(suppliers: List[SupplierResponse], current: Option[(UUID, String)]): List[(String, String)] =
    val listed = suppliers.sortBy(_.name.toLowerCase.nn).map(s => s.id.toString -> s.name)
    val missing = current.filterNot((id, _) => suppliers.exists(_.id == id)).map((id, name) => id.toString -> s"$name (eliminato)")
    listed ++ missing

  private def parseUuid(value: String): Option[UUID] = Try(UUID.fromString(value.trim).nn).toOption
end DeliveryNoteForm
