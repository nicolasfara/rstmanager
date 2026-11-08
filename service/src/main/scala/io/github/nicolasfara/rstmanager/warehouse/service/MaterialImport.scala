package io.github.nicolasfara.rstmanager.warehouse.service

import java.io.ByteArrayInputStream
import java.text.Normalizer
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.math.BigDecimal.RoundingMode
import scala.util.Using

import io.github.nicolasfara.rstmanager.warehouse.domain.{ UnitOfMeasure, UnitPrice }
import io.github.nicolasfara.rstmanager.warehouse.domain.material.{ MaterialAggregate, MaterialError, MaterialId, MaterialInfo, Mpn }
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.{ Supplier, SupplierAggregate, SupplierError, SupplierId }

import cats.data.{ NonEmptyChain, ValidatedNec }
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import org.dhatim.fastexcel.reader.{ Cell, CellType, ReadableWorkbook, Row }

/**
 * The bulk load of materials from an Excel workbook (`.xlsx`). The whole sheet is read and checked before anything is written. A problem of the file
 * as a whole (not an `.xlsx`, missing headers, no rows, too many rows) rejects it; any other problem ignores the material (CODICE) it belongs to, all
 * of its rows, while the other materials are loaded. Every problem is listed with its Excel row (`Riga N, COLONNA: …`, or `Righe a, b (CODICE): …`
 * for a material spread over several rows). A material is never loaded with only part of its rows: once loaded, its code would be skipped when the
 * fixed file is loaded again.
 *
 * The first sheet has the headers on row 1 ([[Column]]: all required, in any order, extra columns ignored) and one row per MPN of a material: rows
 * with the same CODICE are one material, and must agree on its unit, description, type, customer price, and notes. A code already used by an active
 * material is skipped (and reported); unknown suppliers are created with their name only.
 *
 * Everything here is pure: [[readSheet]] decodes the bytes, [[draft]] lists what needs a new id, and [[plan]] checks the rows against the warehouse
 * and dry-runs the decisions that would create the materials and suppliers, using the ids the service generated for the draft.
 */
object MaterialImport:
  /** Largest file accepted, in bytes (5 MB). */
  val maxBytes: Int = 5 * 1024 * 1024

  /** Most material rows a sheet may have. */
  val maxRows: Int = 5000

  /** A problem of the file, reported on Excel row `line` (`0` for the file as a whole), which orders the problems. */
  final case class Problem(line: Int, message: String) derives CanEqual

  type Checked[A] = ValidatedNec[Problem, A]

  /** A required column of the sheet, by the header it has on row 1. */
  enum Column(val header: String) derives CanEqual:
    case Code extends Column("CODICE")
    case Measure extends Column("UNITA' DI MISURA")
    case Description extends Column("DESCRIZIONE")
    case Category extends Column("TIPOLOGIA")
    case CustomerPrice extends Column("PREZZO CLIENTE")
    case Notes extends Column("NOTE")
    case SupplierName extends Column("FORNITORE")
    case PartNumber extends Column("CODICE MPN")
    case SupplierDescription extends Column("DESCRIZIONE FORNITORE")
    case SupplierPrice extends Column("PREZZO FORNITORE")

    /** The header as it is matched: see [[headerKey]]. */
    def key: String = headerKey(header)
  end Column

  /** The value of a non-empty cell; a formula counts with the value Excel last computed and saved in the file. */
  enum CellValue derives CanEqual:
    case Number(value: BigDecimal)
    case Text(value: String)

    /** A formula the file holds without a computed value (a file written by a program that does not compute formulas). */
    case Uncomputed

    /** An Excel error (`#DIV/0!`, `#N/D`, …). */
    case Failed(code: String)

  /** A material row of the sheet, `line` being its Excel row; a blank cell is absent. */
  final case class ImportRow(
      line: Int,
      code: Option[CellValue],
      unit: Option[CellValue],
      description: Option[CellValue],
      materialType: Option[CellValue],
      customerPrice: Option[CellValue],
      notes: Option[CellValue],
      supplier: Option[CellValue],
      mpn: Option[CellValue],
      supplierDescription: Option[CellValue],
      supplierPrice: Option[CellValue],
  ) derives CanEqual

  /** What needs a new id: the (normalized) codes and the supplier names ([[supplierKey]]) of the file. */
  final case class ImportDraft(codes: List[String], supplierKeys: List[String]) derives CanEqual

  /** The ids generated for a [[ImportDraft]]: the domain never draws random numbers, so the service draws them. */
  final case class ImportIds(materials: Map[String, MaterialId], suppliers: Map[String, SupplierId])

  /**
   * What the rows are checked against.
   *
   * @param materials
   *   The active materials: their codes are skipped, their MPNs are taken, their types give the spelling of a type.
   * @param suppliers
   *   The active suppliers, matched by name.
   */
  final case class ImportContext(today: LocalDate, materials: List[MaterialInfo], suppliers: List[Supplier], ids: ImportIds)

  /** A material to create, from the Excel rows `lines`. */
  final case class PlannedMaterial(info: MaterialInfo, lines: List[Int])

  /** A material skipped because an active material already has its code. */
  final case class SkippedMaterial(code: String, lines: List[Int]) derives CanEqual

  /**
   * What the import writes, the new suppliers first, then the materials, and what it leaves out.
   *
   * @param suppliers
   *   The new suppliers the materials to create use, by name.
   * @param skipped
   *   The codes an active material already has.
   * @param ignored
   *   The codes (normalized, sorted) left out because of a problem of one of their rows.
   * @param problems
   *   Every problem of the file, sorted by row; a problem of a row without a code ignores that row only.
   */
  final case class ImportPlan(
      suppliers: List[Supplier],
      materials: List[PlannedMaterial],
      skipped: List[SkippedMaterial],
      ignored: List[String],
      problems: List[Problem],
  )

  /** Header text as it is matched: accents removed, upper case, letters and digits only (`Unità di misura` matches `UNITA' DI MISURA`). */
  def headerKey(raw: String): String =
    Normalizer.normalize(raw, Normalizer.Form.NFD).nn.replaceAll("\\p{M}", "").nn.toUpperCase(Locale.ROOT).nn.filter(_.isLetterOrDigit)

  /** A supplier name as it is matched: surrounding blanks dropped, inner blanks collapsed, case ignored. */
  def supplierKey(name: String): String = displayName(name).toLowerCase(Locale.ROOT).nn

  /** A supplier name as a new supplier gets it: surrounding blanks dropped, inner blanks collapsed. */
  private def displayName(name: String): String = name.trim.nn.replaceAll("\\s+", " ").nn

  /** Units as the sheet may write them (case ignored): the Italian short names, the Italian names, and the wire names. */
  val unitAliases: Map[String, UnitOfMeasure] =
    List(
      UnitOfMeasure.Pieces -> List("pz", "pezzi", "pieces"),
      UnitOfMeasure.LinearMeters -> List("mt", "m", "ml", "metri", "linear_meters"),
      UnitOfMeasure.SquareMeters -> List("mq", "m2", "m²", "square_meters"),
      UnitOfMeasure.Kilograms -> List("kg", "kilograms"),
      UnitOfMeasure.Packs -> List("cf", "conf", "confezioni", "packs"),
    ).flatMap((unit, aliases) => aliases.map(_ -> unit)).toMap

  // ---------------------------------------------------------------------------------------------------------------------------------------------
  // Reading
  // ---------------------------------------------------------------------------------------------------------------------------------------------

  /**
   * The material rows of the first sheet of the workbook `bytes`: the headers on row 1, blank rows skipped, at least one and at most [[maxRows]]
   * rows. A file that is not a readable `.xlsx`, or whose headers miss a column, is rejected as a whole.
   */
  def readSheet(bytes: Array[Byte]): Either[NonEmptyChain[Problem], List[ImportRow]] =
    Either
      .catchNonFatal(ReadableWorkbook.isOOXMLZipHeader(bytes.take(8)))
      .toOption
      .filter(identity)
      .toRight(fileProblem("il file non è una cartella di lavoro Excel .xlsx"))
      .flatMap(_ =>
        Either
          .catchNonFatal(
            Using.resource(new ReadableWorkbook(new ByteArrayInputStream(bytes)))(workbook =>
              Using.resource(workbook.getFirstSheet.nn.openStream().nn)(stream => readRows(stream.iterator().nn.asScala)),
            ),
          )
          .leftMap(error => fileProblem(s"il file non si può leggere come cartella di lavoro Excel (${error.getMessage})"))
          .flatten,
      )

  private def fileProblem(message: String): NonEmptyChain[Problem] = NonEmptyChain.one(Problem(0, s"File: $message"))

  /** Reads the header row, then at most one row more than [[maxRows]] (the stream is lazy: a huge sheet is not read past the limit). */
  private def readRows(rows: Iterator[Row]): Either[NonEmptyChain[Problem], List[ImportRow]] =
    val buffered = rows.buffered
    val header = if buffered.hasNext && buffered.head.getRowNum == 1 then Some(buffered.next()) else None
    columnsOf(header).flatMap { columns =>
      val material = buffered.flatMap(importRow(_, columns)).take(maxRows + 1).toList
      if material.isEmpty then fileProblem("il foglio non contiene righe di materiali sotto le intestazioni").asLeft
      else if material.sizeIs > maxRows then fileProblem(s"il foglio ha più di $maxRows righe di materiali, il massimo consentito").asLeft
      else material.asRight
    }

  /** The column index of every [[Column]], from the header row (the first column with a header wins). */
  private def columnsOf(header: Option[Row]): Either[NonEmptyChain[Problem], Map[Column, Int]] =
    val found = header.toList
      .flatMap(_.iterator().nn.asScala.toList)
      .flatMap(cell => cellValue(cell).collect { case CellValue.Text(text) => headerKey(text) -> cell.getColumnIndex })
      .reverse
      .toMap
    val columns = Column.values.toList.flatMap(column => found.get(column.key).map(column -> _))
    val missing = Column.values.toList.filterNot(column => columns.exists(_._1 == column))
    NonEmptyChain
      .fromSeq(missing)
      .fold(columns.toMap.asRight)(absent =>
        NonEmptyChain
          .one(
            Problem(
              1,
              s"Riga 1: mancano le intestazioni ${absent.map(_.header).toList.mkString(", ")}" +
                s" (servono ${Column.values.map(_.header).mkString(", ")})",
            ),
          )
          .asLeft,
      )
  end columnsOf

  /** The row, unless every column of it is blank. */
  private def importRow(row: Row, columns: Map[Column, Int]): Option[ImportRow] =
    def at(column: Column): Option[CellValue] =
      columns
        .get(column)
        .filter(index => index < row.getCellCount)
        .flatMap(index => Option.when(row.hasCell(index))(row.getCell(index).nn))
        .flatMap(cellValue)
    val imported = ImportRow(
      row.getRowNum,
      at(Column.Code),
      at(Column.Measure),
      at(Column.Description),
      at(Column.Category),
      at(Column.CustomerPrice),
      at(Column.Notes),
      at(Column.SupplierName),
      at(Column.PartNumber),
      at(Column.SupplierDescription),
      at(Column.SupplierPrice),
    )
    Option.when(Column.values.exists(column => cellOf(imported, column).isDefined))(imported)
  end importRow

  /** fastexcel's cell types compare by identity (a Java enum). */
  private given CanEqual[CellType, CellType] = CanEqual.derived

  /** The value of `cell`: blank (or only blanks) is absent; a formula has the value saved in the file. */
  private def cellValue(cell: Cell): Option[CellValue] =
    def text(raw: String): Option[CellValue] = Some(raw.trim.nn).filter(_.nonEmpty).map(CellValue.Text(_))
    def computed(value: Any): Option[CellValue] = value match
      case number: java.math.BigDecimal => Some(CellValue.Number(BigDecimal(number)))
      case string: String => text(string)
      case flag: java.lang.Boolean => text(flag.toString)
      case _ => Some(CellValue.Uncomputed)
    cell.getType.nn match
      case CellType.EMPTY => None
      case CellType.NUMBER | CellType.FORMULA => computed(cell.getValue)
      case CellType.ERROR => Some(CellValue.Failed(cell.getText.nn.trim.nn))
      case CellType.STRING | CellType.BOOLEAN => text(cell.getText.nn)
  end cellValue

  private def cellOf(row: ImportRow, column: Column): Option[CellValue] = column match
    case Column.Code => row.code
    case Column.Measure => row.unit
    case Column.Description => row.description
    case Column.Category => row.materialType
    case Column.CustomerPrice => row.customerPrice
    case Column.Notes => row.notes
    case Column.SupplierName => row.supplier
    case Column.PartNumber => row.mpn
    case Column.SupplierDescription => row.supplierDescription
    case Column.SupplierPrice => row.supplierPrice

  // ---------------------------------------------------------------------------------------------------------------------------------------------
  // Cells
  // ---------------------------------------------------------------------------------------------------------------------------------------------

  private def problem[A](line: Int, column: Column, message: String): Checked[A] = Problem(line, s"Riga $line, ${column.header}: $message").invalidNec

  /** The text of a cell; a number is written without trailing decimal zeros (`12.50` is `12.5`). */
  private def text(row: ImportRow, column: Column): Checked[Option[String]] =
    cellOf(row, column) match
      case None => None.validNec
      case Some(CellValue.Text(value)) => Some(value).validNec
      case Some(CellValue.Number(value)) => Some(value.bigDecimal.stripTrailingZeros.nn.toPlainString.nn).validNec
      case Some(CellValue.Uncomputed) => problem(row.line, column, uncomputedMessage)
      case Some(CellValue.Failed(code)) => problem(row.line, column, s"la cella contiene l'errore $code")

  private val uncomputedMessage = "formula senza valore calcolato, apri e salva il file in Excel"

  private def required(row: ImportRow, column: Column): Checked[String] =
    text(row, column).andThen(_.fold(problem(row.line, column, "il valore è obbligatorio"))(_.validNec))

  /**
   * A price: an Excel number (`9.8E-2` included) or a text in the Italian format (`1.234,56`, `3,7`; without a comma the dot is the decimal
   * separator), rounded HALF_UP to 4 decimals.
   */
  private def price(row: ImportRow, column: Column): Checked[Option[UnitPrice]] =
    def rounded(value: BigDecimal): Checked[UnitPrice] =
      UnitPrice.fromDecimalString(value.setScale(UnitPrice.scale, RoundingMode.HALF_UP).bigDecimal.toPlainString.nn) match
        case Right(parsed) => parsed.validNec
        case Left(error) => problem(row.line, column, error)
    cellOf(row, column) match
      case Some(CellValue.Number(value)) => rounded(value).map(Some(_))
      case Some(CellValue.Text(raw)) =>
        val compact = raw.filterNot(c => c.isWhitespace || c == '€' || c == ' ')
        val plain = if compact.contains(',') then compact.replace(".", "").nn.replace(',', '.').nn else compact
        if plain.matches("""-?\d+(\.\d+)?""") then rounded(BigDecimal(plain)).map(Some(_))
        else problem(row.line, column, s"'$raw' non è un prezzo valido (scrivi ad esempio 12,50)")
      case _ => text(row, column).as(None)

  private def unit(row: ImportRow): Checked[UnitOfMeasure] =
    required(row, Column.Measure).andThen(raw =>
      unitAliases
        .get(raw.toLowerCase(Locale.ROOT).nn)
        .fold(problem(row.line, Column.Measure, s"'$raw' non è un'unità di misura riconosciuta (usa pz, mt, mq, kg o cf)"))(_.validNec),
    )

  /** The MPN of a row: supplier name, part number (normalized), supplier-side description, expected unit cost. */
  private final case class RowMpn(supplier: String, code: String, description: Option[String], expectedUnitCost: Option[UnitPrice])

  /** A row whose cells are all well formed. */
  private final case class ParsedRow(
      line: Int,
      unit: UnitOfMeasure,
      description: Option[String],
      materialType: Option[String],
      customerPrice: UnitPrice,
      notes: Option[String],
      mpn: Option[RowMpn],
  )

  /** An MPN of the material `code`, named on Excel row `line`. */
  private final case class MpnUse(code: String, line: Int, mpn: RowMpn)

  private def parseRow(row: ImportRow): Checked[ParsedRow] =
    (
      unit(row),
      text(row, Column.Description),
      text(row, Column.Category),
      price(row, Column.CustomerPrice).andThen(_.fold(problem(row.line, Column.CustomerPrice, "il valore è obbligatorio"))(_.validNec)),
      text(row, Column.Notes),
      rowMpn(row),
    ).mapN((parsedUnit, description, materialType, customerPrice, notes, parsedMpn) =>
      ParsedRow(row.line, parsedUnit, description, materialType, customerPrice, notes, parsedMpn),
    )

  /** The MPN group of a row: FORNITORE and CODICE MPN together, with the optional DESCRIZIONE FORNITORE and PREZZO FORNITORE. */
  private def rowMpn(row: ImportRow): Checked[Option[RowMpn]] =
    (
      text(row, Column.SupplierName),
      text(row, Column.PartNumber),
      text(row, Column.SupplierDescription),
      price(row, Column.SupplierPrice),
    ).tupled.andThen {
      case (Some(supplier), Some(code), description, cost) =>
        Some(RowMpn(displayName(supplier), MaterialInfo.normalizedCode(code), description, cost)).validNec
      case (None, None, None, None) => None.validNec
      case (Some(_), None, _, _) => problem(row.line, Column.PartNumber, "indica il codice MPN del fornitore, oppure lascia vuoto anche FORNITORE")
      case (None, Some(_), _, _) => problem(row.line, Column.SupplierName, "indica il fornitore dell'MPN, oppure lascia vuoto anche CODICE MPN")
      case (None, None, _, _) =>
        problem(row.line, Column.PartNumber, "DESCRIZIONE FORNITORE e PREZZO FORNITORE si indicano solo con FORNITORE e CODICE MPN")
    }

  // ---------------------------------------------------------------------------------------------------------------------------------------------
  // Planning
  // ---------------------------------------------------------------------------------------------------------------------------------------------

  /** What needs an id, whatever the warehouse holds: every normalized code and every supplier name ([[supplierKey]]) of the rows. */
  def draft(rows: List[ImportRow]): ImportDraft =
    def texts(column: Column): List[String] = rows.flatMap(row => text(row, column).toOption.flatten)
    ImportDraft(texts(Column.Code).map(MaterialInfo.normalizedCode).distinct, texts(Column.SupplierName).map(supplierKey).distinct)

  /** A supplier of a row: an active one, matched by name, or a new one. */
  private enum ResolvedSupplier derives CanEqual:
    case Existing(supplier: Supplier)
    case New(key: String, newName: String, newId: SupplierId)

    def id: SupplierId = this match
      case Existing(supplier) => supplier.id
      case New(_, _, newId) => newId

    def name: String = this match
      case Existing(supplier) => supplier.name
      case New(_, newName, _) => newName
  end ResolvedSupplier

  /** A problem and the codes it ignores: every code of the rows it is about. */
  private type Attributed = (Set[String], Problem)

  private def problemsOf[A](checked: Checked[A]): List[Problem] = checked.fold(_.toChain.toList, _ => Nil)

  /**
   * The plan of `rows` against `context`: the materials to create, and the codes ignored because of a problem of one of their rows.
   *
   *   - Every row has a CODICE; rows with the same code (normalized like [[MaterialInfo.normalizedCode]]) are one material. A row without a code is
   *     reported and left out. A code an active material already has is skipped, and its rows are not checked further.
   *   - The rows of a material agree on the unit, the customer price, and, where they give them, the description, the type (case ignored), and the
   *     notes. A material without a description takes the supplier description of its first row that has one.
   *   - A supplier is matched by name (blanks collapsed, case ignored) among the active ones: two active suppliers with the name are ambiguous (every
   *     material naming it is ignored), no active supplier means a new one.
   *   - An MPN (supplier, code) belongs to one material: repeated on several materials of the file it ignores all of them (the rows that are wrong
   *     for other reasons included, so fixing them does not reveal a new problem); used by an active material it ignores the material.
   *   - Every material and new supplier passes the decision that creates it, on the empty state; a new supplier is created only when a material to
   *     create uses it.
   *   - A type is spelled as an active material already spells it (case ignored), otherwise as the material to create with the first row has it:
   *     ignored rows have no effect on the materials created.
   */
  def plan(rows: List[ImportRow], context: ImportContext): ImportPlan =
    val coded = rows.map(row => required(row, Column.Code).map(code => row -> MaterialInfo.normalizedCode(code)))
    val codeProblems = coded.flatMap(problemsOf)
    val groups = coded
      .flatMap(_.toOption)
      .groupMap(_._2)(_._1)
      .toList
      .map((code, grouped) => (code, grouped.sortBy(_.line)))
      .sortBy(_._2.headOption.fold(0)(_.line))
    val activeCodes = context.materials.map(info => info.internalCode: String).toSet
    val kept = groups.filterNot((code, _) => activeCodes.contains(code))
    val skipped = groups.collect { case (code, grouped) if activeCodes.contains(code) => SkippedMaterial(code, grouped.map(_.line)) }

    val parsed = kept.map((code, grouped) => (code, grouped.map(parseRow)))
    val parsedRows = parsed.flatMap((code, checked) => checked.flatMap(_.toOption).map(code -> _))
    // The MPNs of every kept row, also of a row with a problem elsewhere: fixing that problem must not reveal a new one.
    val uses = kept.flatMap((code, grouped) => grouped.flatMap(row => rowMpn(row).toOption.flatten.map(MpnUse(code, row.line, _))))
    def usersOf(key: String): Set[String] = uses.filter(use => supplierKey(use.mpn.supplier) == key).map(_.code).toSet

    val suppliers = resolveSuppliers(uses, context)
    val resolved = suppliers.view.mapValues(_.toOption).toMap.collect { case (key, Some(supplier)) => key -> supplier }
    val rowProblems: List[Attributed] =
      parsed.flatMap((code, checked) => checked.flatMap(problemsOf).map(Set(code) -> _)) ++
        suppliers.toList.flatMap((key, checked) => problemsOf(checked).map(usersOf(key) -> _)) ++
        sharedMpns(uses, resolved) ++
        takenMpns(uses, resolved, context)

    val built = parsed.collect {
      case (code, checked) if checked.forall(_.isValid) => code -> material(code, checked.flatMap(_.toOption), resolved, context)
    }
    val materialProblems: List[Attributed] = built.flatMap((code, checked) => problemsOf(checked).map(Set(code) -> _))
    val rejected = (rowProblems ++ materialProblems).flatMap(_._1).toSet
    val candidates = built.flatMap((code, checked) => checked.toOption.filterNot(_ => rejected.contains(code)))

    val needed = candidates.flatMap(_.info.suppliers).toSet
    val candidateRows = parsedRows.filter((code, _) => candidates.exists(m => (m.info.internalCode: String) == code)).map(_._2)
    val newSuppliers = resolved.values.toList.distinct.collect {
      case ResolvedSupplier.New(key, name, id) if needed.contains(id) => id -> newSupplier(key, name, id, candidateRows)
    }
    val supplierProblems: List[Attributed] = newSuppliers.flatMap((id, checked) =>
      problemsOf(checked).map(candidates.filter(_.info.suppliers.contains(id)).map(m => m.info.internalCode: String).toSet -> _),
    )

    val ignored = (rowProblems ++ materialProblems ++ supplierProblems).flatMap(_._1).toSet
    val loaded = candidates.filterNot(m => ignored.contains(m.info.internalCode))
    val used = loaded.flatMap(_.info.suppliers).toSet
    val spellings = typeSpellings(loaded.sortBy(_.lines.headOption.getOrElse(0)).flatMap(_.info.materialType), context)
    ImportPlan(
      newSuppliers
        .flatMap((id, checked) => checked.toOption.filter(_ => used.contains(id)))
        .sortBy(supplier => (supplier.name: String).toLowerCase(Locale.ROOT).nn),
      loaded.map(m => m.copy(info = m.info.copy(materialType = m.info.materialType.map(spelled => spellings.getOrElse(typeKey(spelled), spelled))))),
      skipped,
      ignored.toList.sorted,
      (codeProblems ++ (rowProblems ++ materialProblems ++ supplierProblems).map(_._2)).distinct.sortBy(_.line),
    )
  end plan

  /** The supplier of every supplier name of `rows`, by [[supplierKey]], or the problem of an ambiguous name or a missing id. */
  private def resolveSuppliers(uses: List[MpnUse], context: ImportContext): Map[String, Checked[ResolvedSupplier]] =
    val active = context.suppliers.groupBy(supplier => supplierKey(supplier.name))
    uses
      .map(use => (supplierKey(use.mpn.supplier), use.mpn.supplier, use.line))
      .groupBy(_._1)
      .map { (key, uses) =>
        val name = uses.minBy(_._3)._2
        val lines = uses.map(_._3).distinct.sorted
        val resolved = active.getOrElse(key, Nil) match
          case List(supplier) => ResolvedSupplier.Existing(supplier).validNec
          case Nil =>
            context.ids.suppliers
              .get(key)
              .fold(Problem(lines.min, s"${linesLabel(lines)}, FORNITORE: manca l'identificativo del nuovo fornitore $name").invalidNec)(id =>
                ResolvedSupplier.New(key, name, id).validNec,
              )
          case several =>
            Problem(
              lines.min,
              s"${linesLabel(lines)}, FORNITORE: ci sono ${several.size} fornitori attivi di nome $name; rinominali in modo che il nome sia univoco",
            ).invalidNec
        key -> resolved
      }
  end resolveSuppliers

  /** The rows of an MPN (supplier, code) listed for more than one material of the file. */
  private def sharedMpns(uses: List[MpnUse], suppliers: Map[String, ResolvedSupplier]): List[Attributed] =
    uses
      .flatMap(use => suppliers.get(supplierKey(use.mpn.supplier)).map(supplier => (supplier, use.mpn.code, use.code, use.line)))
      .groupBy((supplier, mpnCode, _, _) => (supplier.id, mpnCode))
      .values
      .toList
      .flatMap { uses =>
        val materials = uses.sortBy(_._4).map(_._3).distinct
        uses.headOption.filter(_ => materials.sizeIs > 1).map { (supplier, mpnCode, _, _) =>
          val lines = uses.map(_._4).sorted
          materials.toSet -> Problem(
            lines.min,
            s"${linesLabel(lines)}: MPN $mpnCode di ${supplier.name} indicato per più materiali (${materials.mkString(", ")})",
          )
        }
      }

  /** The rows naming an MPN an active material already has: an MPN belongs to one active material. */
  private def takenMpns(uses: List[MpnUse], suppliers: Map[String, ResolvedSupplier], context: ImportContext): List[Attributed] =
    for
      use <- uses
      supplier <- suppliers.get(supplierKey(use.mpn.supplier)).toList
      owner <- context.materials.find(_.mpn(supplier.id, use.mpn.code).isDefined).toList
    yield Set(use.code) -> Problem(
      use.line,
      s"Riga ${use.line}, CODICE MPN: l'MPN ${use.mpn.code} di ${supplier.name} è già usato dal materiale ${owner.internalCode}",
    )

  /** The spelling of every type (by lower case): the one of an active material, otherwise the first of `types`. */
  private def typeSpellings(types: List[String], context: ImportContext): Map[String, String] =
    val existing = context.materials.flatMap(_.materialType).reverse.map(spelled => typeKey(spelled) -> spelled).toMap
    types
      .foldLeft(existing)((spellings, spelled) =>
        if spellings.contains(typeKey(spelled)) then spellings else spellings + (typeKey(spelled) -> spelled),
      )

  private def typeKey(materialType: String): String = materialType.toLowerCase(Locale.ROOT).nn

  /** `Riga 5` or `Righe 5, 6`. */
  private def linesLabel(lines: List[Int]): String = lines match
    case List(line) => s"Riga $line"
    case _ => s"Righe ${lines.mkString(", ")}"

  /** The material of the rows of `code` (all well formed), dry-run on the empty state; its type is spelled as its first row has it. */
  private def material(
      code: String,
      rows: List[ParsedRow],
      suppliers: Map[String, ResolvedSupplier],
      context: ImportContext,
  ): Checked[PlannedMaterial] =
    val lines = rows.map(_.line)
    val first = lines.headOption.getOrElse(0)
    val label = s"${linesLabel(lines)} ($code)"
    def fail[A](message: String): Checked[A] = Problem(first, s"$label: $message").invalidNec
    def single[A](values: List[A], differ: String, shown: A => String): Checked[Option[A]] =
      values.distinct match
        case Nil => None.validNec
        case List(value) => Some(value).validNec
        case several => fail(s"$differ: ${several.map(shown).mkString(", ")}")

    val unit = single(rows.map(_.unit), "UNITA' DI MISURA diverse", WarehouseHttpSupport.unitShortLabel)
    val price = single(rows.map(_.customerPrice), "PREZZO CLIENTE diversi", WarehouseHttpSupport.displayUnitPrice)
    val description = single(rows.flatMap(_.description), "DESCRIZIONE diverse", identity).andThen(
      _.orElse(rows.flatMap(_.mpn.flatMap(_.description)).headOption)
        .fold(fail[String]("DESCRIZIONE: manca, e nessuna riga del codice ha una DESCRIZIONE FORNITORE da usare al suo posto"))(_.validNec),
    )
    val materialType = rows
      .flatMap(_.materialType)
      .groupBy(typeKey)
      .keys
      .toList match
      case Nil => None.validNec
      case List(_) => rows.flatMap(_.materialType).headOption.validNec
      case _ => fail(s"TIPOLOGIA diverse: ${rows.flatMap(_.materialType).distinctBy(typeKey).mkString(", ")}")
    val notes = single(rows.flatMap(_.notes), "NOTE diverse", identity)
    val mpns = rows.flatTraverse(row =>
      row.mpn.toList.flatTraverse(mpn =>
        suppliers
          .get(supplierKey(mpn.supplier))
          .toList
          .traverse(supplier =>
            Mpn
              .createMpn(mpn.code, supplier.id, mpn.description, mpn.expectedUnitCost)
              .leftMap(_.map(error => Problem(row.line, s"Riga ${row.line}: $error"))),
          ),
      ),
    )
    val id = context.ids.materials.get(code).fold(fail[MaterialId]("manca l'identificativo del nuovo materiale"))(_.validNec)

    (id, unit, price, description, materialType, notes, mpns).tupled.andThen {
      case (materialId, Some(parsedUnit), Some(customerPrice), parsedDescription, parsedType, parsedNotes, parsedMpns) =>
        MaterialInfo
          .createMaterialInfo(materialId, code, parsedDescription, parsedType, parsedNotes, parsedUnit, customerPrice, parsedMpns)
          .leftMap(_.map(error => Problem(first, s"$label: $error")))
          .andThen(info =>
            MaterialAggregate.Empty.create(info, context.today).toEither match
              case Right(_) => PlannedMaterial(info, lines).validNec
              case Left(errors) =>
                NonEmptyChain
                  .fromSeq(errors.toChain.toList.flatMap(materialProblem(_, info)).distinct.map(problem => Problem(first, s"$label: $problem")))
                  .fold(PlannedMaterial(info, lines).validNec[Problem])(_.invalid),
          )
      case _ => fail("il materiale non ha righe")
    }
  end material

  private def materialProblem(error: MaterialError, info: MaterialInfo): List[String] =
    WarehouseHttpSupport.materialProblems(error, info.id.value, Some(info))

  /** The new supplier `name`, dry-run on the empty state; a problem is reported on the first row that names it. */
  private def newSupplier(key: String, name: String, id: SupplierId, rows: List[ParsedRow]): Checked[Supplier] =
    val lines = rows.filter(_.mpn.exists(mpn => supplierKey(mpn.supplier) == key)).map(_.line).sorted
    val label = s"${linesLabel(lines)}, FORNITORE"
    def located(message: String): Problem = Problem(lines.headOption.getOrElse(0), s"$label: $message")
    Supplier
      .createSupplier(id, name, None, None, None, None, None)
      .leftMap(_.map(located))
      .andThen(supplier =>
        SupplierAggregate.Empty.create(supplier).toEither match
          case Right(_) => supplier.validNec
          case Left(errors) => errors.map(error => located(supplierProblem(error, name))).invalid,
      )

  private def supplierProblem(error: SupplierError, name: String): String = error match
    case SupplierError.SupplierAlreadyExists => s"il fornitore $name esiste già"
    case SupplierError.SupplierNotFound => s"il fornitore $name non esiste"
    case SupplierError.SupplierIdMismatch => s"l'identificativo del fornitore $name non corrisponde"
end MaterialImport
