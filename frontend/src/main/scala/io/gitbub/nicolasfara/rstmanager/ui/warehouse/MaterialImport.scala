package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ ApiError, MaterialImportResponse }

/**
 * Pure (DOM-free) state of the "Carica materiali da Excel" dialog: the chosen file, the outcome of its check (a dry run of the import), and the
 * upload.
 *
 * `F` is the file handle: a `dom.File` in the dialog, any value in tests. Every choice of a file (and every reset) bumps `selection`, and a response
 * is applied only when it carries the selection it was sent for, so the answer about a file replaced meanwhile is dropped. "Carica" is possible only
 * after a successful check of the current file that would create at least one material.
 */
object MaterialImport:

  /** Error code of a file the server checked and refused: expected feedback, shown in the dialog only (not as a global error). */
  val rejectedCode: String = "import-rejected"

  enum Phase derives CanEqual:
    /** No file chosen yet. */
    case Idle

    /** The dry run of the chosen file is in flight. */
    case Checking

    /** The dry run succeeded: what the upload would do. */
    case Checked(summary: MaterialImportResponse)

    /** The check or the upload failed: nothing was saved. */
    case Rejected(error: ApiError)

    /** The upload of a checked file is in flight. */
    case Importing(summary: MaterialImportResponse)

    /** The upload succeeded. */
    case Imported(summary: MaterialImportResponse)
  end Phase

  final case class State[F](selection: Int, file: Option[F], fileName: String, phase: Phase):

    /** A request is in flight: the upload button stays disabled. */
    def busy: Boolean = phase match
      case Phase.Checking | Phase.Importing(_) => true
      case _ => false

    /** The upload is in flight: the file cannot be changed and the dialog cannot be closed. */
    def importing: Boolean = phase match
      case Phase.Importing(_) => true
      case _ => false

    /** The checked file would create something: "Carica" is enabled. */
    def canImport: Boolean = phase match
      case Phase.Checked(summary) => summary.created.nonEmpty
      case _ => false

    /**
     * Chooses `file` (named `name`) and starts its check. A file that is not an `.xlsx` workbook is refused at once, without a request. Ignored
     * during an upload.
     */
    def choose(chosen: F, name: String): State[F] =
      if importing then this
      else
        val phase = if isXlsx(name) then Phase.Checking else Phase.Rejected(notXlsx)
        State(selection + 1, Some(chosen), name, phase)

    /** True when the dialog must send the dry run of the current file. */
    def needsCheck: Boolean = phase match
      case Phase.Checking => true
      case _ => false

    /** Applies the dry run outcome sent for `forSelection`; a stale one (another file chosen since) is dropped. */
    def checked(forSelection: Int, result: Either[ApiError, MaterialImportResponse]): State[F] =
      phase match
        case Phase.Checking if forSelection == selection => copy(phase = result.fold(Phase.Rejected(_), Phase.Checked(_)))
        case _ => this

    /** Starts the upload of the checked file: the next state and the file to send, `None` when the upload is not allowed now. */
    def startImport: Option[(State[F], F)] =
      phase match
        case Phase.Checked(summary) if summary.created.nonEmpty => file.map(f => (copy(phase = Phase.Importing(summary)), f))
        case _ => None

    /** Applies the upload outcome sent for `forSelection`. */
    def imported(forSelection: Int, result: Either[ApiError, MaterialImportResponse]): State[F] =
      phase match
        case Phase.Importing(_) if forSelection == selection => copy(phase = result.fold(Phase.Rejected(_), Phase.Imported(_)))
        case _ => this

    /** Back to no file, as when the dialog is reopened; `selection` keeps growing so late responses are still dropped. */
    def reset: State[F] = State(selection + 1, None, "", Phase.Idle)
  end State

  def initial[F]: State[F] = State(0, None, "", Phase.Idle)

  private val notXlsx = ApiError("invalid-file", "Scegli un file Excel in formato .xlsx.", Nil)

  private def isXlsx(name: String): Boolean = name.trim.nn.toLowerCase.nn.endsWith(".xlsx")

  // ---- Texts -------------------------------------------------------------------------------------

  private def count(n: Int, one: String, many: String): String = if n == 1 then s"1 $one" else s"$n $many"

  /** `"359 materiali nuovi, 380 MPN"`. */
  def summaryText(summary: MaterialImportResponse): String =
    s"${count(summary.created.size, "materiale nuovo", "materiali nuovi")}, ${count(summary.mpns, "MPN", "MPN")}"

  /** The confirmation shown after an upload: `"Caricati 359 materiali (380 MPN). Creati 12 fornitori. 3 codici già presenti saltati."`. */
  def confirmationText(summary: MaterialImportResponse): String =
    val materials =
      if summary.created.size == 1 then s"Caricato 1 materiale (${count(summary.mpns, "MPN", "MPN")})."
      else s"Caricati ${summary.created.size} materiali (${count(summary.mpns, "MPN", "MPN")})."
    val suppliers = summary.newSuppliers.size match
      case 0 => None
      case 1 => Some("Creato 1 fornitore.")
      case n => Some(s"Creati $n fornitori.")
    val skipped = summary.skipped.size match
      case 0 => None
      case 1 => Some("1 codice già presente saltato.")
      case n => Some(s"$n codici già presenti saltati.")
    (materials :: suppliers.toList ::: skipped.toList).mkString(" ")
end MaterialImport
