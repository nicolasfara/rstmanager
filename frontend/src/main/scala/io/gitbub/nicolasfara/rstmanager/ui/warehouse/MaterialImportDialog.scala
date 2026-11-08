package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, ErrorCenter }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MaterialImport.Phase
import org.scalajs.dom

/**
 * The "Carica materiali da Excel" dialog (Admin): explains the expected columns, checks the chosen `.xlsx` file at once with a dry run of the import,
 * shows what the upload would do (or every problem found, with its row), and uploads the checked file with "Carica".
 *
 * Materials with problems are left out of the upload (listed as a warning after the check), the others are loaded; a file refused as a whole shows
 * the error instead. Opened by `isOpen`; every opening starts from no file. The state rules live in [[MaterialImport]]. After an upload it calls
 * [[AppBus.mutatedWarehouse]] (materials and suppliers share the warehouse tick), closes, and hands the summary to `onImported` for the confirmation.
 */
object MaterialImportDialog:

  private val xlsxAccept = ".xlsx,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

  def apply(isOpen: Var[Boolean], onImported: MaterialImportResponse => Unit): HtmlElement =
    val state = Var(MaterialImport.initial[dom.File])
    // The ✕ is ignored during an upload: its outcome must still close the dialog and refresh the warehouse.
    val close: () => Unit = () => if !state.now().importing then isOpen.set(false)

    def check(): Unit =
      val current = state.now()
      current.file.filter(_ => current.needsCheck).foreach { file =>
        ApiClient.importMaterials(file, true).foreach { result =>
          result.left.foreach(reportUnexpected("Verifica file materiali"))
          state.update(_.checked(current.selection, result))
        }
      }

    def upload(): Unit =
      state.now().startImport.foreach { (next, file) =>
        state.set(next)
        ApiClient.importMaterials(file, false).foreach { result =>
          state.update(_.imported(next.selection, result))
          result match
            case Right(summary) =>
              AppBus.mutatedWarehouse()
              isOpen.set(false)
              onImported(summary)
            case Left(err) =>
              // A failed upload may have touched the warehouse before being rolled back: refresh anyway.
              AppBus.mutatedWarehouse()
              reportUnexpected("Caricamento materiali")(err)
        }
      }

    modal(isOpen.signal, Signal.fromValue("Carica materiali da Excel"), "max-w-2xl", close)(
      div(
        // Every opening starts over: no file, no outcome. The content (file input included) is rebuilt too.
        isOpen.signal.changes.filter(identity) --> (_ => state.update(_.reset)),
        child.maybe <-- isOpen.signal.distinct.map(open => Option.when(open)(content(state, check, upload, close))),
      ),
    )
  end apply

  /** A refused file is expected feedback, shown in the dialog; anything else (network, permissions, a failed write) also goes to the error center. */
  private def reportUnexpected(context: String)(err: ApiError): Unit =
    if err.code != MaterialImport.rejectedCode then ErrorCenter.report(context, err)

  private def content(state: Var[MaterialImport.State[dom.File]], check: () => Unit, upload: () => Unit, close: () => Unit): HtmlElement =
    div(
      cls := "space-y-4 text-sm",
      instructions,
      filePicker(state, check),
      child <-- state.signal.map(_.phase).distinct.map(renderPhase),
      div(
        cls := "flex justify-end gap-2",
        button(tpe := "button", cls := btnGhost, "Annulla", disabled <-- state.signal.map(_.importing), onClick --> (_ => close())),
        button(
          tpe := "button",
          cls := btnPrimary,
          child.text <-- state.signal.map(s => if s.importing then "Caricamento…" else "Carica"),
          disabled <-- state.signal.map(s => !s.canImport || s.busy),
          onClick --> (_ => upload()),
        ),
      ),
    )

  private def instructions: HtmlElement =
    def column(name: String, text: String): HtmlElement =
      li(span(cls := "font-medium text-slate-700", name), if text.isEmpty then "" else s" — $text")
    div(
      cls := "rounded-md bg-slate-50 px-3 py-2 text-xs text-slate-600",
      p("Il primo foglio del file deve avere le intestazioni nella prima riga, con queste colonne (in qualsiasi ordine; le altre sono ignorate):"),
      ul(
        cls := "mt-1 list-disc space-y-0.5 pl-5",
        column("CODICE", "codice interno, obbligatorio"),
        column("UNITA' DI MISURA", "pz, mt, mq, kg o cf, obbligatoria"),
        column("DESCRIZIONE", "se vuota si usa la DESCRIZIONE FORNITORE"),
        column("TIPOLOGIA", ""),
        column("PREZZO CLIENTE", "obbligatorio"),
        column("NOTE", ""),
        column("FORNITORE e CODICE MPN", "da indicare insieme, o entrambi vuoti"),
        column("DESCRIZIONE FORNITORE e PREZZO FORNITORE", "facoltativi, solo con un MPN"),
      ),
      p(cls := "mt-2", "Regole:"),
      ul(
        cls := "mt-1 list-disc space-y-0.5 pl-5",
        li("Una riga per MPN: più righe con lo stesso CODICE sono lo stesso materiale con più MPN, e gli altri campi devono coincidere."),
        li("I codici già presenti in magazzino vengono saltati."),
        li("I fornitori mancanti vengono creati con il solo nome."),
        li("I materiali con errori non vengono caricati (sono ignorate tutte le righe di quel CODICE); gli altri sì."),
        li("Corretto il file, caricalo di nuovo: i materiali già caricati verranno saltati."),
      ),
    )
  end instructions

  /**
   * A button-styled label around a hidden file input, with the chosen file name next to it. The input is emptied after every choice, so choosing the
   * same file again (e.g. after fixing it in Excel) checks it again.
   */
  private def filePicker(state: Var[MaterialImport.State[dom.File]], check: () => Unit): HtmlElement =
    div(
      cls := "flex flex-wrap items-center gap-3",
      label(
        cls := btnGhost,
        cls <-- state.signal.map(s => if s.importing then "pointer-events-none opacity-50" else "cursor-pointer"),
        "Scegli file .xlsx…",
        input(
          typ := "file",
          accept := xlsxAccept,
          cls := "sr-only",
          disabled <-- state.signal.map(_.importing),
          inContext { el =>
            onChange.mapToFiles --> { files =>
              files.headOption.foreach { file =>
                state.update(_.choose(file, file.name))
                check()
              }
              el.ref.value = ""
            }
          },
        ),
      ),
      span(
        cls := "min-w-0 truncate text-xs text-slate-500",
        child.text <-- state.signal.map(s => if s.fileName.isEmpty then "Nessun file scelto" else s.fileName),
      ),
    )

  private def renderPhase(phase: Phase): HtmlElement = phase match
    case Phase.Idle => div(cls := "text-xs text-slate-400", "Scegli il file: viene verificato subito, senza salvare niente.")
    case Phase.Checking => div(cls := "text-xs text-slate-500", "Verifica del file in corso…")
    case Phase.Checked(summary) => summaryView(summary)
    case Phase.Importing(summary) => div(summaryView(summary), div(cls := "mt-2 text-xs text-slate-500", "Caricamento in corso…"))
    case Phase.Imported(summary) => div(cls := "text-xs text-slate-600", MaterialImport.confirmationText(summary))
    case Phase.Rejected(error) =>
      div(
        // The details can list hundreds of problems: they scroll inside the dialog.
        cls := "max-h-96 overflow-y-auto",
        Option.when(error.details.size > 1)(div(cls := "mb-1 text-xs text-slate-500", s"${error.details.size} problemi trovati")),
        errorBanner(error),
      )

  private def summaryView(summary: MaterialImportResponse): HtmlElement =
    div(
      cls := "rounded-md border border-slate-200 p-3",
      div(cls := "font-medium text-slate-800", s"Verifica riuscita: ${MaterialImport.summaryText(summary)}."),
      Option.when(summary.created.isEmpty)(
        div(cls := "mt-1 text-xs text-slate-500", "Nessun materiale nuovo da caricare: tutti i codici sono già presenti."),
      ),
      Option.when(summary.newSuppliers.nonEmpty)(
        div(
          cls := "mt-2 text-xs",
          div(cls := "font-medium text-slate-600", s"Fornitori che verranno creati (${summary.newSuppliers.size})"),
          itemList(summary.newSuppliers),
        ),
      ),
      Option.when(summary.ignored.nonEmpty)(
        div(
          cls := "mt-3 rounded-md border border-amber-200 bg-amber-50 p-3 text-amber-800",
          div(cls := "font-medium", s"${MaterialImport.ignoredText(summary)}."),
          Option.when(summary.created.nonEmpty)(
            div(cls := "mt-0.5 text-xs", "Con «Carica» vengono caricati solo i materiali senza errori."),
          ),
          problemsList(summary),
        ),
      ),
      // The skipped codes can be hundreds (e.g. the same file uploaded again): collapsed by default.
      Option.when(summary.skipped.nonEmpty)(
        detailsTag(
          cls := "mt-2 text-xs",
          summaryTag(cls := "cursor-pointer font-medium text-slate-600", s"Codici già presenti, saltati (${summary.skipped.size})"),
          itemList(summary.skipped),
        ),
      ),
    )

  /** The problems of the ignored materials, scrollable like the error box, then their codes. */
  private[warehouse] def problemsList(summary: MaterialImportResponse): HtmlElement =
    div(
      cls := "mt-2 max-h-96 overflow-y-auto text-xs",
      ul(cls := "list-disc space-y-0.5 pl-5", summary.problems.map(problem => li(problem))),
      Option.when(summary.ignored.nonEmpty)(div(cls := "mt-2", span(cls := "font-medium", "Codici ignorati: "), summary.ignored.mkString(", "))),
    )

  private def itemList(items: List[String]): HtmlElement =
    ul(cls := "mt-1 max-h-40 list-disc overflow-y-auto pl-5 text-slate-600", items.map(item => li(item)))
end MaterialImportDialog
