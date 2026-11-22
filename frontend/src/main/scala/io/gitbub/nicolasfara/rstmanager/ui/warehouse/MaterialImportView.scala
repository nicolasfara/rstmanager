package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import scala.concurrent.ExecutionContext.Implicits.global

import com.raquo.laminar.api.L.*
import io.gitbub.nicolasfara.rstmanager.api.ApiClient
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.{ AppBus, ErrorCenter, NavigationGuard }
import io.gitbub.nicolasfara.rstmanager.ui.Components.*
import io.gitbub.nicolasfara.rstmanager.ui.warehouse.MaterialImport.Phase
import org.scalajs.dom

/**
 * The "Carica materiali da Excel" page (Admin), shown in place of the materials list: explains the expected columns, checks the chosen `.xlsx` file
 * at once with a dry run of the import, shows what the upload would do (or every problem found, with its row), and uploads the checked file with
 * "Carica", which stays disabled until a check found something to load (the bar and the phase panel say why).
 *
 * Materials with problems are left out of the upload (listed as a warning after the check), the others are loaded; a file refused as a whole shows
 * the error instead. Every opening starts from no file. The state rules live in [[MaterialImport]]. After an upload it calls
 * [[AppBus.mutatedWarehouse]] (materials and suppliers share the warehouse tick) and hands the summary to `onImported`, which goes back to the list.
 * During the upload "Annulla" is disabled, and leaving the page (back link, browser back, top navigation, tab switch) asks first: leaving does not
 * stop the upload, whose outcome still reaches `onImported`.
 */
object MaterialImportView:

  private val xlsxAccept = ".xlsx,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

  /** The page; `onExit` goes back to the list, `onImported` receives the outcome of a successful upload (even after leaving during it). */
  def apply(onExit: () => Unit, onImported: MaterialImportResponse => Unit): HtmlElement =
    val state = Var(MaterialImport.initial[dom.File])
    // During an upload the leave goes through the navigation guard, which asks first.
    val leave: () => Unit = () => if state.now().importing then NavigationGuard.request(onExit) else onExit()

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
              onImported(summary)
            case Left(err) =>
              // A failed upload may have touched the warehouse before being rolled back: refresh anyway.
              AppBus.mutatedWarehouse()
              reportUnexpected("Caricamento materiali")(err)
        }
      }

    formPage(
      PageHeader(
        "← Torna ai materiali",
        leave,
        Signal.fromValue("Carica materiali da Excel"),
        context = List("Magazzino", "Materiali"),
        requiredLegend = false,
        initialFocus = PageFocus.FirstField,
      ),
    )(
      NavigationGuard.guard(
        "Uscire durante il caricamento?",
        "Caricamento in corso: uscire ora non annulla il caricamento.",
        () => state.now().importing,
      ),
      formColumn(
        formSection("Istruzioni", "Il file viene verificato appena lo scegli, senza salvare niente.")(instructions),
        formSection("File Excel", "")(
          filePicker(state, check),
          div(aria.live := "polite", child <-- state.signal.map(_.phase).distinct.map(renderPhase)),
        ),
        stickyActionBar(
          p(aria.live := "polite", cls := "text-xs", child.text <-- state.signal.map(s => statusText(s.phase))),
          importActions(state.signal, leave, () => upload()),
        ),
      ),
    )
  end apply

  /** A refused file is expected feedback, shown on the page; anything else (network, permissions, a failed write) also goes to the error center. */
  private def reportUnexpected(context: String)(err: ApiError): Unit =
    if err.code != MaterialImport.rejectedCode then ErrorCenter.report(context, err)

  /**
   * The one-line state of the page next to the buttons, which explains a disabled "Carica" (the phase panel above has the details).
   */
  private[warehouse] def statusText(phase: Phase): String = phase match
    case Phase.Idle => "Scegli un file .xlsx da verificare."
    case Phase.Checking => "Verifica del file in corso…"
    case Phase.Checked(summary) if summary.created.isEmpty => "Nessun materiale nuovo da caricare."
    case Phase.Checked(summary) =>
      if summary.created.size == 1 then "Pronto: 1 materiale da caricare." else s"Pronto: ${summary.created.size} materiali da caricare."
    case Phase.Importing(_) => "Caricamento in corso…"
    case Phase.Imported(_) => "Caricamento completato."
    case Phase.Rejected(_) => "File non caricabile: correggilo e sceglilo di nuovo."

  /**
   * "Annulla" (disabled during the upload) and "Carica" (enabled only once a check found materials to load), 40px tall below `md`. A local variant of
   * [[Components.formActionsInline]], whose cancel cannot be disabled.
   */
  private def importActions(state: Signal[MaterialImport.State[dom.File]], onCancel: () => Unit, onUpload: () => Unit): HtmlElement =
    div(
      cls := "ml-auto flex items-center gap-2 *:min-h-10 *:min-w-24 md:*:min-h-0",
      button(
        tpe := "button",
        cls := s"$btnGhost justify-center disabled:opacity-50",
        "Annulla",
        disabled <-- state.map(_.importing),
        onClick --> (_ => onCancel()),
      ),
      button(
        tpe := "button",
        cls := s"$btnPrimary justify-center",
        disabled <-- state.map(s => !s.canImport || s.busy),
        aria.busy <-- state.map(_.importing),
        child.text <-- state.map(s => if s.importing then "Caricamento…" else "Carica"),
        onClick --> (_ => onUpload()),
      ),
    )

  private def instructions: HtmlElement =
    def column(name: String, text: String): HtmlElement =
      li(span(cls := "font-medium text-slate-700", name), if text.isEmpty then "" else s" — $text")
    div(
      cls := "text-sm text-slate-600",
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
      p(cls := "mt-3", "Regole:"),
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
   * A button-styled label around a visually hidden file input (it keeps the keyboard focus, shown as a ring on the label), with the chosen file name
   * next to it. The input is emptied after every choice, so choosing the same file again (e.g. after fixing it in Excel) checks it again.
   */
  private def filePicker(state: Var[MaterialImport.State[dom.File]], check: () => Unit): HtmlElement =
    div(
      cls := "flex flex-wrap items-center gap-3",
      label(
        cls := s"$btnGhost min-h-10 md:min-h-0 has-[:focus-visible]:ring-2 has-[:focus-visible]:ring-slate-500",
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
        cls := "min-w-0 truncate text-sm text-slate-500",
        child.text <-- state.signal.map(s => if s.fileName.isEmpty then "Nessun file scelto" else s.fileName),
      ),
    )

  private def renderPhase(phase: Phase): HtmlElement = phase match
    case Phase.Idle => div(cls := "text-sm text-slate-500", "Scegli il file: viene verificato subito, senza salvare niente.")
    case Phase.Checking => div(cls := "flex items-center gap-2 text-sm text-slate-500", spinner, "Verifica del file in corso…")
    case Phase.Checked(summary) => summaryView(summary)
    case Phase.Importing(summary) =>
      div(summaryView(summary), div(cls := "mt-2 flex items-center gap-2 text-sm text-slate-500", spinner, "Caricamento in corso…"))
    case Phase.Imported(summary) => div(cls := "text-sm text-slate-600", MaterialImport.confirmationText(summary))
    case Phase.Rejected(error) =>
      div(
        // The details can list hundreds of problems: they scroll inside the section.
        cls := "max-h-96 overflow-y-auto",
        Option.when(error.details.size > 1)(div(cls := "mb-1 text-xs text-slate-500", s"${error.details.size} problemi trovati")),
        errorBanner(error),
      )

  private def summaryView(summary: MaterialImportResponse): HtmlElement =
    div(
      cls := "rounded-md border border-slate-200 p-3 text-sm",
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
end MaterialImportView
