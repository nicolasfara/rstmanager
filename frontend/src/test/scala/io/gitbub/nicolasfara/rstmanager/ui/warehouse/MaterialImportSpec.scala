package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import io.gitbub.nicolasfara.rstmanager.api.Dtos.{ ApiError, MaterialImportResponse }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import MaterialImport.*

/** Unit tests for the pure state of the material import dialog: check, stale responses, upload gating, and the summary texts. */
final class MaterialImportSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares states structurally, so it needs CanEqual instances.
  private given CanEqual[State[String], State[String]] = CanEqual.derived

  private val summary = MaterialImportResponse(true, List("A", "B"), List("C"), List("Nautea"), 3)
  private val nothingNew = MaterialImportResponse(true, Nil, List("A", "B"), Nil, 0)
  private val rejected = ApiError(rejectedCode, "Caricamento annullato: nessun materiale è stato salvato.", List("Riga 2, CODICE: obbligatorio"))

  private val start: State[String] = initial[String]

  test("a fresh state has no file and allows nothing"):
    start.phase shouldBe Phase.Idle
    start.busy shouldBe false
    start.canImport shouldBe false
    start.startImport shouldBe None

  test("choosing an xlsx file starts its check"):
    val chosen = start.choose("file-1", "materiali.xlsx")
    chosen.phase shouldBe Phase.Checking
    chosen.needsCheck shouldBe true
    chosen.busy shouldBe true
    chosen.canImport shouldBe false
    chosen.file shouldBe Some("file-1")
    chosen.fileName shouldBe "materiali.xlsx"
    start.choose("f", " MATERIALI.XLSX ").phase shouldBe Phase.Checking

  test("a file that is not xlsx is refused without a check"):
    val chosen = start.choose("file-1", "materiali.xls")
    chosen.needsCheck shouldBe false
    chosen.phase match
      case Phase.Rejected(error) => error.code shouldBe "invalid-file"
      case other => fail(s"unexpected phase $other")

  test("a successful check enables the upload, a failed one does not"):
    val chosen = start.choose("file-1", "a.xlsx")
    val ok = chosen.checked(chosen.selection, Right(summary))
    ok.phase shouldBe Phase.Checked(summary)
    ok.canImport shouldBe true
    ok.busy shouldBe false
    val ko = chosen.checked(chosen.selection, Left(rejected))
    ko.phase shouldBe Phase.Rejected(rejected)
    ko.canImport shouldBe false

  test("a check that leaves some materials out still enables the upload of the others"):
    val chosen = start.choose("file-1", "a.xlsx")
    val partial = summary.copy(ignored = List("X"), problems = List("Riga 5, PREZZO CLIENTE: obbligatorio"))
    val checked = chosen.checked(chosen.selection, Right(partial))
    checked.canImport shouldBe true
    checked.startImport.map(_._2) shouldBe Some("file-1")

  test("a check where every material has problems does not enable the upload"):
    val chosen = start.choose("file-1", "a.xlsx")
    val allIgnored = MaterialImportResponse(true, Nil, Nil, Nil, 0, List("X", "Y"), List("Riga 2, CODICE: …", "Riga 3, CODICE: …"))
    chosen.checked(chosen.selection, Right(allIgnored)).canImport shouldBe false

  test("a check that would create nothing does not enable the upload"):
    val chosen = start.choose("file-1", "a.xlsx")
    val checked = chosen.checked(chosen.selection, Right(nothingNew))
    checked.canImport shouldBe false
    checked.startImport shouldBe None

  test("the check of a file replaced meanwhile is dropped"):
    val first = start.choose("file-1", "a.xlsx")
    val second = first.choose("file-2", "b.xlsx")
    second.checked(first.selection, Right(summary)) shouldBe second
    second.checked(second.selection, Right(summary)).phase shouldBe Phase.Checked(summary)

  test("choosing another file after a check requires a new check"):
    val first = start.choose("file-1", "a.xlsx")
    val checked = first.checked(first.selection, Right(summary))
    val again = checked.choose("file-2", "b.xlsx")
    again.canImport shouldBe false
    again.phase shouldBe Phase.Checking

  test("the upload sends the checked file and locks the dialog until it answers"):
    val chosen = start.choose("file-1", "a.xlsx")
    val checked = chosen.checked(chosen.selection, Right(summary))
    val (importing, file) = checked.startImport.getOrElse(fail("upload not allowed"))
    file shouldBe "file-1"
    importing.phase shouldBe Phase.Importing(summary)
    importing.busy shouldBe true
    importing.importing shouldBe true
    importing.canImport shouldBe false
    importing.startImport shouldBe None
    importing.choose("file-2", "b.xlsx") shouldBe importing
    val done = summary.copy(dryRun = false)
    importing.imported(importing.selection, Right(done)).phase shouldBe Phase.Imported(done)
    importing.imported(importing.selection, Left(rejected)).phase shouldBe Phase.Rejected(rejected)

  test("reset forgets the file and drops late responses"):
    val chosen = start.choose("file-1", "a.xlsx")
    val reset = chosen.reset
    reset.phase shouldBe Phase.Idle
    reset.file shouldBe None
    reset.fileName shouldBe ""
    reset.checked(chosen.selection, Right(summary)) shouldBe reset
    reset.choose("file-2", "b.xlsx").selection should be > chosen.selection

  test("summary and confirmation texts agree in number"):
    summaryText(summary) shouldBe "2 materiali nuovi, 3 MPN"
    summaryText(MaterialImportResponse(true, List("A"), Nil, Nil, 1)) shouldBe "1 materiale nuovo, 1 MPN"
    confirmationText(summary) shouldBe "Caricati 2 materiali (3 MPN). Creato 1 fornitore. 1 codice già presente saltato."
    confirmationText(MaterialImportResponse(false, List("A"), List("B", "C"), List("X", "Y"), 0)) shouldBe
      "Caricato 1 materiale (0 MPN). Creati 2 fornitori. 2 codici già presenti saltati."
    confirmationText(MaterialImportResponse(false, List("A", "B"), Nil, Nil, 2)) shouldBe "Caricati 2 materiali (2 MPN)."

  test("ignored materials are counted in the warning and in the confirmation"):
    val one = summary.copy(ignored = List("X"))
    val many = summary.copy(created = List("A"), mpns = 1, newSuppliers = Nil, skipped = Nil, ignored = List("X", "Y", "Z"))
    ignoredText(one) shouldBe "1 materiale non verrà caricato per errori"
    ignoredText(many) shouldBe "3 materiali non verranno caricati per errori"
    confirmationText(one) shouldBe
      "Caricati 2 materiali (3 MPN). Creato 1 fornitore. 1 codice già presente saltato. 1 materiale ignorato per errori."
    confirmationText(many) shouldBe "Caricato 1 materiale (1 MPN). 3 materiali ignorati per errori."
end MaterialImportSpec
