package io.gitbub.nicolasfara.rstmanager.ui.warehouse

import java.util.UUID

import io.gitbub.nicolasfara.rstmanager.Equality.given
import io.gitbub.nicolasfara.rstmanager.api.Dtos.*
import io.gitbub.nicolasfara.rstmanager.ui.SearchTerms
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import DeliveryNoteForm.*

/**
 * Unit tests for the pure delivery note form: line options per supplier, search, expected price prefill and override, new MPNs, supplier change,
 * totals, validation messages, request building, and editing an existing note.
 */
final class DeliveryNoteFormSpec extends AnyFunSuite with Matchers:

  // strictEquality: `shouldBe` compares these DTOs structurally, so it needs CanEqual instances.
  private given CanEqual[DeliveryNoteRequest, DeliveryNoteRequest] = CanEqual.derived
  private given CanEqual[DeliveryNoteLineRequest, DeliveryNoteLineRequest] = CanEqual.derived

  private def uuid(s: String): UUID = UUID.fromString(s).nn

  private val supplierA = uuid("00000000-0000-0000-0000-0000000000a1")
  private val supplierB = uuid("00000000-0000-0000-0000-0000000000b2")
  private val today = "2026-11-01"

  private def material(id: String, code: String, description: String, unit: String, mpns: List[MpnDto]): MaterialResponse =
    MaterialResponse(
      uuid(id),
      code,
      description,
      None,
      None,
      unit,
      "10.0000",
      mpns,
      Nil,
      "2026-01-01",
      false,
      None,
      StockDto("0.000", None, None, "0.00", false),
    )

  private val tes = material(
    "00000000-0000-0000-0000-0000000000f1",
    "TES-001",
    "Tessuto blu navy",
    "linear_meters",
    List(MpnDto("AC-152", supplierA, None, Some("3.5000")), MpnDto("X9", supplierB, None, None)),
  )
  private val vit = material(
    "00000000-0000-0000-0000-0000000000f2",
    "VIT-010",
    "Vite inox M6",
    "pieces",
    List(MpnDto("VM6", supplierA, None, Some("0.1200")), MpnDto("VM6-B", supplierA, None, None), MpnDto("VM6-B", supplierB, None, Some("0.1500"))),
  )
  private val cor = material("00000000-0000-0000-0000-0000000000f3", "COR-005", "Corda nylon", "linear_meters", Nil)
  private val old =
    material("00000000-0000-0000-0000-0000000000f4", "OLD-001", "Vecchio tessuto", "linear_meters", List(MpnDto("OLD", supplierA, None, None)))
      .copy(archived = true, archivedOn = Some("2026-06-01"))

  private val all = List(vit, old, tes, cor)
  private val ctx = Context.of(all, Set.empty)

  private val tesA = existingKey(tes.id, "AC-152")
  private val tesB = existingKey(tes.id, "X9")
  private val vitA = existingKey(vit.id, "VM6")
  private val vitAB = existingKey(vit.id, "VM6-B")
  private val corNew = newMpnKey(cor.id)

  /** A form for supplier A with the given rows (keys from 0). */
  private def withRows(rows: Row*): State =
    DeliveryNoteForm.empty(today).copy(number = "123", supplierId = supplierA.toString, rows = rows.toList, nextKey = rows.size)

  private def row(key: Int, choice: String, quantity: String, cost: String, touched: Boolean): Row = Row(key, choice, "", quantity, cost, touched)

  // ---- options per supplier ----------------------------------------------------------------------

  test("each MPN of the supplier is an option, plus a new MPN option for active materials without one, sorted by code"):
    ctx.options(supplierA.toString).map(o => o.key -> o.label) shouldBe List(
      corNew -> "COR-005 — Corda nylon · nuovo MPN",
      tesA -> "TES-001 — Tessuto blu navy · MPN AC-152",
      vitA -> "VIT-010 — Vite inox M6 · MPN VM6",
      vitAB -> "VIT-010 — Vite inox M6 · MPN VM6-B",
    )
    ctx.options(supplierB.toString).map(_.key) shouldBe List(corNew, tesB, vitAB)

  test("options carry the material unit and the expected price of the MPN"):
    val tesOption = ctx.option(supplierA.toString, tesA)
    tesOption.map(o => (o.materialId, o.unit, o.mpnCode, o.expectedUnitCost, o.isNewMpn)) shouldBe
      Some((tes.id, "linear_meters", Some("AC-152"), Some("3.5000"), false))
    ctx.option(supplierA.toString, corNew).map(o => (o.mpnCode, o.expectedUnitCost, o.isNewMpn)) shouldBe Some((None, None, true))

  test("no supplier selected means no options, and an option of another supplier is not one of this supplier"):
    ctx.options("") shouldBe Nil
    ctx.option(supplierA.toString, tesB) shouldBe None
    ctx.option("", tesA) shouldBe None

  test("archived materials are offered only when kept, under their existing MPNs and never as a new MPN"):
    ctx.options(supplierA.toString).exists(_.materialId == old.id) shouldBe false
    val kept = Context.of(all, Set(old.id))
    kept.options(supplierA.toString).filter(_.materialId == old.id).map(_.label) shouldBe List("OLD-001 — Vecchio tessuto · MPN OLD (archiviato)")
    kept.options(supplierB.toString).exists(_.materialId == old.id) shouldBe false

  // ---- search ------------------------------------------------------------------------------------

  private def search(query: String, supplier: UUID): List[String] =
    ctx.selectOptions(supplier.toString).collect { case (key, _, text) if SearchTerms.matches(query, text) => key }

  test("a line is found by internal code, description, or any MPN code of the material, term by term"):
    search("tes-001", supplierA) shouldBe List(tesA)
    search("NYLON", supplierA) shouldBe List(corNew)
    search("ac-152", supplierA) shouldBe List(tesA)
    // X9 is an MPN of supplier B, yet it still finds the material among the options of supplier A.
    search("x9", supplierA) shouldBe List(tesA)
    search("vite m6", supplierA) shouldBe List(vitA, vitAB)
    search("vm6-b inox", supplierB) shouldBe List(vitAB)
    search("blu vite", supplierA) shouldBe Nil
    search("  ", supplierB) shouldBe List(corNew, tesB, vitAB)

  // ---- expected price ----------------------------------------------------------------------------

  test("choosing an option prefills the cost with its expected price"):
    val chosen = withRows(Row.empty(0)).choose(0, tesA, ctx)
    chosen.rows shouldBe List(Row(0, tesA, "", "", "3,50", false))
    chosen.priceNote(chosen.rows.head, ctx) shouldBe Some("Prezzo previsto")

  test("a typed cost is kept when choosing another option, and the note tells it from the expected price"):
    val typed = withRows(Row.empty(0)).choose(0, tesA, ctx).setCost(0, "4")
    typed.priceNote(typed.rows.head, ctx) shouldBe Some("Modificato (previsto 3,50 €)")
    val rechosen = typed.choose(0, vitA, ctx)
    rechosen.rows.head.unitCost shouldBe "4"
    rechosen.rows.head.costTouched shouldBe true
    // Typing the expected price again reads as the expected price.
    val same = typed.setCost(0, "3,5000")
    same.priceNote(same.rows.head, ctx) shouldBe Some("Prezzo previsto")

  test("an untouched cost follows the option, and is cleared for an MPN without an expected price"):
    val prefilled = withRows(Row.empty(0)).choose(0, vitA, ctx)
    prefilled.rows.head.unitCost shouldBe "0,12"
    val noPrice = prefilled.choose(0, vitAB, ctx)
    noPrice.rows.head.unitCost shouldBe ""
    noPrice.priceNote(noPrice.rows.head, ctx) shouldBe Some("Nessun prezzo previsto: indica il costo")
    noPrice.copy(rows = noPrice.rows.map(_.copy(quantity = "3"))).errors(ctx) shouldBe List("Riga 1: Costo unitario obbligatorio")

  test("clearing a typed cost lets the next choice prefill again"):
    val cleared = withRows(Row.empty(0)).choose(0, tesA, ctx).setCost(0, "9").setCost(0, " ")
    cleared.rows.head.costTouched shouldBe false
    cleared.choose(0, vitA, ctx).rows.head.unitCost shouldBe "0,12"

  test("a blank cost falls back to the expected price"):
    val state = withRows(row(0, tesA, "2", "", false))
    state.priceNote(state.rows.head, ctx) shouldBe Some("Prezzo previsto")
    state.request(ctx).map(_.lines) shouldBe Some(List(DeliveryNoteLineRequest(tes.id, "AC-152", false, "2.000", "3.5000")))

  // ---- new MPN -----------------------------------------------------------------------------------

  test("a new MPN requires its code and is sent with newMpn"):
    val chosen = withRows(Row.empty(0)).choose(0, corNew, ctx).updateRow(0)(_.copy(quantity = "10", unitCost = "1,2"))
    chosen.priceNote(chosen.rows.head, ctx) shouldBe Some("Nuovo MPN: il costo diventa il suo prezzo previsto")
    chosen.errors(ctx) shouldBe List("Riga 1: Indica il codice del nuovo MPN")
    val coded = chosen.updateRow(0)(_.copy(newMpnCode = " cn-1 "))
    coded.request(ctx).map(_.lines) shouldBe Some(List(DeliveryNoteLineRequest(cor.id, "cn-1", true, "10.000", "1.2000")))

  test("a new MPN code the supplier already uses for another material is reported"):
    val clash = withRows(Row(0, corNew, "ac-152", "1", "1", true))
    clash.errors(ctx) shouldBe List("Riga 1: L'MPN AC-152 di questo fornitore è già del materiale TES-001")
    withRows(Row(0, corNew, "X9", "1", "1", true)).errors(ctx) shouldBe Nil

  // ---- supplier change ---------------------------------------------------------------------------

  test("changing supplier moves every line onto the new supplier's MPNs of the same material"):
    val state = withRows(
      row(0, tesA, "1", "3,50", false),
      row(1, vitAB, "2", "0,2", true),
      row(2, vitA, "3", "0,12", false),
      Row(3, corNew, "CN-1", "4", "", false),
      Row.empty(4),
    )
    val moved = state.withSupplier(supplierB.toString, ctx)
    moved.supplierId shouldBe supplierB.toString
    moved.rows shouldBe List(
      // X9 has no expected price: the untouched cost is cleared.
      row(0, tesB, "1", "", false),
      // Same code under the new supplier; the typed cost is kept.
      row(1, vitAB, "2", "0,2", true),
      // VM6 is not an MPN of B: the first MPN of B for the material, with its expected price.
      row(2, vitAB, "3", "0,15", false),
      Row(3, corNew, "CN-1", "4", "", false),
      Row.empty(4),
    )
    moved.withSupplier(supplierA.toString, ctx).rows.head shouldBe row(0, tesA, "1", "3,50", false)

  test("a material without any option for the new supplier loses its choice; the same supplier changes nothing"):
    val kept = Context.of(all, Set(old.id))
    val state = withRows(row(0, existingKey(old.id, "OLD"), "1", "2", true))
    state.withSupplier(supplierB.toString, kept).rows shouldBe List(row(0, "", "1", "2", true))
    state.withSupplier(supplierA.toString, kept) shouldBe state

  test("a material without MPNs of the new supplier moves to its new MPN option"):
    val pin = material("00000000-0000-0000-0000-0000000000f5", "PIN-001", "Perno", "pieces", List(MpnDto("P1", supplierA, None, Some("1.0000"))))
    val withPin = Context.of(pin :: all, Set.empty)
    val moved = withRows(row(0, existingKey(pin.id, "P1"), "2", "1,00", false)).withSupplier(supplierB.toString, withPin)
    moved.rows shouldBe List(row(0, newMpnKey(pin.id), "2", "", false))
    moved.optionOf(moved.rows.head, withPin).map(_.isNewMpn) shouldBe Some(true)

  // ---- totals ------------------------------------------------------------------------------------

  test("line totals and the note total follow the readable lines"):
    val state = withRows(row(0, tesA, "2,5", "3,50", false), row(1, vitA, "3", "0,12", false), row(2, vitAB, "x", "1", true))
    state.rows.map(state.lineTotal(_, ctx)) shouldBe List(Some("8.75"), Some("0.36"), None)
    state.total(ctx) shouldBe "9.11"
    state.unitOf(state.rows(1), ctx) shouldBe Some("pieces")
    DeliveryNoteForm.empty(today).total(ctx) shouldBe "0.00"

  // ---- validation and request --------------------------------------------------------------------

  test("the empty form reports the header and the missing lines, and is pristine"):
    val fresh = DeliveryNoteForm.empty(today)
    fresh.pristine shouldBe true
    fresh.date shouldBe today
    fresh.errors(ctx) shouldBe List("Numero DDT obbligatorio", "Seleziona il fornitore", "Aggiungi almeno una riga")
    fresh.copy(number = "1").pristine shouldBe false

  test("the day must be valid and not after today"):
    val state = withRows(row(0, tesA, "1", "1", true))
    state.copy(date = "2026-11-02").errors(ctx) shouldBe List("La data non può essere successiva a oggi")
    state.copy(date = "").errors(ctx) shouldBe List("Data obbligatoria")
    state.copy(date = "2026-02-30").errors(ctx) shouldBe List("Data non valida")

  test("line errors carry the 1-based row position, blank rows are skipped"):
    val state = withRows(
      Row(0, "", "", "2", "", false),
      Row.empty(1),
      row(2, vitA, "2,5", "0,12", false),
      row(3, tesA, "", "abc", true),
    )
    state.errors(ctx) shouldBe List(
      "Riga 1: Seleziona il materiale",
      "Riga 1: Costo unitario obbligatorio",
      "Riga 3: Il materiale è gestito a pezzi: la quantità deve essere un numero intero",
      "Riga 4: Quantità obbligatoria",
      "Riga 4: Costo unitario non valido (es. 3,50, al massimo 4 decimali)",
    )
    state.request(ctx) shouldBe None

  test("a choice that is not an option of the supplier is reported"):
    withRows(row(0, tesB, "1", "1", true)).errors(ctx) shouldBe List("Riga 1: Il materiale selezionato non è disponibile per questo fornitore")

  test("request trims the header, drops a blank note, and keeps the line order"):
    val state = withRows(row(0, vitA, "3", "0,12", false), Row.empty(1), row(2, tesA, "2,5", "4", true))
      .copy(number = " 1234/A ", date = "2026-10-30", note = "  ")
    state.errors(ctx) shouldBe Nil
    state.request(ctx) shouldBe Some(
      DeliveryNoteRequest(
        "1234/A",
        "2026-10-30",
        supplierA,
        None,
        List(DeliveryNoteLineRequest(vit.id, "VM6", false, "3.000", "0.1200"), DeliveryNoteLineRequest(tes.id, "AC-152", false, "2.500", "4.0000")),
      ),
    )
    state.copy(note = " consegna parziale ").request(ctx).flatMap(_.note) shouldBe Some("consegna parziale")

  test("addRow mints increasing keys; removeRow targets a key"):
    val three = DeliveryNoteForm.empty(today).addRow.addRow
    three.rows.map(_.key) shouldBe List(0, 1, 2)
    three.removeRow(1).addRow.rows.map(_.key) shouldBe List(0, 2, 3)

  // ---- editing an existing delivery note ---------------------------------------------------------

  private val noteId = uuid("00000000-0000-0000-0000-0000000000d1")

  private def line(m: MaterialResponse, mpn: String, quantity: String, cost: String): DeliveryNoteLineResponse =
    DeliveryNoteLineResponse(m.id, m.internalCode, m.description, m.unit, mpn, quantity, cost, "0.00", true)

  private val response = DeliveryNoteResponse(
    noteId,
    "77",
    "2026-10-15",
    supplierA,
    "Tessiture Rossi",
    true,
    Some("Consegna parziale"),
    "registered",
    "2026-10-15",
    None,
    None,
    None,
    List(line(tes, "AC-152", "2.500", "3.5000"), line(vit, "VM6", "4.000", "0.1000"), line(cor, "CN-1", "1.000", "2.0000")),
    "11.15",
  )

  test("fromResponse picks the MPN options, recreating a code the material no longer has as a new MPN"):
    val state = DeliveryNoteForm.fromResponse(response, ctx, today)
    state.editingId shouldBe Some(noteId)
    state.pristine shouldBe false
    state.rows shouldBe List(
      // Equal to the expected price: untouched, so picking another option updates it.
      Row(0, tesA, "", "2,5", "3,50", false),
      Row(1, vitA, "", "4", "0,10", true),
      Row(2, corNew, "CN-1", "1", "2,00", true),
    )
    state.addRow.rows.map(_.key) shouldBe List(0, 1, 2, 3)

  test("fromResponse -> request round-trips the stored delivery note"):
    DeliveryNoteForm.fromResponse(response, ctx, today).request(ctx) shouldBe Some(
      DeliveryNoteRequest(
        "77",
        "2026-10-15",
        supplierA,
        Some("Consegna parziale"),
        List(
          DeliveryNoteLineRequest(tes.id, "AC-152", false, "2.500", "3.5000"),
          DeliveryNoteLineRequest(vit.id, "VM6", false, "4.000", "0.1000"),
          DeliveryNoteLineRequest(cor.id, "CN-1", true, "1.000", "2.0000"),
        ),
      ),
    )

  test("the supplier select lists the registry by name and keeps a deleted supplier of the note"):
    val suppliers =
      List(SupplierResponse(supplierB, "Zeta", None, None, None, None, None), SupplierResponse(supplierA, "alfa", None, None, None, None, None))
    DeliveryNoteForm.supplierOptions(suppliers, None) shouldBe List(supplierA.toString -> "alfa", supplierB.toString -> "Zeta")
    val gone = uuid("00000000-0000-0000-0000-0000000000c3")
    DeliveryNoteForm.supplierOptions(suppliers, Some(gone -> "Vecchio")).lastOption shouldBe Some(gone.toString -> "Vecchio (eliminato)")
    DeliveryNoteForm.supplierOptions(suppliers, Some(supplierA -> "alfa")).size shouldBe 2
end DeliveryNoteFormSpec
