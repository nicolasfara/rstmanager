package io.github.nicolasfara.rstmanager.warehouse.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.service.auth.Role
import io.github.nicolasfara.rstmanager.service.http.{ ApiError, ApiSecurity, Secured }
import io.github.nicolasfara.rstmanager.warehouse.domain.*
import io.github.nicolasfara.rstmanager.warehouse.domain.material.MaterialId
import io.github.nicolasfara.rstmanager.warehouse.domain.supplier.SupplierId
import io.github.nicolasfara.rstmanager.warehouse.service.MaterialHttpApi.{ MaterialSupplierDto, MpnDto, StockDto }
import io.github.nicolasfara.rstmanager.warehouse.service.SupplierApp.StoredSupplier
import io.github.nicolasfara.rstmanager.work.service.OrderApp

import cats.data.ValidatedNec
import cats.effect.IO
import cats.syntax.all.*
import com.github.nscala_time.time.Imports.LocalDate
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import sttp.model.{ HeaderNames, MediaType }
import sttp.tapir.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint

/**
 * JSON DTOs, tapir endpoints, and http4s server logic for the warehouse reports (Viewer): the state of any past day (`/warehouse/state`) and its
 * Excel export (`/warehouse/export.xlsx`), the incoming and outgoing flows per week or month (`/warehouse/flows`), and the spend per supplier
 * (`/warehouse/supplier-spend`).
 *
 * The figures are computed by the pure [[WarehouseReports]] over every material, archived ones included; this layer only parses the period and names
 * materials and suppliers. Error messages are in Italian.
 */
object WarehouseReportsHttpApi:
  import ApiError.ApiFailure
  import WarehouseHttpSupport.*

  /** A material in the state of a day, with the customer price in effect on that day and its stock at the end of it. */
  final case class StateRowResponse(
      materialId: UUID,
      internalCode: String,
      description: String,
      materialType: Option[String],
      notes: Option[String],
      unit: String,
      customerPrice: Option[String],
      stock: StockDto,
      archived: Boolean,
      suppliers: List[MaterialSupplierDto],
      mpns: List[MpnDto],
  )

  object StateRowResponse:
    def fromDomain(row: StateRow, suppliers: Map[SupplierId, StoredSupplier]): StateRowResponse =
      val info = row.material.record.info
      StateRowResponse(
        info.id.value,
        info.internalCode,
        info.description,
        info.materialType,
        info.notes,
        info.unit.wireName,
        row.customerPrice.map(_.toDecimalString),
        StockDto.fromDomain(row.stock),
        row.archived,
        info.suppliers.map(MaterialSupplierDto.of(_, suppliers)),
        info.mpns.map(MpnDto.fromDomain),
      )

  final case class StateTotalsResponse(materials: Int, value: String, purchasedTotal: String, negativeMaterials: Int, unvaluedMaterials: Int)

  final case class WarehouseStateResponse(at: String, rows: List[StateRowResponse], totals: StateTotalsResponse)

  object WarehouseStateResponse:
    def fromDomain(state: WarehouseState, suppliers: Map[SupplierId, StoredSupplier]): WarehouseStateResponse =
      WarehouseStateResponse(
        formatDay(state.at),
        state.rows.map(StateRowResponse.fromDomain(_, suppliers)),
        StateTotalsResponse(
          state.totals.materials,
          state.totals.value.toDecimalString,
          state.totals.purchasedTotal.toDecimalString,
          state.totals.negativeMaterials,
          state.totals.unvaluedMaterials,
        ),
      )

  /** Quantities in and out (non-negative) and their difference, in one unit of measure. */
  final case class UnitFlowResponse(unit: String, quantityIn: String, quantityOut: String, net: String)

  /** Totals of a period: `net = inValue - outValue`; `purchaseValue` is the part of `inValue` bought from suppliers. */
  final case class FlowTotalsResponse(inValue: String, outValue: String, net: String, purchaseValue: String, quantities: List[UnitFlowResponse])

  object FlowTotalsResponse:
    def fromDomain(totals: FlowTotals): FlowTotalsResponse =
      FlowTotalsResponse(
        totals.inValue.toDecimalString,
        totals.outValue.toDecimalString,
        totals.net.toDecimalString,
        totals.purchaseValue.toDecimalString,
        totals.quantities.map(flow =>
          UnitFlowResponse(flow.unit.wireName, flow.quantityIn.toDecimalString, flow.quantityOut.toDecimalString, flow.net.toDecimalString),
        ),
      )

  /** One week (from Monday) or month (from the 1st): `from`/`to` are the days counted, clipped to the report period. */
  final case class FlowBucketResponse(start: String, label: String, from: String, to: String, totals: FlowTotalsResponse)

  final case class FlowReportResponse(from: String, to: String, granularity: String, buckets: List[FlowBucketResponse], totals: FlowTotalsResponse)

  object FlowReportResponse:
    def fromDomain(report: FlowReport): FlowReportResponse =
      FlowReportResponse(
        formatDay(report.from),
        formatDay(report.to),
        report.granularity.wireName,
        report.buckets.map(bucket =>
          FlowBucketResponse(
            formatDay(bucket.start),
            bucket.label,
            formatDay(bucket.from),
            formatDay(bucket.to),
            FlowTotalsResponse.fromDomain(bucket.totals),
          ),
        ),
        FlowTotalsResponse.fromDomain(report.totals),
      )

  /** What was bought under one MPN of one material. */
  final case class MpnSpendResponse(
      materialId: UUID,
      internalCode: String,
      description: String,
      mpnCode: String,
      unit: String,
      quantity: String,
      amount: String,
      purchases: Int,
  )

  /** What was bought from one supplier (`active` is false once the supplier is deleted). */
  final case class SupplierSpendResponse(
      supplierId: UUID,
      name: String,
      active: Boolean,
      amount: String,
      purchases: Int,
      mpns: List[MpnSpendResponse],
  )

  final case class SupplierSpendReportResponse(from: String, to: String, total: String, suppliers: List[SupplierSpendResponse])

  object SupplierSpendReportResponse:
    def fromDomain(
        report: SupplierSpendReport,
        materials: Map[MaterialId, ReportedMaterial],
        suppliers: Map[SupplierId, StoredSupplier],
    ): SupplierSpendReportResponse =
      SupplierSpendReportResponse(
        formatDay(report.from),
        formatDay(report.to),
        report.total.toDecimalString,
        report.suppliers.map { spend =>
          val supplier = MaterialSupplierDto.of(spend.supplierId, suppliers)
          SupplierSpendResponse(
            supplier.id,
            supplier.name,
            supplier.active,
            spend.amount.toDecimalString,
            spend.purchases,
            spend.mpns.map { mpn =>
              val info = materials.get(mpn.materialId).map(_.record.info)
              MpnSpendResponse(
                mpn.materialId.value,
                info.fold(mpn.materialId.value.toString)(material => material.internalCode: String),
                info.fold("")(material => material.description: String),
                mpn.mpnCode,
                mpn.unit.wireName,
                mpn.quantity.toDecimalString,
                mpn.amount.toDecimalString,
                mpn.purchases,
              )
            },
          )
        },
      )
  end SupplierSpendReportResponse

  given Codec[StateRowResponse] = deriveCodec
  given Codec[StateTotalsResponse] = deriveCodec
  given Codec[WarehouseStateResponse] = deriveCodec
  given Codec[UnitFlowResponse] = deriveCodec
  given Codec[FlowTotalsResponse] = deriveCodec
  given Codec[FlowBucketResponse] = deriveCodec
  given Codec[FlowReportResponse] = deriveCodec
  given Codec[MpnSpendResponse] = deriveCodec
  given Codec[SupplierSpendResponse] = deriveCodec
  given Codec[SupplierSpendReportResponse] = deriveCodec
  given Schema[StateRowResponse] = Schema.derived
  given Schema[StateTotalsResponse] = Schema.derived
  given Schema[WarehouseStateResponse] = Schema.derived
  given Schema[UnitFlowResponse] = Schema.derived
  given Schema[FlowTotalsResponse] = Schema.derived
  given Schema[FlowBucketResponse] = Schema.derived
  given Schema[FlowReportResponse] = Schema.derived
  given Schema[MpnSpendResponse] = Schema.derived
  given Schema[SupplierSpendResponse] = Schema.derived
  given Schema[SupplierSpendReportResponse] = Schema.derived

  val state: Secured.SecuredEndpoint[Option[String], WarehouseStateResponse] =
    Secured.base.get
      .in("warehouse" / "state")
      .in(query[Option[String]]("at").description("Day, YYYY-MM-DD, not after today (default today)."))
      .tag("Warehouse")
      .summary("The warehouse as it stood at the end of a day")
      .out(jsonBody[WarehouseStateResponse])

  val flows: Secured.SecuredEndpoint[(Option[String], Option[String], Option[String]), FlowReportResponse] =
    Secured.base.get
      .in("warehouse" / "flows")
      .in(query[Option[String]]("from").description("First day, YYYY-MM-DD (required)."))
      .in(query[Option[String]]("to").description(s"Last day, YYYY-MM-DD (required; less than ${maxReportYears} years after from)."))
      .in(query[Option[String]]("granularity").description("week (ISO, from Monday) or month (default month)."))
      .tag("Warehouse")
      .summary("Incoming and outgoing stock value per week or month")
      .out(jsonBody[FlowReportResponse])

  val supplierSpend: Secured.SecuredEndpoint[(Option[String], Option[String]), SupplierSpendReportResponse] =
    Secured.base.get
      .in("warehouse" / "supplier-spend")
      .in(query[Option[String]]("from").description("First day, YYYY-MM-DD (required)."))
      .in(query[Option[String]]("to").description(s"Last day, YYYY-MM-DD (required; less than ${maxReportYears} years after from)."))
      .tag("Warehouse")
      .summary("What was bought from each supplier, per MPN")
      .out(jsonBody[SupplierSpendReportResponse])

  /** The `.xlsx` content type as a tapir codec format, so that the body is documented and served with it. */
  private object XlsxFormat extends CodecFormat:
    override val mediaType: MediaType = MediaType.unsafeParse(WarehouseExport.mediaType)

  /** The bytes of an `.xlsx` file, served as [[WarehouseExport.mediaType]]. */
  private val xlsxBody: EndpointIO.Body[Array[Byte], Array[Byte]] =
    EndpointIO.Body(RawBodyType.ByteArrayBody, sttp.tapir.Codec.byteArray.format(XlsxFormat), EndpointIO.Info.empty)

  /** The workbook (bytes) and its `Content-Disposition` header (`attachment; filename="magazzino-YYYY-MM-DD.xlsx"`). */
  val exportXlsx: Secured.SecuredEndpoint[Option[String], (Array[Byte], String)] =
    Secured.base.get
      .in("warehouse" / "export.xlsx")
      .in(query[Option[String]]("at").description("Day, YYYY-MM-DD, not after today (default today)."))
      .tag("Warehouse")
      .summary("Excel workbook of the warehouse at the end of a day, with the movements up to it")
      .out(xlsxBody.description("Sheets Magazzino (state and totals), Movimenti (movements not voided up to the day), Info."))
      .out(header[String](HeaderNames.ContentDisposition))

  def endpoints: List[AnyEndpoint] = List(state, exportXlsx, flows, supplierSpend)

  def routes(
      materials: MaterialApp.Store,
      suppliers: SupplierApp.Store,
      orders: OrderApp.Store,
      security: ApiSecurity,
  ): List[ServerEndpoint[Any, IO]] =
    List(
      security.secure(state, Role.Viewer)(stateLogic(materials, suppliers)),
      security.secure(exportXlsx, Role.Viewer)(exportLogic(materials, suppliers, orders)),
      security.secure(flows, Role.Viewer)(flowsLogic(materials)),
      security.secure(supplierSpend, Role.Viewer)(supplierSpendLogic(materials, suppliers)),
    )

  /** The day of a state or of an export: `at`, or `today` when absent; never after `today`. */
  def parseStateDay(at: Option[String], today: LocalDate): ValidatedNec[String, LocalDate] =
    parseOptionalDay(at, "Giorno").andThen {
      case Some(requested) if requested.isAfter(today) =>
        s"Giorno: la data ${displayDay(requested)} è successiva a oggi (${displayDay(today)})".invalidNec
      case requested => requested.getOrElse(today).validNec
    }

  /** `attachment; filename="name"`: the browser saves the response as `name`. */
  def contentDisposition(fileName: String): String = s"attachment; filename=\"$fileName\""

  private def stateLogic(materials: MaterialApp.Store, suppliers: SupplierApp.Store)(
      at: Option[String],
  ): IO[Either[ApiFailure, WarehouseStateResponse]] =
    today.flatMap { day =>
      parseStateDay(at, day).toEither match
        case Left(errors) => IO.pure(validation(errors).asLeft)
        case Right(requested) =>
          report(materials) { reported =>
            val warehouse = WarehouseReports.stateAt(reported, requested)
            MaterialHttpApi
              .suppliersById(suppliers, warehouse.rows.flatMap(_.material.record.info.suppliers))
              .map(WarehouseStateResponse.fromDomain(warehouse, _))
          }
    }

  /** The workbook of the state at the end of `at` (default today), with the movements up to that day, as an attachment. */
  private def exportLogic(materials: MaterialApp.Store, suppliers: SupplierApp.Store, orders: OrderApp.Store)(
      at: Option[String],
  ): IO[Either[ApiFailure, (Array[Byte], String)]] =
    IO.realTimeInstant.flatMap { now =>
      parseStateDay(at, BusinessCalendar.day(now)).toEither match
        case Left(errors) => IO.pure(validation(errors).asLeft)
        case Right(day) =>
          report(materials) { reported =>
            val warehouse = WarehouseReports.stateAt(reported, day)
            val movements = WarehouseExport.movementsUntil(reported, day)
            for
              named <- MaterialHttpApi.suppliersById(
                suppliers,
                warehouse.rows.flatMap(_.material.record.info.suppliers) ++ movements.flatMap(_.supplierId),
              )
              labels <- orderLabels(orders, movements.map(_.movement))
              bytes <- WarehouseExport.write(WarehouseExport.workbook(warehouse, movements, named, labels, now))
            yield (bytes, contentDisposition(WarehouseExport.fileName(day)))
          }
    }

  private def flowsLogic(materials: MaterialApp.Store)(
      from: Option[String],
      to: Option[String],
      granularity: Option[String],
  ): IO[Either[ApiFailure, FlowReportResponse]] =
    val parsed: ValidatedNec[String, ((LocalDate, LocalDate), FlowGranularity)] =
      (
        parsePeriod(from, to),
        granularity
          .map(_.trim.nn)
          .filter(_.nonEmpty)
          .fold(FlowGranularity.Month.validNec)(raw => FlowGranularity.fromWireName(raw).leftMap(error => s"Raggruppa per: $error").toValidatedNec),
      ).tupled
    parsed.toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right(((start, end), parsedGranularity)) =>
        report(materials)(reported => IO.pure(FlowReportResponse.fromDomain(WarehouseReports.flows(reported, start, end, parsedGranularity))))

  private def supplierSpendLogic(materials: MaterialApp.Store, suppliers: SupplierApp.Store)(
      from: Option[String],
      to: Option[String],
  ): IO[Either[ApiFailure, SupplierSpendReportResponse]] =
    parsePeriod(from, to).toEither match
      case Left(errors) => IO.pure(validation(errors).asLeft)
      case Right((start, end)) =>
        report(materials) { reported =>
          val spend = WarehouseReports.supplierSpend(reported, start, end)
          MaterialHttpApi
            .suppliersById(suppliers, spend.suppliers.map(_.supplierId))
            .map(SupplierSpendReportResponse.fromDomain(spend, reported.map(material => material.record.info.id -> material).toMap, _))
        }

  /** Runs `build` on every material, archived ones included; unexpected failures become a 500. */
  private def report[R](materials: MaterialApp.Store)(build: List[ReportedMaterial] => IO[R]): IO[Either[ApiFailure, R]] =
    MaterialApp.listAll(materials).flatMap(stored => build(stored.map(_.reported))).attempt.map(_.leftMap(internal))
end WarehouseReportsHttpApi
