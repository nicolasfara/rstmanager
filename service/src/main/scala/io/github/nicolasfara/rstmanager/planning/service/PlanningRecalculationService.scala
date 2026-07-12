package io.github.nicolasfara.rstmanager.planning.service

import java.util.UUID

import io.github.nicolasfara.rstmanager.hr.domain.Employee
import io.github.nicolasfara.rstmanager.planning.{ PlanningError, PlanningRequest, PlanningTrigger }
import io.github.nicolasfara.rstmanager.planning.service.PlanningApiDtos.PlanningDomainErrorDto
import io.github.nicolasfara.rstmanager.work.domain.order.Order

import cats.data.NonEmptyChain
import cats.effect.IO
import com.github.nscala_time.time.Imports.DateTime

trait PlanningRecalculator:
  def recalculate(trigger: PlanningTrigger): IO[PlanningRecalculationResult]
  def recalculate(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult]

final case class PlanningRecalculationResult(commandId: Option[String], errors: List[String])

final case class PlanningRecalculationCause(trigger: PlanningTrigger, commandId: Option[String], requestId: Option[UUID])

object PlanningRecalculationCause:
  def manual(trigger: PlanningTrigger): PlanningRecalculationCause = PlanningRecalculationCause(trigger, None, None)

object PlanningRecalculationResult:
  val skipped: PlanningRecalculationResult = PlanningRecalculationResult(None, Nil)

object PlanningRecalculator:
  val noop: PlanningRecalculator = new PlanningRecalculator:
    override def recalculate(trigger: PlanningTrigger): IO[PlanningRecalculationResult] =
      recalculate(PlanningRecalculationCause.manual(trigger))

    override def recalculate(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult] =
      IO.pure(PlanningRecalculationResult.skipped)

object PlanningRecalculationService:
  type ComputePlan = (String, PlanningRequest, List[Order], List[Employee]) => IO[Either[NonEmptyChain[PlanningError], String]]

  def apply(backend: PlanningApp.PlanningBackend, gateway: PlanningEntityGateway): PlanningRecalculator =
    fromCompute(gateway) { (commandId, request, orders, employees) =>
      PlanningApp.computePlanWithCommandId(backend, commandId, request, orders, employees)
    }

  def fromCompute(gateway: PlanningEntityGateway)(compute: ComputePlan): PlanningRecalculator =
    DefaultPlanningRecalculator(gateway, compute)

  private final class DefaultPlanningRecalculator(gateway: PlanningEntityGateway, compute: ComputePlan) extends PlanningRecalculator:
    override def recalculate(trigger: PlanningTrigger): IO[PlanningRecalculationResult] =
      recalculate(PlanningRecalculationCause.manual(trigger))

    override def recalculate(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult] =
      run(cause).attempt.flatMap {
        case Right(result) => IO.pure(result)
        case Left(error) =>
          val message = s"Planning recalculation failed unexpectedly for ${cause.trigger}: $error"
          IO.println(message).as(PlanningRecalculationResult(None, List(message)))
      }

    private def run(cause: PlanningRecalculationCause): IO[PlanningRecalculationResult] =
      gateway.snapshot(None, None).flatMap {
        case Left(error) =>
          val messages = List(loadErrorMessage(error))
          IO.println(messages.mkString("; ")).as(PlanningRecalculationResult(None, messages))
        case Right(snapshot) =>
          val computed =
            for
              commandId <- IO(cause.commandId.getOrElse(UUID.randomUUID().nn.toString))
              request <- buildRequest(cause, snapshot)
              result <- compute(commandId, request, snapshot.orders, snapshot.employees).flatMap {
                case Left(errors) =>
                  val messages = errors.toChain.toList.map(PlanningDomainErrorDto.fromDomain(_).message)
                  IO.println(messages.mkString("; ")).as(PlanningRecalculationResult(None, messages))
                case Right(acceptedCommandId) =>
                  IO.pure(PlanningRecalculationResult(Some(acceptedCommandId), Nil))
              }
            yield result
          computed
      }

    private def buildRequest(cause: PlanningRecalculationCause, snapshot: PlanningEntityGateway.Snapshot): IO[PlanningRequest] =
      for
        requestId <- IO(cause.requestId.getOrElse(UUID.randomUUID().nn))
        requestedOn <- IO(DateTime.now().nn)
      yield PlanningRequest(
        requestId,
        requestedOn.withTimeAtStartOfDay().nn,
        cause.trigger,
        requestedOn,
        snapshot.orders.map(_.data.id),
      )

    private def loadErrorMessage(error: PlanningEntityGateway.LoadError): String =
      error match
        case PlanningEntityGateway.LoadError.UnknownOrders(ids) =>
          s"Planning recalculation could not load open orders: ${ids.mkString(", ")}"
        case PlanningEntityGateway.LoadError.UnknownEmployees(ids) =>
          s"Planning recalculation could not load employees: ${ids.mkString(", ")}"
end PlanningRecalculationService
