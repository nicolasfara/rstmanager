package io.github.nicolasfara.rstmanager.service

import io.github.nicolasfara.rstmanager.customer.service.{ CustomerApp, CustomerHttpApi }
import io.github.nicolasfara.rstmanager.hr.service.{ EmployeeApp, EmployeeHttpApi }
import io.github.nicolasfara.rstmanager.planning.service.{ PlanningApp, PlanningEntityGateway, PlanningRoutes }
import io.github.nicolasfara.rstmanager.work.service.{ ManufacturingApp, ManufacturingHttpApi, OrderApp, OrderHttpApi, TaskApp, TaskHttpApi }

import cats.effect.IO
import org.http4s.HttpRoutes
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter

/** Combines every context's endpoints into a single http4s app served under one Swagger document. */
object ApiServer:
  def routes(
      planningBackend: PlanningApp.PlanningBackend,
      employees: EmployeeApp.Store,
      customers: CustomerApp.Store,
      tasks: TaskApp.Store,
      manufacturings: ManufacturingApp.Store,
      orders: OrderApp.Store,
  ): HttpRoutes[IO] =
    val planningGateway = PlanningEntityGateway.fromStores(orders, employees)
    val apiEndpoints: List[ServerEndpoint[Any, IO]] =
      EmployeeHttpApi.routes(employees) ++
        CustomerHttpApi.routes(customers) ++
        TaskHttpApi.routes(tasks, manufacturings) ++
        ManufacturingHttpApi.routes(manufacturings, tasks) ++
        OrderHttpApi.routes(orders, customers, tasks, employees) ++
        PlanningRoutes.serverEndpoints(planningBackend, planningGateway)

    val documentationEndpoints = SwaggerInterpreter().fromServerEndpoints[IO](apiEndpoints, "RST Manager API", "0.1.0")

    Http4sServerInterpreter[IO]().toRoutes(apiEndpoints ++ documentationEndpoints)
  end routes
end ApiServer
