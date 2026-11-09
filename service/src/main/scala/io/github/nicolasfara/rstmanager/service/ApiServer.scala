package io.github.nicolasfara.rstmanager.service

import io.github.nicolasfara.rstmanager.customer.service.{ CustomerApp, CustomerHttpApi }
import io.github.nicolasfara.rstmanager.hr.service.{ CompanyCalendarApp, CompanyCalendarHttpApi, EmployeeApp, EmployeeHttpApi }
import io.github.nicolasfara.rstmanager.planning.service.{ PlanningApp, PlanningEntityGateway, PlanningRoutes }
import io.github.nicolasfara.rstmanager.service.http.ApiSecurity
import io.github.nicolasfara.rstmanager.warehouse.service.{
  DeliveryNoteApp,
  DeliveryNoteHttpApi,
  MaterialApp,
  MaterialHttpApi,
  StockHttpApi,
  SupplierApp,
  SupplierHttpApi,
  WarehouseDeliveryNoteReconciler,
  WarehouseReportsHttpApi,
}
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
      calendar: CompanyCalendarApp.Store,
      customers: CustomerApp.Store,
      tasks: TaskApp.Store,
      manufacturings: ManufacturingApp.Store,
      orders: OrderApp.Store,
      suppliers: SupplierApp.Store,
      materials: MaterialApp.Store,
      deliveryNotes: DeliveryNoteApp.Store,
      deliveryNoteReconciler: WarehouseDeliveryNoteReconciler,
      security: ApiSecurity,
  ): HttpRoutes[IO] =
    val planningGateway = PlanningEntityGateway.fromStores(orders, employees, calendar)
    val apiEndpoints: List[ServerEndpoint[Any, IO]] =
      EmployeeHttpApi.routes(employees, security) ++
        CompanyCalendarHttpApi.routes(calendar, security) ++
        CustomerHttpApi.routes(customers, security) ++
        TaskHttpApi.routes(tasks, manufacturings, security) ++
        ManufacturingHttpApi.routes(manufacturings, tasks, security) ++
        OrderHttpApi.routes(orders, customers, tasks, employees, materials, security) ++
        SupplierHttpApi.routes(suppliers, materials, security) ++
        MaterialHttpApi.routes(materials, suppliers, security) ++
        StockHttpApi.routes(materials, suppliers, orders, security) ++
        WarehouseReportsHttpApi.routes(materials, suppliers, orders, security) ++
        DeliveryNoteHttpApi.routes(deliveryNotes, materials, suppliers, deliveryNoteReconciler, security) ++
        PlanningRoutes.serverEndpoints(planningBackend, planningGateway, manufacturings, tasks, security)

    val documentationEndpoints = SwaggerInterpreter().fromServerEndpoints[IO](apiEndpoints, "RST Manager API", "0.1.0")

    Http4sServerInterpreter[IO]().toRoutes(apiEndpoints ++ documentationEndpoints)
  end routes
end ApiServer
