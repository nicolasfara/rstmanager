import org.scalajs.linker.interface.ModuleSplitStyle
import sbtassembly.AssemblyPlugin.autoImport.*
import sbtassembly.MergeStrategy

lazy val scala3Version = "3.3.7"
lazy val http4sVersion = "0.23.36"
lazy val slf4jVersion = "2.0.17"
lazy val logbackVersion = "1.5.38"
lazy val tapirVersion = "1.13.26"
lazy val projectScalacOptions = Seq(
  "-encoding",
  "utf-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:strictEquality",
  "-Werror",
  "-Wunused:all",
  "-Yexplicit-nulls",
  "-Ysafe-init"
//  "-explain"
)
lazy val sharedDependencies = Seq(
  "com.github.nscala-time" %% "nscala-time" % "3.0.0",
  "io.circe" %% "circe-core" % "0.14.16",
  "io.circe" %% "circe-generic" % "0.14.16",
  "io.github.iltotore" %% "iron" % "3.3.1",
  "io.github.iltotore" %% "iron-cats" % "3.3.2",
  "org.scalactic" %% "scalactic" % "3.2.20",
  "org.scalatest" %% "scalatest" % "3.2.20" % "test",
  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test",
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % "test",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "cats-kernel" % "2.13.0",
  "org.typelevel" %% "kittens" % "3.5.0",
  "dev.hnaderi" %% "edomata-core" % "0.13.0",
  "org.scala-graph" %% "graph-core" % "2.0.3",
  "dev.optics" %% "monocle-core" % "3.3.0",
  "dev.optics" %% "monocle-macro" % "3.3.0"
)

// ThisBuild / wartremoverErrors ++= Warts.unsafe
ThisBuild / autoAPIMappings := true

inThisBuild(
  List(
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)



// ThisBuild / Compile / doc / scalacOptions ++= Seq(
//   "-project-version",
//   version.value,
//   "-siteroot",
//   (baseDirectory.value / "docs").getAbsolutePath,
// )

lazy val domain = project
  .in(file("domain"))
  .settings(
    name := "rstmanager-domain",
    scalaVersion := scala3Version,
    scalacOptions ++= projectScalacOptions,
    libraryDependencies ++= Seq() ++ sharedDependencies
  )

lazy val service = project
  .in(file("service"))
  .dependsOn(domain)
  .settings(
    name := "rstmanager-service",
    scalaVersion := scala3Version,
    scalacOptions ++= projectScalacOptions,
    Compile / mainClass := Some("io.github.nicolasfara.rstmanager.planning.service.Main"),
    assembly / mainClass := Some("io.github.nicolasfara.rstmanager.planning.service.Main"),
    assembly / assemblyJarName := "rstmanager-service.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "maven", "org.webjars", "swagger-ui", "pom.properties") => MergeStrategy.singleOrError
      case PathList("META-INF", "resources", "webjars", "swagger-ui", _*) => MergeStrategy.singleOrError
      case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
      case PathList("META-INF", "INDEX.LIST") => MergeStrategy.discard
      case PathList("META-INF", "DEPENDENCIES") => MergeStrategy.discard
      case PathList("META-INF", "LICENSE" | "LICENSE.txt" | "NOTICE" | "NOTICE.txt") => MergeStrategy.discard
      case PathList("META-INF", "versions", _, "module-info.class") => MergeStrategy.discard
      case PathList("META-INF", file) if file.endsWith(".SF") || file.endsWith(".DSA") || file.endsWith(".RSA") => MergeStrategy.discard
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case "module-info.class" => MergeStrategy.discard
      case other => (assembly / assemblyMergeStrategy).value(other)
    },
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron-circe" % "3.3.2",
      "dev.hnaderi" %% "edomata-backend" % "0.13.0",
      "dev.hnaderi" %% "edomata-skunk-circe" % "0.13.0",
      "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion % Runtime
    ) ++ sharedDependencies
  )

lazy val circeVersion = "0.14.15"

lazy val frontend = project
  .in(file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "rstmanager",
    scalaVersion := scala3Version,
    scalacOptions ++= projectScalacOptions,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    // The frontend is a Scala.js module: it needs `%%%` (Scala.js) artifacts and cannot depend on the
    // JVM `domain`/`service` code, so the API DTOs are mirrored locally with circe codecs.
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "17.2.0",
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion
    )
  )

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(domain, service, frontend)
  .settings(
    name := "rstmanager",
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(frontend),
    ScalaUnidoc / unidoc / scalacOptions ++= Seq(
      "-project-version",
      version.value,
      // "-siteroot",
      // (baseDirectory.value / "docs").getAbsolutePath,
    ),
    publish / skip := true
  )
