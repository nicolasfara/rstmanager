import org.scalajs.linker.interface.ModuleSplitStyle

lazy val scala3Version = "3.3.7"
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
  "-Ysafe-init",
  "-explain"
)
lazy val sharedDependencies = Seq(
  "com.github.nscala-time" %% "nscala-time" % "3.0.0",
  "io.circe" %% "circe-core" % "0.14.15",
  "io.circe" %% "circe-generic" % "0.14.15",
  "io.github.iltotore" %% "iron" % "3.2.2",
  "io.github.iltotore" %% "iron-cats" % "3.2.2",
  "org.scalactic" %% "scalactic" % "3.2.19",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "cats-kernel" % "2.13.0",
  "org.typelevel" %% "kittens" % "3.5.0",
  "dev.hnaderi" %% "edomata-core" % "0.12.8",
  "org.scala-graph" %% "graph-core" % "2.0.3"
)

ThisBuild / wartremoverErrors ++= Warts.unsafe
ThisBuild / autoAPIMappings := true

inThisBuild(
  List(
    scalaVersion := scala3Version,
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

lazy val domain = project
  .in(file("domain"))
  .settings(
    name := "rstmanager-domain",
    scalaVersion := scala3Version,
    scalacOptions ++= projectScalacOptions,
    libraryDependencies ++= Seq() ++ sharedDependencies
  )

lazy val frontend = project
  .in(file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(domain)
  .settings(
    name := "rstmanager",
    scalaVersion := scala3Version,
    scalacOptions ++= projectScalacOptions,
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "17.2.0"
    ) ++ sharedDependencies
  )

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .aggregate(domain, frontend)
  .settings(
    name := "rstmanager",
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(frontend),
    publish / skip := true
  )