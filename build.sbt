import org.scalajs.linker.interface.ModuleSplitStyle
import org.typelevel.sbt.gha.JobEnvironment
import org.typelevel.sbt.gha.PermissionValue
import org.typelevel.sbt.gha.Permissions

ThisBuild / scalaVersion := "3.7.1"
ThisBuild / scalacOptions ++= Seq(
  "-no-indent",
  "-deprecation",
  "-Wunused:all",
  "-Xkind-projector",
  "-Wvalue-discard",
)

ThisBuild / githubWorkflowPermissions := Some {
  Permissions
    .Specify
    .defaultRestrictive
    .withPages(PermissionValue.Write)
    .withIdToken(PermissionValue.Write)
}

val yarnBuildSteps = Seq(
  WorkflowStep.Sbt(List("smithyDump/assembly")),
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v4"),
    params = Map(
      "node-version" -> "20",
      "cache" -> "yarn",
      "cache-dependency-path" -> "web/yarn.lock",
    ),
  ),
  WorkflowStep.Run(List("yarn"), workingDirectory = Some("web")),
  WorkflowStep.Run(
    List("yarn build"),
    workingDirectory = Some("web"),
  ),
)

ThisBuild / githubWorkflowBuild ++= yarnBuildSteps
ThisBuild / githubWorkflowPublish := List.concat(
  yarnBuildSteps,
  List(
    WorkflowStep.Use(
      UseRef.Public("actions", "upload-pages-artifact", "v3"),
      params = Map("path" -> "web/dist"),
    ),
    WorkflowStep.Use(
      UseRef.Public("actions", "deploy-pages", "v4")
    ),
  ),
)

ThisBuild / githubWorkflowGeneratedCI ~= {
  _.map {
    case job if job.id == "publish" =>
      job.withEnvironment(
        Some(
          JobEnvironment(
            "github-pages",
            // https://github.com/typelevel/sbt-typelevel/issues/802
            Some(new URL("https://kubukoz.github.io/smithy-transcoder")),
          )
        )
      )
    case job => job
  }
}

ThisBuild / mergifyStewardConfig ~= (_.map(_.withMergeMinors(true)))

val smithyDump = project
  .settings(
    libraryDependencies ++= Seq(
      "software.amazon.smithy" % "smithy-model" % "1.61.0",
      "software.amazon.smithy" % "smithy-syntax" % "1.61.0",
      "com.disneystreaming.alloy" % "alloy-core" % "0.3.31",
    ),
    autoScalaLibrary := false,
    javacOptions ++= Seq(
      "--release",
      "8",
    ),
  )
  .enablePlugins(AssemblyPlugin)

val smithyDumpApi = project
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % "0.23.30",
      "org.http4s" %% "http4s-ember-server" % "0.23.30",
      "org.http4s" %% "http4s-circe" % "0.23.30",
    ),
    fork := false,
  )
  .dependsOn(smithyDump)

val web = project
  .enablePlugins(ScalaJSPlugin, Smithy4sCodegenPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "calico" % "0.2.3",
      "org.typelevel" %%% "kittens" % "3.5.0",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "com.disneystreaming.smithy4s" %%% "smithy4s-cats" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %%% "smithy4s-xml" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %%% "smithy4s-protobuf" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %%% "smithy4s-http4s" % smithy4sVersion.value,
      "com.disneystreaming.smithy4s" %%% "smithy4s-dynamic" % smithy4sVersion.value,
      "org.http4s" %%% "http4s-ember-core" % "0.23.30",
    ),
  )

val root = project
  .in(file("."))
  .aggregate(smithyDump, smithyDumpApi, web)
