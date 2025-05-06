import org.typelevel.sbt.gha.JobEnvironment
import org.typelevel.sbt.gha.PermissionValue
import org.typelevel.sbt.gha.Permissions
ThisBuild / scalaVersion := "3.6.3"
ThisBuild / scalacOptions ++= Seq(
  "-no-indent",
  "-deprecation",
  "-Wunused:all",
  "-Xkind-projector",
  "-Wvalue-discard",
)

ThisBuild / githubWorkflowPermissions := Some {
  // https://github.com/typelevel/sbt-typelevel/pull/794
  Permissions.Specify(
    pages = PermissionValue.Write,
    idToken = PermissionValue.Write,
    actions = PermissionValue.None,
    checks = PermissionValue.None,
    contents = PermissionValue.Read,
    deployments = PermissionValue.None,
    issues = PermissionValue.None,
    packages = PermissionValue.None,
    pullRequests = PermissionValue.None,
    repositoryProjects = PermissionValue.None,
    securityEvents = PermissionValue.None,
    statuses = PermissionValue.None,
  )
}

ThisBuild / githubWorkflowPublish := Seq(
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
  WorkflowStep.Use(
    UseRef.Public("actions", "upload-pages-artifact", "v3"),
    params = Map("path" -> "web/dist"),
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "deploy-pages", "v4")
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

val smithyDump = project
  .settings(
    libraryDependencies ++= Seq(
      "software.amazon.smithy" % "smithy-model" % "1.54.0",
      "software.amazon.smithy" % "smithy-syntax" % "1.54.0",
      "com.disneystreaming.alloy" % "alloy-core" % "0.3.14",
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
    )
  )
  .dependsOn(smithyDump)

val web = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
    },
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "calico" % "0.2.3",
      "org.typelevel" %%% "kittens" % "3.4.0",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "com.disneystreaming.smithy4s" %%% "smithy4s-xml" % "0.18.35",
      "com.disneystreaming.smithy4s" %%% "smithy4s-protobuf" % "0.18.35",
      "com.disneystreaming.smithy4s" %%% "smithy4s-http4s" % "0.18.35",
      "com.disneystreaming.smithy4s" %%% "smithy4s-dynamic" % "0.18.35",
      "org.http4s" %%% "http4s-ember-core" % "0.23.30",
    ),
  )

val root = project
  .in(file("."))
  .aggregate(smithyDump, smithyDumpApi, web)
