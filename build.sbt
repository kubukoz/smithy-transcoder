ThisBuild / scalaVersion := "3.6.3"
ThisBuild / scalacOptions ++= Seq(
  "-no-indent",
  "-deprecation",
  "-Wunused:all",
  "-Xkind-projector",
  "-Wvalue-discard",
)

val smithyDump = project
  .settings(
    libraryDependencies ++= Seq(
      "software.amazon.smithy" % "smithy-model" % "1.54.0",
      "com.disneystreaming.alloy" % "alloy-core" % "0.3.14",
    )
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
      "com.disneystreaming.smithy4s" %%% "smithy4s-xml" % "0.18.29",
      "com.disneystreaming.smithy4s" %%% "smithy4s-protobuf" % "0.18.29",
      "com.disneystreaming.smithy4s" %%% "smithy4s-http4s" % "0.18.29",
      "com.disneystreaming.smithy4s" %%% "smithy4s-dynamic" % "0.18.29",
      "org.http4s" %%% "http4s-ember-core" % "0.23.30",
      "com.thesamet.scalapb" %%% "protobuf-runtime-scala" % "0.8.14",
    ),
  )

val root = project
  .in(file("."))
  .aggregate(smithyDump, smithyDumpApi, web)
