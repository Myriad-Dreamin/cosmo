import org.scalajs.linker.interface.ModuleSplitStyle
import org.scalajs.jsenv.nodejs.NodeJSEnv

lazy val root = project
  .in(file("."))
  .aggregate(cosmo0)
  .settings(
    name := "cosmo-root",
    publish / skip := true,
  )

lazy val cosmo0 = project
  .in(file("packages/cosmo0"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "cosmo0",
    scalaVersion := "3.3.3",

    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("cosmo0")))
    },
    jsEnv := new NodeJSEnv(
      NodeJSEnv.Config().withArgs(List("--enable-source-maps")),
    ),

    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.1.1",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test,
  )
