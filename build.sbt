import org.scalajs.linker.interface.ModuleSplitStyle
import org.scalajs.jsenv.nodejs.NodeJSEnv

lazy val cosmo = project
  .in(file("packages/cosmo"))
  .enablePlugins(ScalaJSPlugin) // Enable the Scala.js plugin in this project
  .dependsOn(cosmo0)
  .settings(
    name := "cosmo",
    scalaVersion := "3.3.3",

    // Tell Scala.js that this is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("cosmo")))
    },
    jsEnv := new NodeJSEnv(
      NodeJSEnv.Config().withArgs(List("--enable-source-maps")),
    ),

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.1.1",
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.4.2",
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test,
    libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.9.0",
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
