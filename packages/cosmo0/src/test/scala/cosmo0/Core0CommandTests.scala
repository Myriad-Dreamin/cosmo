package cosmo0

class Core0CommandTests extends munit.FunSuite:
  private val pathFsPath = "packages/cosmoc/src/core0/path_fs.cos"
  private val commandPath = "packages/cosmoc/src/core0/command.cos"
  private val commandTestPath = "packages/cosmoc/src/core0/command_test.cos"
  private val linkCommandPath = "packages/cosmoc/src/link/command.cos"
  private val driverConfigPath = "packages/cosmoc/src/driver/config.cos"

  test("core0.command source API lowers through trusted externs, not descriptors"):
    val source = combineSources(
      List(pathFsPath, commandPath),
      """def core0_command_run_smoke(path: Path): Result[CommandResult, CommandError] = {
        |  val command = core0_command(path);
        |  command.arg("--version");
        |  command.run()
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("core0_command_run_smoke.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.command lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @command_run command_run(%command command: &Command) -> Result[CommandResult, CommandError] extern cosmo0.extern.v0 \"::cosmo0_runtime::command_run\""))
    assert(rendered.contains("call @command_run"))
    assert(!rendered.contains("descriptor Command"))
    assert(!rendered.contains("descriptor Process"))
    assert(!rendered.contains("descriptor Shell"))

    val compiled = CppBackend().emit(lowered.value.get.lir)
    assert(
      compiled.isSuccess,
      s"core0.command C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    assert(compiled.value.get.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::command_run")))
    assert(compiled.value.get.backendRequirements.contains(BackendRequirement.include("<cstdlib>")))
    assert(compiled.value.get.backendRequirements.contains(BackendRequirement.include("<string>")))
    assert(compiled.value.get.backendRequirements.contains(BackendRequirement.include("<vector>")))

  test("core0.command source tests and link component compile as later-stage source"):
    val source = combineSources(
      List(pathFsPath, commandPath, commandTestPath, driverConfigPath, linkCommandPath),
      """def core0_command_component_smoke(): Bool = {
        |  val config = driver_config_with_input("main.o");
        |  val command = driver_link_smoke_command(config);
        |  command_construction_smoke() and command_result_smoke() and command.is_some()
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("core0_command_component_smoke.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.command component lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

  test("core0.command capability is later-stage std metadata, not a descriptor family"):
    val profile = StageCapabilityProfile(
      "cosmo1.link",
      requiredPrimitiveDescriptors = Set.empty,
      requiredStdCapabilities = Set(StageCapabilityRegistry.Core0Command),
    )
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      stdCapabilities = StageCapabilityRegistry.defaultAvailability.stdCapabilities - StageCapabilityRegistry.Core0Command,
    )

    val diagnostics = StageCapabilityRegistry.validate(profile, availability)

    assert(diagnostics.exists(_.message.contains("requires std capability core0.command")))
    assert(StageCapabilityRegistry.knownStdCapabilities.contains(StageCapabilityRegistry.Core0Command))
    assert(StageCapabilityRegistry.defaultAvailability.stdCapabilities.contains(StageCapabilityRegistry.Core0Command))
    assert(StageCapabilityRegistry.laterStageStdCapabilities.contains(StageCapabilityRegistry.Core0Command))
    assert(!StageCapabilityRegistry.stage1Profile.requiredStdCapabilities.contains(StageCapabilityRegistry.Core0Command))
    assert(StandardGenericDescriptors.get("Command").isEmpty)
    assert(StandardGenericDescriptors.get("CommandResult").isEmpty)

  private def combineSources(paths: List[String], extra: String): String =
    (paths.map(readCosmoSource) :+ extra).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
