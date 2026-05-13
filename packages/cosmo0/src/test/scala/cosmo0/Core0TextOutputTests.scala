package cosmo0

class Core0TextOutputTests extends munit.FunSuite:
  private val core0TextPath = "packages/cosmoc/src/core0/text.cos"
  private val core0TextOutputPath = "packages/cosmoc/src/core0/text_output.cos"
  private val core0PathFsPath = "packages/cosmoc/src/core0/path_fs.cos"
  private val sourceTextPath = "packages/cosmoc/src/source/source.cos"
  private val sourceMapPath = "packages/cosmoc/src/source/source_map.cos"
  private val diagnosticPath = "packages/cosmoc/src/driver/diagnostic.cos"
  private val diagnosticTestPath = "packages/cosmoc/src/driver/diagnostic_test.cos"

  test("core0.text-output source API lowers through trusted externs, not descriptors"):
    val source = combineSources(
      List(core0TextOutputPath),
      """def core0_text_output_smoke(): Unit = {
        |  core0_stdout_write("diagnostic");
        |  core0_stdout_write_line(" rendered")
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("core0_text_output_smoke.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.text-output lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @print print(%value value: String) -> Unit extern cosmo0.extern.v0 \"::cosmo0_runtime::print\""))
    assert(rendered.contains("fn @println println(%value value: String) -> Unit extern cosmo0.extern.v0 \"::cosmo0_runtime::println\""))
    assert(rendered.contains("fn @TextWriter.write write"))
    assert(!rendered.contains("descriptor TextWriter"))
    assert(!rendered.contains("descriptor TextOutput"))

    val compiled = CppBackend().emit(lowered.value.get.lir)

    assert(
      compiled.isSuccess,
      s"core0.text-output C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get
    assert(output.source.contains("struct TextWriter"))
    assert(output.source.contains("::cosmo0_runtime::print("))
    assert(output.source.contains("::cosmo0_runtime::println("))
    assert(!output.source.contains("TextWriter_descriptor"))
    assert(output.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::print")))
    assert(output.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::println")))

  test("diagnostic rendering is deterministic and writes through a text sink"):
    val source = combineSources(
      List(
        core0TextPath,
        core0TextOutputPath,
        core0PathFsPath,
        sourceTextPath,
        sourceMapPath,
        diagnosticPath,
        diagnosticTestPath,
      ),
      "",
    )

    val lowered = Cosmo0().lower(SourceFile(diagnosticTestPath, source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"diagnostic output lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @render_diagnostic render_diagnostic"))
    assert(rendered.contains("fn @write_diagnostic write_diagnostic"))
    assert(rendered.contains("fn @diagnostic_render_smoke diagnostic_render_smoke"))
    assert(rendered.contains("method_call %writer.write("))
    assert(!rendered.contains("descriptor TextWriter"))

    val compiled = CppBackend().emit(lowered.value.get.lir)

    assert(
      compiled.isSuccess,
      s"diagnostic output C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get
    assert(output.source.contains("std::string(\"error\")"))
    assert(output.source.contains("std::string(\": \")"))
    assert(output.source.contains("std::string(\"  label: \")"))
    assert(output.source.contains("inline void write_diagnostic(TextWriter writer, Diagnostic diagnostic)"))
    assert(output.source.contains("::cosmo0_runtime::print("))

  test("Stage 1 profile diagnoses missing core0.text-output"):
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      stdCapabilities =
        StageCapabilityRegistry.defaultAvailability.stdCapabilities - StageCapabilityRegistry.Core0TextOutput,
    )

    val diagnostics =
      StageCapabilityRegistry.validate(StageCapabilityRegistry.Cosmo1Stage1, availability)

    assert(
      diagnostics.exists(diagnostic =>
        diagnostic.code == "cosmo0.stage.missing-capability" &&
          diagnostic.message.contains(StageCapabilityRegistry.Core0TextOutput),
      ),
      s"missing capability diagnostic for core0.text-output in ${diagnostics.map(d => d.code -> d.message)}",
    )

  private def combineSources(paths: List[String], extra: String): String =
    (paths.map(ParserFixtureManifest.readFile) :+ extra).mkString("\n")
