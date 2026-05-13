package cosmo0

class Core0JsonBridgeTests extends munit.FunSuite:
  private val core0JsonPath = "packages/cosmoc/src/core0/json.cos"
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val loaderPath = "packages/cosmoc/src/syntax/json_loader.cos"

  test("core0.json source accessors cover objects, arrays, strings, booleans, null, and numbers"):
    val source = combineSources(
      List(core0JsonPath),
      """def core0_json_accessor_smoke(value: JsonValue): Bool = {
        |  val field = value.field("kind");
        |  val item = value.array_get(0);
        |  value.is_null() or
        |    value.as_bool().is_some() or
        |    value.as_number_text().is_some() or
        |    value.as_string().is_some() or
        |    value.array_len().is_some() or
        |    field.is_some() or
        |    item.is_some()
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("core0_json_accessor_smoke.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.json accessor lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @JsonValue.field field"))
    assert(rendered.contains("fn @JsonValue.array_get array_get"))
    assert(rendered.contains("fn @json_field json_field"))
    assert(rendered.contains("extern cosmo0.extern.v0 \"::cosmo0_runtime::json_field\""))
    assert(rendered.contains("fn @json_array_get json_array_get"))
    assert(rendered.contains("extern cosmo0.extern.v0 \"::cosmo0_runtime::json_array_get\""))
    assert(!rendered.contains("descriptor Json"))
    assert(!rendered.contains("descriptor JsonValue"))

    val compiled = CppBackend().emit(lowered.value.get.lir)
    assert(
      compiled.isSuccess,
      s"core0.json accessor C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.source
    assert(output.contains("struct JsonValue"))
    assert(!output.contains("JsonValue_descriptor"))

  test("Json.parse lowers through a trusted extern-backed std binding"):
    val source = combineSources(
      List(core0JsonPath),
      """def core0_json_parse_smoke(text: String): Result[JsonValue, JsonParseError] = {
        |  core0_json().parse(text)
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("core0_json_parse_smoke.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.json parse lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @json_parse json_parse(%text text: String) -> Result[JsonValue, JsonParseError] extern cosmo0.extern.v0 \"::cosmo0_runtime::json_parse\""))
    assert(rendered.contains("call @json_parse(%text) -> Result[JsonValue, JsonParseError]"))
    assert(!rendered.contains("descriptor Json"))
    assert(!rendered.contains("descriptor JsonValue"))

    val compiled = CppBackend().emit(lowered.value.get.lir)
    assert(
      compiled.isSuccess,
      s"core0.json parse C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    assert(compiled.value.get.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::json_parse")))

  test("cosmo1 syntax JSON loader compiles selected parser JSON mapping into syntax arenas"):
    val source = combineSources(
      List(core0JsonPath, spanPath, astPath, loaderPath),
      """def syntax_json_loader_smoke(): Bool = {
        |  val loaded = syntax_json_load("{}");
        |  loaded.is_ok() or loaded.is_err()
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("syntax_json_loader_smoke.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"syntax JSON loader lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @syntax_json_load_expr syntax_json_load_expr"))
    assert(rendered.contains("descriptor Arena[SyntaxExpr]::alloc"))
    assert(!rendered.contains("descriptor Json"))
    assert(!rendered.contains("descriptor JsonValue"))

  test("core0.json capability is later-stage std metadata, not a descriptor family"):
    val profile = StageCapabilityProfile(
      "cosmo1.json-loader",
      requiredPrimitiveDescriptors = Set.empty,
      requiredStdCapabilities = Set(StageCapabilityRegistry.Core0Json),
    )
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      stdCapabilities = StageCapabilityRegistry.defaultAvailability.stdCapabilities - StageCapabilityRegistry.Core0Json,
    )

    val diagnostics = StageCapabilityRegistry.validate(profile, availability)

    assert(diagnostics.exists(_.message.contains("requires std capability core0.json")))
    assert(StageCapabilityRegistry.laterStageStdCapabilities.contains(StageCapabilityRegistry.Core0Json))
    assert(!StageCapabilityRegistry.stage1Profile.requiredStdCapabilities.contains(StageCapabilityRegistry.Core0Json))
    assert(StandardGenericDescriptors.get("Json").isEmpty)
    assert(StandardGenericDescriptors.get("JsonValue").isEmpty)
    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("Json"))
    assert(StandardGenericDescriptors.Boundary.rejectedRuntimeDescriptorFamilies.contains("JsonValue"))

  private def combineSources(paths: List[String], extra: String): String =
    (paths.map(readCosmoSource) :+ extra).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
