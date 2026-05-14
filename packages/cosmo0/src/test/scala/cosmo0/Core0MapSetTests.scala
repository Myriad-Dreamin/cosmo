package cosmo0

class Core0MapSetTests extends munit.FunSuite:
  private val core0MapSetPath = "packages/cosmoc/src/core0/map_set.cos"
  private val core0MapSetTestPath = "packages/cosmoc/src/core0/map_set_test.cos"
  private val core0JsonPath = "packages/cosmoc/src/core0/json.cos"
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val symbolPath = "packages/cosmoc/src/names/symbol.cos"
  private val scopePath = "packages/cosmoc/src/names/scope.cos"
  private val moduleGraphPath = "packages/cosmoc/src/package/module_graph.cos"
  private val moduleGraphTestPath = "packages/cosmoc/src/package/module_graph_test.cos"

  test("core0.map-set source API compiles through deterministic ordered backend containers"):
    val source = combineSources(List(core0MapSetPath, core0MapSetTestPath), "")

    val compiled = Cosmo0().compile(SourceFile(core0MapSetTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"core0.map-set source test compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.contains("std::map<std::string, std::string>"))
    assert(output.contains("std::set<std::string>"))
    assert(output.contains("cosmo0_runtime::map_insert"))
    assert(output.contains("cosmo0_runtime::iter_next"))
    assert(output.contains("inline bool map_set_test_iteration_is_key_sorted()"))
    assert(output.contains("inline bool map_set_test_set_iteration_is_sorted()"))

  test("Map and Set iteration lower to deterministic descriptor iteration"):
    val lowered = Cosmo0().lower(
      """def walk(values: Map[String, i32], seen: Set[String]): Unit = {
        |  for (key in values) {
        |    key;
        |  }
        |  for (item in seen) {
        |    item;
        |  }
        |}
        |""".stripMargin,
    )

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.map-set lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )
    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("descriptor Map[String, i32]::iter_has_next(%values) -> Bool"))
    assert(rendered.contains("descriptor Map[String, i32]::iter_next(%values) -> String"))
    assert(rendered.contains("descriptor Set[String]::iter_has_next(%seen) -> Bool"))
    assert(rendered.contains("descriptor Set[String]::iter_next(%seen) -> String"))

  test("Map and Set reject unsupported key types"):
    val result = Cosmo0().check(
      """class Token {}
        |
        |class Bad {
        |  val by_token: Map[Token, i32]
        |  val seen_float: Set[f64]
        |  val seen_bool: Set[Bool]
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Failed)
    val keyDiagnostics = result.diagnostics.filter(_.code == "cosmo0.type.unsupported-map-key")
    assertEquals(keyDiagnostics.length, 3)
    assert(keyDiagnostics.exists(_.message.contains("Map key type Token")))
    assert(keyDiagnostics.exists(_.message.contains("Set key type f64")))
    assert(keyDiagnostics.exists(_.message.contains("Set key type Bool")))

  test("Map and Set accept supported key types including aliases and typed IDs"):
    val result = Cosmo0().check(
      """class Definition {}
        |type Symbol = String
        |type DefId = Id[Definition]
        |
        |class SupportedKeys {
        |  val by_symbol: Map[Symbol, DefId]
        |  val by_id: Map[DefId, String]
        |  val seen_id: Set[DefId]
        |  val seen_i32: Set[i32]
        |}
        |""".stripMargin,
    )

    assertEquals(result.phase, Phase.Check)
    assert(
      result.isSuccess,
      s"supported map/set key types should check: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

  test("cosmo1 module graph and name scope compile with deterministic maps and sets"):
    val source = combineSources(
      List(core0JsonPath, spanPath, astPath, symbolPath, scopePath, moduleGraphPath, moduleGraphTestPath),
      "",
    )

    val compiled = Cosmo0().compile(SourceFile(moduleGraphTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 package/name map-set compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.contains("std::map<std::string, std::size_t>"))
    assert(output.contains("std::set<std::string>"))
    assert(output.contains("inline bool module_graph_test_uses_deterministic_maps()"))
    assert(output.contains("inline bool module_graph_test_loads_explicit_sources()"))
    assert(output.contains("inline bool module_graph_test_orders_dependencies_first()"))

  test("core0.map-set capability is later-stage std metadata, not a Stage 1 requirement"):
    val profile = StageCapabilityProfile(
      "cosmo1.names",
      requiredPrimitiveDescriptors = Set.empty,
      requiredStdCapabilities = Set(StageCapabilityRegistry.Core0MapSet),
    )
    val availability = StageCapabilityRegistry.defaultAvailability.copy(
      stdCapabilities = StageCapabilityRegistry.defaultAvailability.stdCapabilities - StageCapabilityRegistry.Core0MapSet,
    )

    val diagnostics = StageCapabilityRegistry.validate(profile, availability)

    assert(diagnostics.exists(_.message.contains("requires std capability core0.map-set")))
    assert(StageCapabilityRegistry.knownStdCapabilities.contains(StageCapabilityRegistry.Core0MapSet))
    assert(StageCapabilityRegistry.defaultAvailability.stdCapabilities.contains(StageCapabilityRegistry.Core0MapSet))
    assert(StageCapabilityRegistry.laterStageStdCapabilities.contains(StageCapabilityRegistry.Core0MapSet))
    assert(!StageCapabilityRegistry.stage1Profile.requiredStdCapabilities.contains(StageCapabilityRegistry.Core0MapSet))

  private def combineSources(paths: List[String], extra: String): String =
    (paths.map(readCosmoSource) :+ extra).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
