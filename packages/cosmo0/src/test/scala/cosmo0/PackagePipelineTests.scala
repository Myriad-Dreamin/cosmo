package cosmo0

class PackagePipelineTests extends munit.FunSuite:
  test("package loader reads metadata and discovers cosmo0 sources deterministically"):
    val result = Cosmo0().loadPackage("fixtures/cosmo0/package/single")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)

    val pkg = result.value.get
    assertEquals(pkg.metadata.name, "@cosmo0/package-single")
    assertEquals(pkg.metadata.version, "0.0.0")
    assertEquals(pkg.metadata.sourceRoot, "src")
    assertEquals(pkg.metadata.target, Some("cosmo0"))
    assertEquals(pkg.metadata.stageProfile, None)
    assertEquals(pkg.metadata.sourceFiles, None)
    assertEquals(pkg.modules.map(_.modulePath), List(List("main")))

  test("package loader reports metadata, source, and unsupported target diagnostics"):
    val cases = List(
      "fixtures/cosmo0/package/no-metadata" ->
        (PhaseStatus.Failed, "cosmo0.package.missing-metadata"),
      "fixtures/cosmo0/package/no-sources" ->
        (PhaseStatus.Failed, "cosmo0.package.no-sources"),
      "fixtures/cosmo0/package/unsupported-target" ->
        (PhaseStatus.Unsupported, "cosmo0.package.unsupported-target"),
    )

    cases.foreach { case (path, (status, code)) =>
      val result = Cosmo0().loadPackage(path)

      assertEquals(result.phase, Phase.Check)
      assertEquals(result.status, status)
      assert(
        result.diagnostics.exists(_.code == code),
        s"missing diagnostic $code in ${result.diagnostics.map(_.code)}",
      )
    }

  test("package check accepts single-module packages"):
    val result = Cosmo0().checkPackage("fixtures/cosmo0/package/single")

    assertEquals(result.phase, Phase.Check)
    assert(
      result.isSuccess,
      s"package check failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(result.value.get.moduleOrder, List("main"))
    assert(result.value.get.lowered.lir.declarations.exists(_.name == "answer"))

  test("package check orders acyclic imports before importing modules"):
    val result = Cosmo0().checkPackage("fixtures/cosmo0/package/multi")

    assertEquals(result.phase, Phase.Check)
    assert(
      result.isSuccess,
      s"package check failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(result.value.get.moduleOrder, List("util", "main"))
    assert(result.value.get.checked.typed.declarations.exists(_.name == "collect"))
    assert(result.value.get.checked.typed.declarations.exists(_.name == "entry"))

  test("package check diagnoses missing imports and dependency cycles"):
    val missingImport = Cosmo0().checkPackage("fixtures/cosmo0/package/missing-import")

    assertEquals(missingImport.phase, Phase.Check)
    assertEquals(missingImport.status, PhaseStatus.Failed)
    assert(
      missingImport.diagnostics.exists(_.code == "cosmo0.package.missing-import"),
      s"missing import diagnostic in ${missingImport.diagnostics.map(_.code)}",
    )
    assert(missingImport.diagnostics.exists(_.span.nonEmpty))

    val cycle = Cosmo0().checkPackage("fixtures/cosmo0/package/import-cycle")

    assertEquals(cycle.phase, Phase.Check)
    assertEquals(cycle.status, PhaseStatus.Failed)
    assert(
      cycle.diagnostics.exists(_.code == "cosmo0.package.import-cycle"),
      s"missing cycle diagnostic in ${cycle.diagnostics.map(_.code)}",
    )
    assert(cycle.diagnostics.exists(_.message.contains("a -> b -> a")))

  test("package compile emits deterministic output and unique runtime requirements"):
    val first = Cosmo0().compilePackage("fixtures/cosmo0/package/multi")
    val second = Cosmo0().compilePackage("fixtures/cosmo0/package/multi")

    assertEquals(first.phase, Phase.Compile)
    assert(
      first.isSuccess,
      s"package compile failed with diagnostics: ${first.diagnostics.map(d => d.code -> d.message)}",
    )
    assert(
      second.isSuccess,
      s"package compile failed with diagnostics: ${second.diagnostics.map(d => d.code -> d.message)}",
    )

    val firstOutput = first.value.get.output
    val secondOutput = second.value.get.output
    assertEquals(firstOutput.source, secondOutput.source)
    assertEquals(firstOutput.moduleName, "cosmo0_package_multi")
    assert(firstOutput.source.contains("namespace cosmo0_package_multi {"))
    assert(firstOutput.source.contains("inline std::vector<std::string> collect()"))
    assert(firstOutput.source.contains("inline std::size_t entry()"))
    assertEquals(firstOutput.runtimeRequirements, firstOutput.runtimeRequirements.distinct.sorted)
    assert(firstOutput.runtimeRequirements.contains("vec"))
    assert(!firstOutput.source.contains("CodeGen"))

  test("cosmo1 primitive span/token smoke package checks and compiles"):
    val path = "fixtures/cosmo0/cosmo1/primitive-descriptor-smoke"
    val checked = Cosmo0().checkPackage(path)

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"cosmo1 span/token package check failed with diagnostics: ${checked.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(checked.value.get.moduleOrder, List("source/span", "lex/token", "main"))

    val compiled = Cosmo0().compilePackage(path)

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 span/token package compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.source.contains("struct Span"))
    assert(output.source.contains("struct TokenKind"))
    assert(output.source.contains("struct Token"))
    assert(output.source.contains("inline bool span_token_smoke()"))

  test("cosmo1 extern std smoke package checks and records runtime requirements"):
    val path = "fixtures/cosmo0/cosmo1/extern-std-smoke"
    val checked = Cosmo0().checkPackage(path)

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"cosmo1 extern std package check failed with diagnostics: ${checked.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(checked.value.get.moduleOrder, List("std/io", "main"))

    val compiled = Cosmo0().compilePackage(path)

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 extern std package compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.source.contains("inline void extern_std_smoke()"))
    assert(output.source.contains("::cosmo0_runtime::println(std::string(\"cosmo1 extern smoke\"));"))
    assert(output.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::println")))
    assert(output.backendRequirements.contains(BackendRequirement.include("<cstdio>")))

  test("cosmoc Stage 1 package selects the cosmo1 Stage 1 capability profile"):
    val path = "packages/cosmoc"
    val loaded = Cosmo0().loadPackage(path)

    assertEquals(loaded.phase, Phase.Check)
    assert(
      loaded.isSuccess,
      s"cosmoc Stage 1 package load failed with diagnostics: ${loaded.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(loaded.value.get.metadata.stageProfile, Some(StageCapabilityRegistry.Cosmo1Stage1))
    assertEquals(
      loaded.value.get.metadata.sourceFiles,
      Some(
        List(
          "core0/text.cos",
          "driver/diagnostic.cos",
          "lex/lexer.cos",
          "source/source.cos",
          "source/source_map.cos",
          "source/source_test.cos",
          "source/source_map_test.cos",
          "parser.cos",
          "parser_test.cos",
        ),
      ),
    )
    assertEquals(
      loaded.value.get.modules.map(_.modulePath),
      List(
        List("core0", "text"),
        List("driver", "diagnostic"),
        List("lex", "lexer"),
        List("parser"),
        List("parser_test"),
        List("source", "source"),
        List("source", "source_map"),
        List("source", "source_map_test"),
        List("source", "source_test"),
      ),
    )

    val checked = Cosmo0().checkPackage(path)

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"cosmoc Stage 1 package check failed with diagnostics: ${checked.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(
      checked.value.get.moduleOrder,
      List(
        "core0/text",
        "source/source",
        "source/source_map",
        "driver/diagnostic",
        "lex/lexer",
        "parser",
        "parser_test",
        "source/source_map_test",
        "source/source_test",
      ),
    )

    val compiled = Cosmo0().compilePackage(path)

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmoc Stage 1 package compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )

    val output = compiled.value.get.output
    assert(output.source.contains("struct TextBuilder"))
    assert(output.source.contains("struct SourceText"))
    assert(output.source.contains("struct SourceMap"))
    assert(output.source.contains("inline std::string core0_text_slice("))
    assert(output.source.contains("inline bool parse_source("))
    assert(output.source.contains("inline int32_t main()"))
    assert(output.source.contains("::cosmo0_runtime::read_file("))
    assert(output.source.contains("::cosmo0_runtime::println("))
    assert(!output.source.contains("StringBuilder"))
    assert(output.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::read_file")))
    assert(output.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::println")))
    assert(output.backendRequirements.contains(BackendRequirement.include("<fstream>")))
    assert(output.backendRequirements.contains(BackendRequirement.include("<cstdio>")))
