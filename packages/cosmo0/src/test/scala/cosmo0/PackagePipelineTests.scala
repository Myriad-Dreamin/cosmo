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
