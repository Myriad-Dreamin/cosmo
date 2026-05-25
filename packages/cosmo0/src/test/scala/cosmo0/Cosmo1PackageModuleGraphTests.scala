package cosmo0

class Cosmo1PackageModuleGraphTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"

  test(
    "cosmo1 package graph fixtures load explicit sources and order dependencies",
  ):
    val path = "fixtures/cosmo0/cosmo1/package-graph/valid"
    val loaded = Cosmo0().loadPackage(path)

    assertEquals(loaded.phase, Phase.Check)
    assert(
      loaded.isSuccess,
      s"cosmo1 package graph fixture load failed: ${loaded.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assertEquals(
      loaded.value.get.metadata.stageProfile,
      Some(StageCapabilityRegistry.Cosmo1Stage1),
    )
    assertEquals(
      loaded.value.get.metadata.sourceFiles,
      Some(List("main.cos", "util.cos")),
    )
    assertEquals(
      loaded.value.get.modules.map(_.modulePath),
      List(List("main"), List("util")),
    )
    assert(!loaded.value.get.modules.exists(_.modulePath == List("unused")))

    val checked = Cosmo0().checkPackage(path)
    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"cosmo1 package graph fixture check failed: ${checked.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assertEquals(checked.value.get.moduleOrder, List("util", "main"))

  test(
    "cosmo1 package graph fixtures diagnose missing imports and cycles before checking",
  ):
    val missingImport = Cosmo0().checkPackage(
      "fixtures/cosmo0/cosmo1/package-graph/missing-import",
    )

    assertEquals(missingImport.phase, Phase.Check)
    assertEquals(missingImport.status, PhaseStatus.Failed)
    assert(
      missingImport.diagnostics.exists(
        _.code == "cosmo0.package.missing-import",
      ),
      s"missing import diagnostic not found: ${missingImport.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val cycle =
      Cosmo0().checkPackage("fixtures/cosmo0/cosmo1/package-graph/cycle")

    assertEquals(cycle.phase, Phase.Check)
    assertEquals(cycle.status, PhaseStatus.Failed)
    assert(
      cycle.diagnostics.exists(_.code == "cosmo0.package.import-cycle"),
      s"cycle diagnostic not found: ${cycle.diagnostics.map(d => d.code -> d.message)}",
    )

  test("single-source parser validation can bypass package metadata"):
    val source = List(
      ParserFixtureManifest.readFile(spanPath),
      ParserFixtureManifest.readFile(astPath),
      ParserFixtureManifest.readFile(ParserFixtureManifest.parserSourcePath),
    ).mkString("\n")

    val checked =
      Cosmo0().check(SourceFile(ParserFixtureManifest.parserSourcePath, source))

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"single-source parser validation failed: ${checked.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assert(checked.value.get.typed.decls.exists(_.name == "SyntaxParserResult"))
    assert(checked.value.get.typed.decls.exists(_.name == "parse_source_ast"))
