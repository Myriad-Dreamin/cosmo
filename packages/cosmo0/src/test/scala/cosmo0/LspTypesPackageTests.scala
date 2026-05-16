package cosmo0

class LspTypesPackageTests extends munit.FunSuite:
  test("lsp-types generator package compiles with checked-in generated core subset"):
    val path = "packages/lsp-types"
    val loaded = Cosmo0().loadPackage(path)

    assertEquals(loaded.phase, Phase.Check)
    assert(
      loaded.isSuccess,
      s"lsp-types package load failed with diagnostics: ${loaded.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(loaded.value.get.metadata.stageProfile, Some(StageCapabilityRegistry.Cosmo1Stage1))
    assertEquals(
      loaded.value.get.modules.map(_.modulePath),
      List(
        List("core0", "json"),
        List("core0", "path_fs"),
        List("core0", "text"),
        List("core0", "text_output"),
        List("generator", "emit"),
        List("generator", "metamodel"),
        List("lsp", "core"),
        List("main"),
      ),
    )

    val compiled = Cosmo0().compileRunnablePackage(path)

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"lsp-types package compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assertEquals(output.moduleName, "cosmo_lsp_types")
    assert(output.source.contains("struct Position"))
    assert(output.source.contains("struct Diagnostic"))
    assert(output.source.contains("struct InitializeRequest"))
    assert(output.source.contains("inline int32_t main()"))
