package cosmo0

class Cosmo1NameResolutionTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val symbolPath = "packages/cosmoc/src/names/symbol.cos"
  private val scopePath = "packages/cosmoc/src/names/scope.cos"
  private val resolutionPath = "packages/cosmoc/src/names/resolution.cos"
  private val resolutionTestPath = "packages/cosmoc/src/names/resolution_test.cos"

  test("cosmo1 name resolution source compiles with parser subset fixtures"):
    val source = combineSources(
      List(
        spanPath,
        astPath,
        symbolPath,
        scopePath,
        resolutionPath,
        resolutionTestPath,
      ),
    )

    val compiled = Cosmo0().compile(SourceFile(resolutionTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 name resolution compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.contains("struct SymbolInterner"))
    assert(output.contains("struct NameResolution"))
    assert(output.contains("inline NameResolution resolve_module_names("))
    assert(output.contains("inline bool name_resolution_test_resolves_parser_subset_fixture()"))
    assert(output.contains("inline bool name_resolution_test_reports_duplicate_definition()"))
    assert(output.contains("inline bool name_resolution_test_allows_repeated_same_import()"))
    assert(output.contains("inline bool name_resolution_test_reports_unresolved_name()"))
    assert(output.contains("int main()"))
    assert(output.contains("cosmo1.name.duplicate-definition"))
    assert(output.contains("cosmo1.name.unresolved-name"))

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1NameResolutionTests
