package cosmo0

class Cosmo1DeclarationResolutionTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val symbolPath = "packages/cosmoc/src/names/symbol.cos"
  private val scopePath = "packages/cosmoc/src/names/scope.cos"
  private val resolutionPath = "packages/cosmoc/src/names/resolution.cos"
  private val modelPath = "packages/cosmoc/src/types/model.cos"
  private val declarationResolutionPath =
    "packages/cosmoc/src/types/declaration_resolution.cos"
  private val declarationResolutionTestPath =
    "packages/cosmoc/src/types/declaration_resolution_test.cos"

  test("cosmo1 declaration resolution source compiles"):
    val source = combineSources(
      List(
        spanPath,
        astPath,
        symbolPath,
        scopePath,
        resolutionPath,
        modelPath,
        declarationResolutionPath,
        declarationResolutionTestPath,
      ),
    )

    val compiled =
      Cosmo0().compile(SourceFile(declarationResolutionTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 declaration resolution compile failed with diagnostics: ${compiled.diagnostics
          .map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.contains("struct DeclarationResolution"))
    assert(output.contains("struct ParameterSignature"))
    assert(output.contains("struct CallableSignature"))
    assert(
      output.contains(
        "inline DeclarationResolution resolve_module_declarations(",
      ),
    )
    assert(
      output.contains(
        "inline bool declaration_resolution_test_records_parser_state_and_helpers()",
      ),
    )
    assert(
      output.contains(
        "inline bool declaration_resolution_test_reports_unknown_type()",
      ),
    )
    assert(
      output.contains(
        "inline bool declaration_resolution_test_reports_alias_cycle()",
      ),
    )
    assert(
      output.contains(
        "inline bool declaration_resolution_test_rejects_invalid_receiver()",
      ),
    )
    assert(output.contains("int main()"))
    assert(output.contains("cosmo1.type.unknown-type"))
    assert(output.contains("cosmo1.type.alias-cycle"))
    assert(output.contains("cosmo1.type.invalid-receiver"))

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1DeclarationResolutionTests
