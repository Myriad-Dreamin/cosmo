package cosmo0

class Cosmo1BasicExpressionTypecheckingTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val symbolPath = "packages/cosmoc/src/names/symbol.cos"
  private val scopePath = "packages/cosmoc/src/names/scope.cos"
  private val resolutionPath = "packages/cosmoc/src/names/resolution.cos"
  private val modelPath = "packages/cosmoc/src/types/model.cos"
  private val declarationResolutionPath = "packages/cosmoc/src/types/declaration_resolution.cos"
  private val checkPath = "packages/cosmoc/src/types/check.cos"
  private val checkTestPath = "packages/cosmoc/src/types/check_test.cos"

  test("cosmo1 basic expression checker source compiles"):
    val source = combineSources(
      List(
        spanPath,
        astPath,
        symbolPath,
        scopePath,
        resolutionPath,
        modelPath,
        declarationResolutionPath,
        checkPath,
        checkTestPath,
      ),
    )

    val compiled = Cosmo0().compile(SourceFile(checkTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 basic expression checker compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.contains("struct TypedExpr"))
    assert(output.contains("struct LocalBinding"))
    assert(output.contains("struct ExpressionCheckResult"))
    assert(output.contains("inline ExpressionCheckResult check_module_basic_expressions("))
    assert(output.contains("inline bool basic_expression_checker_types_supported_expressions()"))
    assert(output.contains("inline bool basic_expression_checker_rejects_immutable_assignment()"))
    assert(output.contains("inline bool basic_expression_checker_reports_assignment_mismatch()"))
    assert(output.contains("inline bool basic_expression_checker_reports_return_mismatch()"))
    assert(output.contains("inline bool basic_expression_checker_rejects_invalid_binary_operands()"))
    assert(output.contains("int main()"))
    assert(output.contains("cosmo1.type.immutable-assignment"))
    assert(output.contains("cosmo1.type.invalid-binary-op"))
    assert(output.contains("cosmo1.type.mismatch"))

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1BasicExpressionTypecheckingTests
