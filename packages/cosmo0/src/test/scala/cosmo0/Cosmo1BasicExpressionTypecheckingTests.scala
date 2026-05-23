package cosmo0

class Cosmo1BasicExpressionTypecheckingTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val symbolPath = "packages/cosmoc/src/names/symbol.cos"
  private val scopePath = "packages/cosmoc/src/names/scope.cos"
  private val resolutionPath = "packages/cosmoc/src/names/resolution.cos"
  private val modelPath = "packages/cosmoc/src/types/model.cos"
  private val profilePath = "packages/cosmoc/src/types/profile.cos"
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
        profilePath,
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
    assert(output.contains("struct CheckerProfile"))
    assert(output.contains("struct CheckerFeatureRejection"))
    assert(output.contains("struct TypedExpr"))
    assert(output.contains("struct LocalBinding"))
    assert(output.contains("struct ExpressionCheckResult"))
    assert(output.contains("inline CheckerProfile checker_profile_cosmoc_basic_expr()"))
    assert(output.contains("inline CheckerProfile checker_profile_mltt_dependent_patterns()"))
    assert(output.contains("inline bool checker_profile_metadata_declares_basic_and_mltt_features()"))
    assert(output.contains("inline bool basic_expression_checker_result_names_profile()"))
    assert(output.contains("inline bool basic_expression_checker_reports_unsupported_decl_as_result()"))
    assert(output.contains("inline bool mltt_core_profile_rejects_object_source_without_artifacts()"))
    assert(output.contains("inline bool mltt_core_profile_reserves_dependent_patterns()"))
    assert(output.contains("inline bool basic_expression_checker_artifact_summary_is_deterministic()"))
    assert(output.contains("inline ExpressionCheckResult check_module_basic_expressions("))
    assert(output.contains("inline ExpressionCheckResult check_module_with_profile("))
    assert(output.contains("inline bool basic_expression_checker_types_supported_expressions()"))
    assert(output.contains("inline bool basic_expression_checker_rejects_immutable_assignment()"))
    assert(output.contains("inline bool basic_expression_checker_reports_assignment_mismatch()"))
    assert(output.contains("inline bool basic_expression_checker_reports_return_mismatch()"))
    assert(output.contains("inline bool basic_expression_checker_reports_body_type_mismatch()"))
    assert(output.contains("inline bool basic_expression_checker_accepts_inferable_unit_body()"))
    assert(output.contains("inline bool basic_expression_checker_preserves_unknown_name()"))
    assert(output.contains("inline bool basic_expression_checker_preserves_invalid_assignment_target()"))
    assert(output.contains("inline bool basic_expression_checker_rejects_invalid_binary_operands()"))
    assert(output.contains("inline bool member_call_checker_types_parser_state_patterns()"))
    assert(output.contains("inline bool member_call_checker_rejects_immutable_self_mutation()"))
    assert(output.contains("inline bool control_flow_checker_types_if_else_and_while()"))
    assert(output.contains("inline bool control_flow_checker_rejects_bad_conditions()"))
    assert(output.contains("inline bool control_flow_checker_reports_missing_return()"))
    assert(output.contains("int main()"))
    assert(output.contains("cosmo1.type.immutable-assignment"))
    assert(output.contains("cosmo1.type.invalid-binary-op"))
    assert(output.contains("cosmo1.type.immutable-receiver"))
    assert(output.contains("cosmo1.type.expected-bool"))
    assert(output.contains("cosmo1.type.missing-return"))
    assert(output.contains("cosmo1.type.mismatch"))
    assert(output.contains("cosmo.type.unsupported-dependent-pattern"))
    assert(output.contains("cosmo.type.unsupported-object-dispatch"))
    assert(output.contains("mltt.dependent-patterns"))
    assert(output.contains("dependent-pattern-elaboration"))

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1BasicExpressionTypecheckingTests
