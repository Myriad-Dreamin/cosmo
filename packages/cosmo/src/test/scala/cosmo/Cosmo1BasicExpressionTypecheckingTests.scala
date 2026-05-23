package cosmo

class Cosmo1BasicExpressionTypecheckingTests extends TestBase:
  private val sourceFiles = List(
    "packages/cosmoc/src/types/profile.cos",
    "packages/cosmoc/src/types/check.cos",
    "packages/cosmoc/src/types/check_test.cos",
  )

  test("cosmoc basic expression checker source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse basic expression checker source: $path")
    }
  }

  test("cosmoc manifest includes basic expression checker slice") {
    val manifest =
      NodeFs.readFileSync("packages/cosmoc/cosmo.json", "utf8").asInstanceOf[String]

    assert(manifest.contains("\"types/profile.cos\""))
    assert(manifest.contains("\"types/check.cos\""))
    assert(!manifest.contains("\"types/check_test.cos\""))
  }

  test("basic expression checker fixtures declare the checker entry points") {
    val profile =
      NodeFs.readFileSync("packages/cosmoc/src/types/profile.cos", "utf8").asInstanceOf[String]
    val checker =
      NodeFs.readFileSync("packages/cosmoc/src/types/check.cos", "utf8").asInstanceOf[String]
    val fixtures =
      NodeFs.readFileSync("packages/cosmoc/src/types/check_test.cos", "utf8").asInstanceOf[String]

    assert(profile.contains("class CheckerProfile"))
    assert(profile.contains("def checker_profile_cosmoc_basic_expr("))
    assert(profile.contains("def checker_profile_cosmo0_subset("))
    assert(profile.contains("def checker_profile_mltt_core("))
    assert(profile.contains("def checker_profile_mltt_dependent_patterns("))
    assert(profile.contains("cosmoc.basic-expr"))
    assert(profile.contains("cosmo0.subset"))
    assert(profile.contains("mltt.core"))
    assert(profile.contains("mltt.dependent-patterns"))
    assert(checker.contains("class TypedExpr"))
    assert(checker.contains("class TypedExprKind"))
    assert(checker.contains("class LocalBinding"))
    assert(checker.contains("class ExpressionCheckResult"))
    assert(checker.contains("class ExpressionChecker"))
    assert(checker.contains("val profile_id: String"))
    assert(checker.contains("val artifact_kind: String"))
    assert(checker.contains("case If(ExprId, ExprId, ExprId)"))
    assert(checker.contains("case While(ExprId, ExprId)"))
    assert(checker.contains("def check_module_basic_expressions("))
    assert(checker.contains("def check_module_basic_expressions_with_profile("))
    assert(checker.contains("def check_module_basic_expressions_with_resolution("))
    assert(checker.contains("def check_module_with_profile("))
    assert(fixtures.contains("checker_profile_metadata_declares_basic_and_mltt_features"))
    assert(fixtures.contains("basic_expression_checker_result_names_profile"))
    assert(fixtures.contains("basic_expression_checker_reports_unsupported_decl_as_result"))
    assert(fixtures.contains("mltt_core_profile_rejects_object_source_without_artifacts"))
    assert(fixtures.contains("mltt_core_profile_reserves_dependent_patterns"))
    assert(fixtures.contains("basic_expression_checker_artifact_summary_is_deterministic"))
    assert(fixtures.contains("basic_expression_checker_types_supported_expressions"))
    assert(fixtures.contains("basic_expression_checker_rejects_immutable_assignment"))
    assert(fixtures.contains("basic_expression_checker_reports_assignment_mismatch"))
    assert(fixtures.contains("basic_expression_checker_reports_return_mismatch"))
    assert(fixtures.contains("basic_expression_checker_rejects_invalid_binary_operands"))
    assert(fixtures.contains("control_flow_checker_types_if_else_and_while"))
    assert(fixtures.contains("control_flow_checker_rejects_bad_conditions"))
    assert(fixtures.contains("control_flow_checker_reports_missing_return"))
  }
end Cosmo1BasicExpressionTypecheckingTests
