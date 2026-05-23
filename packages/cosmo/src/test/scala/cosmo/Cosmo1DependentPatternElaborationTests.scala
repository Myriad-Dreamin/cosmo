package cosmo

class Cosmo1DependentPatternElaborationTests extends TestBase:
  private val sourceFiles = List(
    "packages/cosmoc/src/types/profile.cos",
    "packages/cosmoc/src/types/dependent_pattern.cos",
    "packages/cosmoc/src/types/dependent_pattern_test.cos",
  )

  test("cosmoc dependent pattern source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse dependent pattern source: $path")
    }
  }

  test("cosmoc manifest includes dependent pattern elaboration slice") {
    val manifest =
      NodeFs.readFileSync("packages/cosmoc/cosmo.json", "utf8").asInstanceOf[String]

    assert(manifest.contains("\"types/dependent_pattern.cos\""))
    assert(!manifest.contains("\"types/dependent_pattern_test.cos\""))
  }

  test("dependent pattern fixtures cover profile gating, refinement, coverage, and equality rejection") {
    val profile =
      NodeFs.readFileSync("packages/cosmoc/src/types/profile.cos", "utf8").asInstanceOf[String]
    val elaborator =
      NodeFs.readFileSync("packages/cosmoc/src/types/dependent_pattern.cos", "utf8").asInstanceOf[String]
    val fixtures =
      NodeFs.readFileSync("packages/cosmoc/src/types/dependent_pattern_test.cos", "utf8").asInstanceOf[String]

    assert(profile.contains("def checker_profile_mltt_dependent_patterns("))
    assert(profile.contains("def checker_feature_dependent_pattern_elaboration("))
    assert(elaborator.contains("class DependentPatternConstructorDecl"))
    assert(elaborator.contains("class DependentPatternUnifier"))
    assert(elaborator.contains("class DependentCaseTree"))
    assert(elaborator.contains("def elaborate_dependent_pattern_clauses("))
    assert(elaborator.contains("def dependent_pattern_check_coverage("))
    assert(elaborator.contains("def dependent_pattern_add_nat_fixture("))
    assert(elaborator.contains("def dependent_pattern_add_vec_fixture("))
    assert(fixtures.contains("dependent_pattern_vec_head_elaborates_cons_and_omits_impossible_nil"))
    assert(fixtures.contains("dependent_pattern_vec_append_fixture_elaborates_constructor_split"))
    assert(fixtures.contains("dependent_pattern_variable_wildcard_and_impossible_patterns_elaborate"))
    assert(fixtures.contains("dependent_pattern_unsupported_profile_rejects_clauses"))
    assert(fixtures.contains("dependent_pattern_equality_pattern_is_rejected"))
    assert(fixtures.contains("dependent_pattern_coverage_reports_missing_branch"))
    assert(fixtures.contains("dependent_pattern_coverage_reports_redundant_branch_with_span"))
  }
end Cosmo1DependentPatternElaborationTests
