package cosmo

class Cosmo1MlttTypeCheckerTests extends TestBase:
  private val sourceFiles = List(
    "packages/cosmoc/src/types/profile.cos",
    "packages/cosmoc/src/types/mltt/core.cos",
    "packages/cosmoc/src/types/mltt/check.cos",
    "packages/cosmoc/src/types/mltt/check_test.cos",
  )

  test("cosmoc MLTT checker source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse MLTT checker source: $path")
    }
  }

  test("cosmoc manifest includes MLTT checker implementation slice") {
    val manifest =
      NodeFs.readFileSync("packages/cosmoc/cosmo.json", "utf8").asInstanceOf[String]

    assert(manifest.contains("\"types/mltt/core.cos\""))
    assert(manifest.contains("\"types/mltt/check.cos\""))
    assert(!manifest.contains("\"types/mltt/check_test.cos\""))
  }

  test("MLTT checker fixtures cover core data, checking, conversion, and profile diagnostics") {
    val core =
      NodeFs.readFileSync("packages/cosmoc/src/types/mltt/core.cos", "utf8").asInstanceOf[String]
    val checker =
      NodeFs.readFileSync("packages/cosmoc/src/types/mltt/check.cos", "utf8").asInstanceOf[String]
    val fixtures =
      NodeFs.readFileSync("packages/cosmoc/src/types/mltt/check_test.cos", "utf8").asInstanceOf[String]

    assert(core.contains("class MlttTerm"))
    assert(core.contains("case Pi(String, MlttTermId, MlttTermId)"))
    assert(core.contains("case Sigma(String, MlttTermId, MlttTermId)"))
    assert(core.contains("case Eq(MlttTermId, MlttTermId, MlttTermId)"))
    assert(core.contains("class MlttContextEntry"))
    assert(core.contains("class MlttInductiveDecl"))
    assert(core.contains("class MlttMetaVar"))
    assert(core.contains("def mltt_add_nat_fixture("))
    assert(core.contains("def mltt_add_vec_fixture("))
    assert(checker.contains("class MlttConversionResult"))
    assert(checker.contains("def mltt_check("))
    assert(checker.contains("def mltt_infer("))
    assert(checker.contains("def mltt_convert_with_strategy("))
    assert(checker.contains("def mltt_whnf("))
    assert(checker.contains("mltt.whnf-conversion"))
    assert(fixtures.contains("mltt_lambda_checks_against_pi"))
    assert(fixtures.contains("mltt_application_infers_through_pi"))
    assert(fixtures.contains("mltt_conversion_accepts_beta_and_let"))
    assert(fixtures.contains("mltt_conversion_reduces_transparent_nat_definition"))
    assert(fixtures.contains("mltt_conversion_rejects_effectful_definition"))
    assert(fixtures.contains("mltt_conversion_rejects_unknown_normalization_profile"))
    assert(fixtures.contains("mltt_vec_constructor_signatures_check_without_pattern_matching"))
    assert(fixtures.contains("mltt_nat_and_vec_declaration_metadata_is_deterministic"))
    assert(fixtures.contains("mltt_metavariables_are_local_and_reportable"))
  }
end Cosmo1MlttTypeCheckerTests
