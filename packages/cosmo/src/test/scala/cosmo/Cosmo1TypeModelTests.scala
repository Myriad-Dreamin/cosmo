package cosmo

class Cosmo1TypeModelTest extends TestBase:
  private val sourceFiles = List(
    "packages/cosmoc/src/types/model.cos",
    "packages/cosmoc/src/types/model_test.cos",
  )

  test("cosmoc type model source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse type model source: $path")
    }
  }

  test("cosmoc manifest includes type model slice") {
    val manifest =
      NodeFs.readFileSync("packages/cosmoc/cosmo.json", "utf8").asInstanceOf[String]

    assert(manifest.contains("\"types/model.cos\""))
    assert(manifest.contains("\"types/model_test.cos\""))
  }

  test("type model fixtures cover primitive, user, function, reference, and diagnostics") {
    val model =
      NodeFs.readFileSync("packages/cosmoc/src/types/model.cos", "utf8").asInstanceOf[String]
    val fixtures =
      NodeFs.readFileSync("packages/cosmoc/src/types/model_test.cos", "utf8").asInstanceOf[String]

    assert(model.contains("type TypeId = Id[CosmoType]"))
    assert(model.contains("def type_equals(types: &TypeStore, left: TypeId, right: TypeId): Bool"))
    assert(model.contains("def type_display(types: &TypeStore, id: TypeId): String"))
    assert(model.contains("def type_mismatch_diagnostic(span: Span, expected: TypeId, actual: TypeId): TypeDiagnostic"))
    assert(fixtures.contains("type_model_primitive_smoke"))
    assert(fixtures.contains("type_model_user_and_reference_smoke"))
    assert(fixtures.contains("type_model_function_smoke"))
    assert(fixtures.contains("type_model_special_and_diagnostic_smoke"))
  }
end Cosmo1TypeModelTest
