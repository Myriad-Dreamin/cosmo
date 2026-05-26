package cosmo0

class Cosmo1TypeModelTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val modelPath = "packages/cosmoc/src/types/model.cos"
  private val modelTestPath = "packages/cosmoc/src/types/model_test.cos"

  test("cosmo1 type model fixture source compiles"):
    val source = combineSources(List(spanPath, modelPath, modelTestPath))
    val compiled = Cosmo0().compile(SourceFile(modelTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 type model compile failed with diagnostics: ${compiled.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val output = compiled.value.get.output
    assert(output.contains("struct CosmoType"))
    assert(output.contains("struct TypeStore"))
    assert(output.contains("inline bool type_model_primitive_smoke()"))
    assert(output.contains("inline bool type_model_user_and_reference_smoke()"))
    assert(output.contains("inline bool type_model_function_smoke()"))
    assert(
      output.contains(
        "inline bool type_model_special_and_diagnostic_smoke()",
      ),
    )
    assert(output.contains("type mismatch: expected Unit but found never"))

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1TypeModelTests
