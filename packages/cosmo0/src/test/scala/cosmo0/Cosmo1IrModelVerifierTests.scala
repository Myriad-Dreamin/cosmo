package cosmo0

class Cosmo1IrModelVerifierTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val symbolPath = "packages/cosmoc/src/names/symbol.cos"
  private val scopePath = "packages/cosmoc/src/names/scope.cos"
  private val resolutionPath = "packages/cosmoc/src/names/resolution.cos"
  private val modelPath = "packages/cosmoc/src/types/model.cos"
  private val declarationResolutionPath = "packages/cosmoc/src/types/declaration_resolution.cos"
  private val irModelPath = "packages/cosmoc/src/ir/model.cos"
  private val irLoweringPath = "packages/cosmoc/src/ir/lowering.cos"
  private val irVerifierTestPath = "packages/cosmoc/src/ir/verifier_test.cos"

  test("cosmo1 IR model, verifier, renderer, and fixtures compile"):
    val source = combineSources(
      List(
        spanPath,
        astPath,
        symbolPath,
        scopePath,
        resolutionPath,
        modelPath,
        declarationResolutionPath,
        irModelPath,
        irLoweringPath,
        irVerifierTestPath,
      ),
    )
    val compiled = Cosmo0().compile(SourceFile(irVerifierTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 IR verifier compile failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.output
    assert(output.contains("struct IrModule"))
    assert(output.contains("struct IrTypeDecl"))
    assert(output.contains("struct IrField"))
    assert(output.contains("struct IrFunction"))
    assert(output.contains("struct IrBlock"))
    assert(output.contains("struct IrLocal"))
    assert(output.contains("struct IrValue"))
    assert(output.contains("struct IrOperation"))
    assert(output.contains("struct IrTerminator"))
    assert(output.contains("inline IrVerifyResult verify_ir_module("))
    assert(output.contains("inline IrModule lower_module_declarations("))
    assert(output.contains("inline std::string render_ir_module("))
    assert(output.contains("inline bool ir_verifier_accepts_valid_function()"))
    assert(output.contains("inline bool ir_verifier_rejects_missing_block()"))
    assert(output.contains("inline bool ir_verifier_rejects_use_before_definition()"))
    assert(output.contains("inline bool ir_verifier_rejects_invalid_call_shape()"))
    assert(output.contains("inline bool ir_verifier_rejects_return_type_mismatch()"))
    assert(output.contains("inline bool ir_debug_renderer_is_deterministic()"))
    assert(output.contains("inline bool ir_declaration_lowerer_records_parser_state_and_helpers()"))

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1IrModelVerifierTests
