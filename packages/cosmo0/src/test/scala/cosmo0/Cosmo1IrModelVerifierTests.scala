package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

class Cosmo1IrModelVerifierTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val parserPath = "packages/cosmoc/src/parser.cos"
  private val symbolPath = "packages/cosmoc/src/names/symbol.cos"
  private val scopePath = "packages/cosmoc/src/names/scope.cos"
  private val resolutionPath = "packages/cosmoc/src/names/resolution.cos"
  private val modelPath = "packages/cosmoc/src/types/model.cos"
  private val declarationResolutionPath = "packages/cosmoc/src/types/declaration_resolution.cos"
  private val typeCheckPath = "packages/cosmoc/src/types/check.cos"
  private val irModelPath = "packages/cosmoc/src/ir/model.cos"
  private val irLoweringPath = "packages/cosmoc/src/ir/lowering.cos"
  private val irVerifierTestPath = "packages/cosmoc/src/ir/verifier_test.cos"

  test("cosmo1 IR model, verifier, renderer, and fixtures compile"):
    val source = combineSources(
      List(
        spanPath,
        astPath,
        parserPath,
        symbolPath,
        scopePath,
        resolutionPath,
        modelPath,
        declarationResolutionPath,
        typeCheckPath,
        irModelPath,
        irLoweringPath,
        irVerifierTestPath,
      ),
    )
    val compiled = Cosmo0().compile(SourceFile(irVerifierTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 IR verifier compile failed with diagnostics: ${compiled.diagnostics.map(d => (d.code, d.message, d.span.map(span => span.start -> span.end)))}",
    )
    val output = compiled.value.get.output
    assert(output.contains("struct IrModule"))
    assert(output.contains("struct SyntaxParserResult"))
    assert(output.contains("inline SyntaxParserResult parse_source_ast("))
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
    assert(output.contains("inline bool ir_basic_expression_lowerer_accepts_simple_parser_source()"))
    assert(output.contains("inline bool ir_member_intrinsic_lowerer_accepts_parser_state_methods()"))
    assert(output.contains("inline bool ir_control_flow_lowerer_accepts_parser_style_flow()"))
    assert(output.contains("int main()"))
    assertCxxAccepts(output)

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")

  private def assertCxxAccepts(source: String): Unit =
    val compiler = cxxCompiler().getOrElse(fail("no C++ compiler found for cosmo1 IR C++ acceptance test"))
    val result = IrNodeSpawnSync(
      compiler,
      js.Array("-std=c++17", "-Itarget/cosmo/externals/json/single_include", "-fsyntax-only", "-x", "c++", "-"),
      js.Dynamic.literal(input = source, encoding = "utf8"),
    )
    assertEquals(
      result.status.toOption,
      Some(0),
      s"C++ compiler rejected cosmo1 IR verifier output with ${compiler}\n${result.stderr.getOrElse("")}",
    )

  private def cxxCompiler(): Option[String] =
    List("c++", "g++", "clang++").find { command =>
      val result = IrNodeSpawnSync(
        command,
        js.Array("--version"),
        js.Dynamic.literal(encoding = "utf8"),
      )
      result.status.toOption.contains(0)
    }

end Cosmo1IrModelVerifierTests

@js.native
@JSImport("node:child_process", "spawnSync")
private object IrNodeSpawnSync extends js.Object:
  def apply(command: String, args: js.Array[String], options: js.Any): IrNodeSpawnSyncResult = js.native

@js.native
private trait IrNodeSpawnSyncResult extends js.Object:
  val status: js.UndefOr[Int] = js.native
  val stdout: js.UndefOr[String] = js.native
  val stderr: js.UndefOr[String] = js.native
