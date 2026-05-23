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
  private val profilePath = "packages/cosmoc/src/types/profile.cos"
  private val declarationResolutionPath = "packages/cosmoc/src/types/declaration_resolution.cos"
  private val typeCheckPath = "packages/cosmoc/src/types/check.cos"
  private val irModelPath = "packages/cosmoc/src/ir/model.cos"
  private val irLoweringPath = "packages/cosmoc/src/ir/lowering.cos"
  private val irVerifierTestPath = "packages/cosmoc/src/ir/verifier_test.cos"

  test("cosmo1 IR model, verifier, renderer, and fixtures compile"):
    val compiled = compileIrVerifierTestProgram()

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
    assert(output.contains("inline bool ir_self_compile_parser_source_runs()"))
    assert(output.contains("int main()"))
    assertCxxAccepts(output)

  test("cosmo1 IR verifier executable self-compiles parser.cos"):
    val compiled = compileParserSelfCompileProgram()

    assert(
      compiled.isSuccess,
      s"cosmo1 parser self-compile program failed with diagnostics: ${compiled.diagnostics.map(d => (d.code, d.message, d.span.map(span => span.start -> span.end)))}",
    )
    val output = compiled.value.get.output
    val compiler = cxxCompiler().getOrElse(fail("no C++ compiler found for cosmo1 parser self-compile execution test"))
    IrTestNodeFs.mkdirSync("target/cosmo1-ir-tests", js.Dynamic.literal("recursive" -> true))
    val sourcePath = "target/cosmo1-ir-tests/ir_verifier_test.cpp"
    val executablePath = "target/cosmo1-ir-tests/ir_verifier_test"
    IrTestNodeFs.writeFileSync(sourcePath, output)

    val compile = IrNodeSpawnSync(
      compiler,
      js.Array("-std=c++17", "-O2", NlohmannJsonDependency.includeArg, sourcePath, "-o", executablePath),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      compile.status.toOption,
      Some(0),
      s"C++ compiler rejected cosmo1 IR verifier executable output with ${compiler}\n${compile.stderr.getOrElse("")}",
    )

    val run = IrNodeSpawnSync(
      executablePath,
      js.Array(),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      run.status.toOption,
      Some(0),
      s"cosmo1 parser self-compile executable failed\nstdout:\n${run.stdout.getOrElse("")}\nstderr:\n${run.stderr.getOrElse("")}",
    )

  private def compileIrVerifierTestProgram(): Result[CompiledModule] =
    val source = combineSources(
      List(
        spanPath,
        astPath,
        parserPath,
        symbolPath,
        scopePath,
        resolutionPath,
        modelPath,
        profilePath,
        declarationResolutionPath,
        typeCheckPath,
        irModelPath,
        irLoweringPath,
        irVerifierTestPath,
      ),
    )
    Cosmo0().compile(SourceFile(irVerifierTestPath, source))

  private def compileParserSelfCompileProgram(): Result[CompiledModule] =
    val source = combineSources(
      List(
        spanPath,
        astPath,
        parserPath,
        symbolPath,
        scopePath,
        resolutionPath,
        modelPath,
        profilePath,
        declarationResolutionPath,
        typeCheckPath,
        irModelPath,
        irLoweringPath,
      ),
    ) + "\n" + parserSelfCompileMainSource
    Cosmo0().compile(SourceFile("packages/cosmoc/src/ir/parser_self_compile_test.cos", source))

  private def parserSelfCompileMainSource: String =
    """def ir_self_compile_parser_source_runs(): Bool = {
      |  val source = read_file("packages/cosmoc/src/source/span.cos")
      |    .concat("\n")
      |    .concat(read_file("packages/cosmoc/src/syntax/ast.cos"))
      |    .concat("\n")
      |    .concat(read_file("packages/cosmoc/src/parser.cos"));
      |  val parsed = parse_source_ast(source);
      |  if (!parsed.is_ok()) {
      |    println(parsed.first_diagnostic_code());
      |    return false
      |  }
      |
      |  val names = resolve_module_names(parsed.arenas, parsed.root);
      |  val resolved = resolve_module_declarations_with_names(parsed.arenas, parsed.root, names);
      |  val checked = check_module_basic_expressions_with_resolution(parsed.arenas, parsed.root, names, resolved);
      |  if (!names.is_ok()) {
      |    println(names.first_diagnostic_code());
      |    println(names.first_diagnostic_message());
      |    return false
      |  }
      |  if (!resolved.is_ok()) {
      |    println(resolved.first_diagnostic_code());
      |    return false
      |  }
      |  if (!checked.is_ok()) {
      |    println(checked.first_diagnostic_code());
      |    println(checked.first_diagnostic_message());
      |    println(checked.first_diagnostic_span_text());
      |    println(source.slice(checked.first_diagnostic_span_start(), checked.first_diagnostic_span_end()));
      |    return false
      |  }
      |
      |  val ir_module = lower_module_declarations(parsed.arenas, parsed.root, names, resolved);
      |  val verified = verify_ir_module(checked.types, ir_module);
      |  if (!verified.is_ok()) {
      |    println(verified.first_code());
      |    return false
      |  }
      |
      |  val first_render = render_ir_module(checked.types, ir_module);
      |  val second_render = render_ir_module(checked.types, ir_module);
      |  first_render == second_render and first_render != ""
      |}
      |
      |def main(): i32 = {
      |  if (!ir_self_compile_parser_source_runs()) {
      |    return 1
      |  }
      |  0
      |}
      |""".stripMargin

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
      js.Array("-std=c++17", NlohmannJsonDependency.includeArg, "-fsyntax-only", "-x", "c++", "-"),
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

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object IrTestNodeFs extends js.Object:
  def mkdirSync(path: String, options: js.Any): Unit = js.native
  def writeFileSync(path: String, data: String): Unit = js.native
