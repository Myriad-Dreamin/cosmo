package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

class CppBackendTests extends munit.FunSuite:
  private val tokenType = SourceType.User("Token")
  private val optionTokenType = SourceType.Standard("Option", List(tokenType))
  private val stringVecType = SourceType.Standard("Vec", List(SourceType.String))
  private val spanSourcePath = "packages/cosmoc/src/source/span.cos"
  private val syntaxAstSourcePath = "packages/cosmoc/src/syntax/ast.cos"
  private val symbolSourcePath = "packages/cosmoc/src/names/symbol.cos"
  private val scopeSourcePath = "packages/cosmoc/src/names/scope.cos"
  private val resolutionSourcePath = "packages/cosmoc/src/names/resolution.cos"
  private val typeModelSourcePath = "packages/cosmoc/src/types/model.cos"
  private val declarationResolutionSourcePath = "packages/cosmoc/src/types/declaration_resolution.cos"
  private val typeCheckSourcePath = "packages/cosmoc/src/types/check.cos"

  test("backend rejects structurally invalid LIR at the compile boundary"):
    val result = CppBackend().emit(
      LirModule(
        "bad",
        List(
          Lir.function(
            "bad",
            params = Nil,
            returnType = SourceType.I32,
            locals = Nil,
            blocks = Nil,
          ),
        ),
      ),
    )

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.lir.missing-block"),
      s"missing LIR boundary diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("backend emits declarations, bodies, descriptors, variants, and runtime support"):
    val result = CppBackend().emit(checkedLirModule())

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"C++ emission failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val source = result.value.get.source
    assert(source.contains("namespace cosmo0 {"))
    assert(source.contains("namespace checked {"))
    assert(source.contains("struct Token"))
    assert(source.contains("Token() : data(Variant_Empty{}) {}"))
    assert(source.contains("std::variant<Variant_Empty, Variant_WithText> data;"))
    assert(source.contains("inline int32_t identity(int32_t value);"))
    assert(source.contains("tmp = token.offset;"))
    assert(source.contains("labels.push_back(std::string(\"ok\"));"))
    assert(source.contains("maybe = std::optional<Token>(token);"))
    assert(source.contains("is_some = maybe.has_value();"))
    assert(source.contains("payload = maybe.value();"))
    assert(!source.contains("CodeGen"))

  test("backend output is byte-for-byte stable for equivalent LIR ordering"):
    val first = checkedLirModule()
    val second = LirModule(
      "checked",
      List(
        checkedProcessFunction(
          locals = checkedLocals().reverse,
          blocks = checkedBlocks().reverse,
        ),
        identityFunction(),
        tokenDecl().copy(
          fields = tokenDecl().fields.reverse,
          variants = tokenDecl().variants.reverse,
        ),
      ),
    )

    val firstOutput = CppBackend().emit(first)
    val secondOutput = CppBackend().emit(second)

    assert(firstOutput.isSuccess, firstOutput.diagnostics.map(_.message).mkString("\n"))
    assert(secondOutput.isSuccess, secondOutput.diagnostics.map(_.message).mkString("\n"))
    assertEquals(firstOutput.value.get.source, secondOutput.value.get.source)

  test("backend diagnoses descriptor operations that are valid LIR but unsupported by C++ emission"):
    val boxType = SourceType.Standard("Box", List(SourceType.I32))
    val refType = SourceType.Ref(SourceType.I32, mutable = false)
    val module = LirModule(
      "unsupported",
      List(
        Lir.function(
          "read_box",
          List(Lir.param("box", boxType)),
          refType,
          locals = List(Lir.local("value", refType)),
          blocks = List(
            Lir.block(
              "entry",
              operations = List(
                LirDescriptorIntrinsic(
                  Some(Lir.localId("value")),
                  LirDescriptorRef("Box", List(Lir.t(SourceType.I32))),
                  "get",
                  List(Lir.ref("box", boxType)),
                  Some(Lir.t(refType)),
                ),
              ),
              terminator = LirReturn(Some(Lir.ref("value", refType))),
            ),
          ),
        ),
      ),
    )

    val result = CppBackend().emit(module)

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.cpp.unsupported-descriptor"),
      s"missing backend unsupported descriptor diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("Cosmo0 compile emits C++ for the HelloWorld executable target"):
    val result = Cosmo0().compile(
      SourceFile(
        "samples/HelloWorld/main.cos",
        """def main() = {
          |  println("Hello, World!")
          |
          |  val x = 1
          |  val y = 2
          |  val z = x + y
          |  println(z);
          |}
          |""".stripMargin,
      ),
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get.output
    assert(output.contains("int main()"))
    assert(output.contains("::cosmo0_runtime::println(std::string(\"Hello, World!\"));"))
    assert(output.contains("::cosmo0_runtime::println(z);"))
    assertCxxAccepts(output)

  test("Cosmo0 compile supports mutable references to generic Vec values"):
    val result = Cosmo0().compile(
      SourceFile(
        "mut_vec.cos",
        """def push_label(labels: &mut Vec[String]): Unit = {
          |  labels.push("ok")
          |}
          |
          |def main(): Unit = {
          |  val labels = Vec[String]();
          |  push_label(labels)
          |}
          |""".stripMargin,
      ),
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get.output
    assert(output.contains("push_label(std::vector<std::string> * labels)"))
    assert(output.contains("(*labels).push_back(std::string(\"ok\"));"))
    assert(output.contains("push_label(&labels);"))
    assertCxxAccepts(output)

  test("Cosmo0 compile passes mutable references to readonly parameters"):
    val result = Cosmo0().compile(
      SourceFile(
        "mut_to_readonly_ref.cos",
        """def read_len(labels: &Vec[String]): usize = {
          |  labels.len()
          |}
          |
          |def pass_len(labels: &mut Vec[String]): usize = {
          |  read_len(labels)
          |}
          |
          |def main(): Unit = {
          |  val labels = Vec[String]();
          |  val size: usize = pass_len(labels)
          |}
          |""".stripMargin,
      ),
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get.output
    assert(output.contains("read_len(const std::vector<std::string> * labels)"))
    assert(output.contains("pass_len(std::vector<std::string> * labels)"))
    assert(output.contains("read_len(labels);"))
    assert(output.contains("pass_len(&labels);"))
    assertCxxAccepts(output)

  test("backend emits extern-bound std calls and typed runtime requirements"):
    val lowered = Cosmo0().lower(
      SourceFile(
        "extern_smoke.cos",
        """def println(value: String): Unit
          |
          |def smoke(): Unit = {
          |  println("cosmo1 extern smoke")
          |}
          |""".stripMargin,
      ),
    )

    assert(
      lowered.isSuccess,
      s"lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val result = CppBackend().emit(lowered.value.get.lir)

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"C++ emission failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get
    assert(output.source.contains("::cosmo0_runtime::println(std::string(\"cosmo1 extern smoke\"));"))
    assert(output.backendRequirements.contains(BackendRequirement.runtimeSymbol("cosmo0_runtime::println")))
    assert(output.backendRequirements.contains(BackendRequirement.include("<cstdio>")))
    assert(output.runtimeRequirements.contains("runtime-symbol:cosmo0_runtime::println"))
    assert(!output.runtimeRequirements.contains("Runtime"))
    assertCxxAccepts(output.source)

  test("backend emits direct C extern calls and include requirements"):
    val lowered = Cosmo0().lower(
      SourceFile(
        "direct_c_extern.cos",
        """@include("stdio.h");
          |@include("stdlib.h", kind = "c");
          |@extern("c", name = "abs")
          |def c_abs(value: i32): i32
          |
          |def use(value: i32): i32 = {
          |  c_abs(value)
          |}
          |""".stripMargin,
      ),
    )

    assert(
      lowered.isSuccess,
      s"lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val result = CppBackend().emit(lowered.value.get.lir)

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"C++ emission failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get
    assert(output.source.contains("#include <stdio.h>"))
    assert(output.source.indexOf("#include <stdio.h>") < output.source.indexOf("#include <stdlib.h>"))
    assert(output.source.contains("#include <stdlib.h>"))
    assert(output.source.contains("abs(value);"))
    assert(output.backendRequirements.contains(BackendRequirement.runtimeSymbol("abs")))
    assert(output.backendRequirements.contains(BackendRequirement.include("<stdio.h>")))
    assert(output.backendRequirements.contains(BackendRequirement.include("<stdlib.h>")))
    assert(output.runtimeRequirements.contains("runtime-symbol:abs"))
    assertCxxAccepts(output.source)

  test("backend diagnoses missing extern runtime symbols"):
    val binding = LirExternBinding(
      TrustedExternAbi.abiName,
      CppQualifiedSymbol.global("cosmo0_runtime", "missing"),
      List(BackendRequirement.runtimeSymbol("cosmo0_runtime::missing")),
    )
    val extern = Lir.function(
      "missing",
      List(Lir.param("value", SourceType.String)),
      SourceType.Unit,
      locals = Nil,
      blocks = Nil,
      externBinding = Some(binding),
    )
    val caller = Lir.function(
      "call_missing",
      Nil,
      SourceType.Unit,
      locals = Nil,
      blocks = List(
        Lir.block(
          "entry",
          operations = List(
            LirDirectCall(
              None,
              extern.id,
              List(Lir.string("x")),
              Lir.signature(List(SourceType.String), SourceType.Unit),
            ),
          ),
          terminator = LirReturn(None),
        ),
      ),
    )

    val result = CppBackend().emit(LirModule("missing_extern", List(extern, caller)))

    assertEquals(result.phase, Phase.Compile)
    assertEquals(result.status, PhaseStatus.Failed)
    assert(
      result.diagnostics.exists(_.code == "cosmo0.cpp.missing-extern-runtime-symbol"),
      s"missing extern runtime diagnostic in ${result.diagnostics.map(_.code)}",
    )

  test("Cosmo0 compile emits library-shaped C++ for parser.cos without a main wrapper"):
    val result = Cosmo0().compile(
      SourceFile(
        ParserFixtureManifest.parserSourcePath,
        combineParserLibrarySources(),
      ),
    )

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get.output
    assert(output.contains("struct SyntaxModule"))
    assert(output.contains("struct SyntaxParserResult"))
    assert(output.contains("struct ParserToken"))
    assert(output.contains("inline std::size_t source_len(std::string source)"))
    assert(output.contains("inline SyntaxParserResult parse_source_ast(std::string source)"))
    assert(output.contains("inline bool parse_source(std::string source)"))
    assert(output.contains("inline std::string syntax_debug_module("))
    assert(output.contains("inline bool parser_result_required_spans_ok("))
    assert(!output.contains("int main()"))
    assertCxxAccepts(output)

  test("Cosmo0 compile emits byte-for-byte stable C++ for parser.cos"):
    val source = SourceFile(ParserFixtureManifest.parserSourcePath, combineParserLibrarySources())
    val first = Cosmo0().compile(source)
    val second = Cosmo0().compile(source)

    assert(
      first.isSuccess,
      s"first parser.cos compile failed with diagnostics: ${first.diagnostics.map(d => d.code -> d.message)}",
    )
    assert(
      second.isSuccess,
      s"second parser.cos compile failed with diagnostics: ${second.diagnostics.map(d => d.code -> d.message)}",
    )
    assertEquals(first.value.get.output, second.value.get.output)
    assertCxxAccepts(first.value.get.output)

  test("Cosmo0 compile emits executable C++ for parser_test linked with parser.cos"):
    val result = compileParserTestProgram()

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get.output
    assert(output.contains("int main()"))
    assert(output.contains("cosmo0_runtime::read_file"))
    assert(output.contains("parse_source"))
    assert(output.contains("parser_debug_render_is_deterministic"))
    assertCxxAccepts(output)

  test("parser_test executable passes shared parser fixture directives"):
    val result = compileParserTestProgram()

    assert(
      result.isSuccess,
      s"compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get.output
    val compiler = cxxCompiler().getOrElse(fail("no C++ compiler found for parser_test execution test"))
    TestNodeFs.mkdirSync("target/cosmo0-cpp-tests", js.Dynamic.literal("recursive" -> true))
    val sourcePath = "target/cosmo0-cpp-tests/parser_test.cpp"
    val executablePath = "target/cosmo0-cpp-tests/parser_test"
    TestNodeFs.writeFileSync(sourcePath, output)

    val compile = NodeSpawnSync(
      compiler,
      js.Array("-std=c++17", "-O2", NlohmannJsonDependency.includeArg, sourcePath, "-o", executablePath),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      compile.status.toOption,
      Some(0),
      s"C++ compiler rejected parser_test executable output with ${compiler}\n${compile.stderr.getOrElse("")}",
    )

    val run = NodeSpawnSync(
      executablePath,
      js.Array(),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      run.status.toOption,
      Some(0),
      s"parser_test fixture run failed\nstdout:\n${run.stdout.getOrElse("")}\nstderr:\n${run.stderr.getOrElse("")}",
    )

  test("core0 json_test executable passes through nlohmann runtime bridge"):
    val lowered = Cosmo0().lower(
      SourceFile(
        "library/std/src/std/json_test.cos",
        List(
          ParserFixtureManifest.readFile("library/std/src/std/json.cos"),
          ParserFixtureManifest.readFile("library/std/src/std/json_test.cos"),
        ).mkString("\n"),
      ),
    )

    assert(
      lowered.isSuccess,
      s"json_test lower failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )
    val result = CppBackend().emit(lowered.value.get.lir)
    assert(
      result.isSuccess,
      s"json_test C++ emission failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = result.value.get.source
    assert(output.contains("nlohmann::json"))
    assertCxxAccepts(output)

    val compiler = cxxCompiler().getOrElse(fail("no C++ compiler found for json_test execution test"))
    TestNodeFs.mkdirSync("target/cosmo0-cpp-tests", js.Dynamic.literal("recursive" -> true))
    val sourcePath = "target/cosmo0-cpp-tests/json_test.cpp"
    val executablePath = "target/cosmo0-cpp-tests/json_test"
    val entry =
      s"""
         |int main() {
         |  return ${result.value.get.namespace.mkString("::")}::json_test_smoke() &&
         |    ${result.value.get.namespace.mkString("::")}::json_test_rejects_invalid() ? 0 : 1;
         |}
         |""".stripMargin
    TestNodeFs.writeFileSync(sourcePath, output + entry)

    val compile = NodeSpawnSync(
      compiler,
      js.Array("-std=c++17", "-O2", NlohmannJsonDependency.includeArg, sourcePath, "-o", executablePath),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      compile.status.toOption,
      Some(0),
      s"C++ compiler rejected json_test executable output with ${compiler}\n${compile.stderr.getOrElse("")}",
    )

    val run = NodeSpawnSync(
      executablePath,
      js.Array(),
      js.Dynamic.literal(encoding = "utf8"),
    )
    assertEquals(
      run.status.toOption,
      Some(0),
      s"json_test fixture run failed\nstdout:\n${run.stdout.getOrElse("")}\nstderr:\n${run.stderr.getOrElse("")}",
    )

  private def compileParserTestProgram(): Result[CompiledModule] =
    Cosmo0().compile(
      SourceFile(
        ParserFixtureManifest.parserTestSourcePath,
        List(
          ParserFixtureManifest.readFile(spanSourcePath),
          ParserFixtureManifest.readFile(syntaxAstSourcePath),
          ParserFixtureManifest.readFile(ParserFixtureManifest.parserSourcePath),
          ParserFixtureManifest.readFile(symbolSourcePath),
          ParserFixtureManifest.readFile(scopeSourcePath),
          ParserFixtureManifest.readFile(resolutionSourcePath),
          ParserFixtureManifest.readFile(typeModelSourcePath),
          ParserFixtureManifest.readFile(declarationResolutionSourcePath),
          ParserFixtureManifest.readFile(typeCheckSourcePath),
          ParserFixtureManifest.readFile(ParserFixtureManifest.parserTestSourcePath),
        ).mkString("\n"),
      ),
    )

  private def combineParserLibrarySources(): String =
    List(
      ParserFixtureManifest.readFile(spanSourcePath),
      ParserFixtureManifest.readFile(syntaxAstSourcePath),
      ParserFixtureManifest.readFile(ParserFixtureManifest.parserSourcePath),
    ).mkString("\n")

  private def assertCxxAccepts(source: String): Unit =
    val compiler = cxxCompiler().getOrElse(fail("no C++ compiler found for cosmo0 backend acceptance test"))
    val result = NodeSpawnSync(
      compiler,
      js.Array("-std=c++17", NlohmannJsonDependency.includeArg, "-fsyntax-only", "-x", "c++", "-"),
      js.Dynamic.literal(input = source, encoding = "utf8"),
    )
    assertEquals(
      result.status.toOption,
      Some(0),
      s"C++ compiler rejected generated output with ${compiler}\n${result.stderr.getOrElse("")}",
    )

  private def cxxCompiler(): Option[String] =
    List("c++", "g++", "clang++").find { command =>
      val result = NodeSpawnSync(
        command,
        js.Array("--version"),
        js.Dynamic.literal(encoding = "utf8"),
      )
      result.status.toOption.contains(0)
    }

  private def checkedLirModule(
      function: LirFunction = checkedProcessFunction(),
  ): LirModule =
    LirModule(
      "checked",
      List(tokenDecl(), identityFunction(), function),
    )

  private def checkedProcessFunction(
      locals: List[LirLocal] = checkedLocals(),
      blocks: List[LirBlock] = checkedBlocks(),
  ): LirFunction =
    Lir.function(
      "process",
      List(Lir.param("input", tokenType)),
      SourceType.I32,
      locals = locals,
      blocks = blocks,
    )

  private def checkedLocals(): List[LirLocal] =
    List(
      Lir.local("token", tokenType, mutable = true),
      Lir.local("labels", stringVecType, mutable = true),
      Lir.local("maybe", optionTokenType),
      Lir.local("is_some", SourceType.Bool),
      Lir.local("tag", SourceType.I32),
      Lir.local("payload", tokenType),
      Lir.local("tmp", SourceType.Usize),
      Lir.local("count", SourceType.I32, mutable = true),
    )

  private def checkedBlocks(): List[LirBlock] =
    List(
      Lir.block(
        "entry",
        operations = checkedEntryOperations(),
        terminator = LirCondBranch(
          Lir.bool(true),
          Lir.label("exit"),
          Lir.label("error"),
        ),
      ),
      Lir.block(
        "exit",
        terminator = LirReturn(Some(Lir.ref("count", SourceType.I32))),
      ),
      Lir.block(
        "error",
        terminator = LirErrorExit("cosmo0.lir.test", Some("failed")),
      ),
    )

  private def checkedEntryOperations(): List[LirOp] =
    List(
      LirAllocLocal(Lir.local("token", tokenType, mutable = true)),
      LirAllocLocal(Lir.local("labels", stringVecType, mutable = true)),
      LirAllocLocal(Lir.local("count", SourceType.I32, mutable = true), Some(Lir.int(0))),
      LirFieldGet(
        Lir.localId("tmp"),
        Lir.ref("token", tokenType),
        "offset",
        Lir.t(SourceType.Usize),
      ),
      LirFieldSet(
        Lir.ref("token", tokenType),
        "offset",
        Lir.ref("tmp", SourceType.Usize),
      ),
      LirAssign(Lir.localPlace("count", SourceType.I32), Lir.int(1)),
      LirDirectCall(
        Some(Lir.localId("count")),
        Lir.declId("identity"),
        List(Lir.ref("count", SourceType.I32)),
        Lir.signature(List(SourceType.I32), SourceType.I32),
      ),
      LirDescriptorIntrinsic(
        None,
        LirDescriptorRef("Vec", List(Lir.t(SourceType.String))),
        "push",
        List(Lir.ref("labels", stringVecType), Lir.string("ok")),
        Some(Lir.t(SourceType.Unit)),
      ),
      LirConstructVariant(
        Lir.localId("maybe"),
        Lir.t(optionTokenType),
        "Some",
        List(Lir.ref("token", tokenType)),
      ),
      LirReadVariantTag(
        Lir.localId("tag"),
        Lir.ref("maybe", optionTokenType),
        Lir.t(optionTokenType),
      ),
      LirCheckVariantTag(
        Lir.localId("is_some"),
        Lir.ref("maybe", optionTokenType),
        Lir.t(optionTokenType),
        "Some",
      ),
      LirReadVariantPayload(
        Lir.localId("payload"),
        Lir.ref("maybe", optionTokenType),
        "Some",
        0,
        Lir.t(tokenType),
      ),
    )

  private def tokenDecl(): LirTypeDecl =
    LirTypeDecl(
      Lir.declId("token"),
      "Token",
      fields = List(
        LirField("text", Lir.t(SourceType.String)),
        LirField("offset", Lir.t(SourceType.Usize), mutable = true),
      ),
      variants = List(
        LirVariant("WithText", List(LirVariantPayload(Some("value"), Lir.t(SourceType.String)))),
        LirVariant("Empty"),
      ),
    )

  private def identityFunction(): LirFunction =
    Lir.function(
      "identity",
      List(Lir.param("value", SourceType.I32)),
      SourceType.I32,
      locals = Nil,
      blocks = List(
        Lir.block(
          "entry",
          terminator = LirReturn(Some(Lir.ref("value", SourceType.I32))),
        ),
      ),
    )

@js.native
@JSImport("node:child_process", "spawnSync")
private object NodeSpawnSync extends js.Object:
  def apply(command: String, args: js.Array[String], options: js.Any): NodeSpawnSyncResult = js.native

@js.native
private trait NodeSpawnSyncResult extends js.Object:
  val status: js.UndefOr[Int] = js.native
  val stdout: js.UndefOr[String] = js.native
  val stderr: js.UndefOr[String] = js.native

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object TestNodeFs extends js.Object:
  def mkdirSync(path: String, options: js.Any): Unit = js.native
  def writeFileSync(path: String, data: String): Unit = js.native
