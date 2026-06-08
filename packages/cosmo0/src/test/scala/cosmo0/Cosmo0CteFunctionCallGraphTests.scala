package cosmo0

class Cosmo0CteFunctionCallGraphTests extends munit.FunSuite:
  test("validation mode is disabled by default and by explicit option"):
    val source =
      """def caller(n: i32): i32 = helper(n)
        |def helper(n: i32): i32 = n + 1
        |""".stripMargin

    val ordinary = Cosmo0().check(SourceFile("cte-fns.cos", source))
    assertEquals(ordinary.status, PhaseStatus.Succeeded)

    val runner = RecordingRunner(List("2"))
    val disabled = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("cte-fns.cos", source),
      graphOptions(enabled = false),
      runner,
    )

    assertEquals(disabled.status, PhaseStatus.Succeeded)
    assert(disabled.value.get.plan.callables.isEmpty)
    assert(runner.requests.isEmpty)

  test("dependent helper calls are source-order independent"):
    val callerFirst =
      """def caller(n: i32): i32 = helper(n) + helper(n)
        |def helper(n: i32): i32 = n + 1
        |""".stripMargin
    val helperFirst =
      """def helper(n: i32): i32 = n + 1
        |def caller(n: i32): i32 = helper(n) + helper(n)
        |""".stripMargin

    val first = runGraph(callerFirst, "4")
    val second = runGraph(helperFirst, "4")

    assertEquals(first.result.status, PhaseStatus.Succeeded)
    assertEquals(second.result.status, PhaseStatus.Succeeded)
    assertEquals(first.module.resultValue("caller"), Some(BigInt(4)))
    assertEquals(second.module.resultValue("caller"), Some(BigInt(4)))
    assertEquals(
      first.module.plan.stableSummary,
      second.module.plan.stableSummary,
    )
    assertEquals(first.module.plan.callableCheckCount("helper"), 1)
    assertEquals(
      first.module.plan.artifacts.map(_.id),
      List("cte.artifact.helper", "cte.artifact.caller"),
    )
    assert(first.request.generatedEntrySource.contains("static int helper"))
    assert(first.request.generatedEntrySource.contains("static int caller"))
    assert(
      first.request.serializedInput.contains("\"entryIdentity\":\"caller\""),
    )
    assert(
      first.request.serializedInput.contains("\"argumentPayload\":[\"1\"]"),
    )
    assert(!first.request.stableJson.contains("clangInterpreter"))
    assert(!first.request.stableJson.contains("clang::"))
    assert(!first.request.stableJson.contains("llvm::"))

  test("direct recursion is classified as one callable SCC artifact"):
    val source =
      """def sum(n: i32): i32 = if (n == 0) { 0 } else { n + sum(n - 1) }
        |""".stripMargin
    val runner = RecordingRunner(List("6"))
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("recursive.cos", source),
      graphOptions(
        entries = List(Cosmo0CteFunctionCallGraphEntry("sum", List(3))),
      ),
      runner,
    )

    assertEquals(result.status, PhaseStatus.Succeeded)
    val module = result.value.get
    assertEquals(module.resultValue("sum"), Some(BigInt(6)))
    assertEquals(module.plan.artifacts.map(_.id), List("cte.artifact.scc.sum"))
    assert(module.plan.artifacts.head.recursive)
    assert(runner.requests.head.generatedEntrySource.contains("return sum(3);"))

  test("mutual recursion is classified as one callable SCC artifact"):
    val source =
      """def even(n: i32): i32 = if (n == 0) { 1 } else { odd(n - 1) }
        |def odd(n: i32): i32 = if (n == 0) { 0 } else { even(n - 1) }
        |""".stripMargin
    val runner = RecordingRunner(List("1"))
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("mutual.cos", source),
      graphOptions(
        entries = List(Cosmo0CteFunctionCallGraphEntry("even", List(4))),
      ),
      runner,
    )

    assertEquals(result.status, PhaseStatus.Succeeded)
    val module = result.value.get
    assertEquals(module.resultValue("even"), Some(BigInt(1)))
    assertEquals(
      module.plan.artifacts.map(_.id),
      List("cte.artifact.scc.even+odd"),
    )
    assertEquals(module.plan.artifacts.head.callableNames, List("even", "odd"))
    assert(module.plan.artifacts.head.recursive)

  test("diagnostic ordering is deterministic across repeated graph failures"):
    val source =
      """def caller(n: i32): i32 = missing(n) + other(n)
        |""".stripMargin
    val first = unresolvedResult(source)
    val second = unresolvedResult(source)
    val firstDiagnostics =
      first.diagnostics.map(diagnostic => diagnostic.code -> diagnostic.message)
    val secondDiagnostics =
      second.diagnostics.map(diagnostic =>
        diagnostic.code -> diagnostic.message,
      )

    assertEquals(first.status, PhaseStatus.Failed)
    assertEquals(firstDiagnostics, secondDiagnostics)
    assertEquals(
      first.diagnostics.map(_.code),
      List(
        Cosmo0CteFunctionCallGraph.UnresolvedCallCode,
        Cosmo0CteFunctionCallGraph.UnresolvedCallCode,
      ),
    )

  test("unsupported suffix call shapes are rejected before execution"):
    val source =
      """def caller(n: i32): i32 = helper.value(n)
        |def helper(n: i32): i32 = n
        |""".stripMargin
    val runner = RecordingRunner(List("1"))
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("unsupported-call.cos", source),
      graphOptions(),
      runner,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteFunctionCallGraph.UnsupportedCallShapeCode),
    )
    assert(runner.requests.isEmpty)

  test("unavailable eval support reports a stable diagnostic"):
    val source = "def caller(n: i32): i32 = n"
    val runner = RecordingRunner(List("1"))
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("unavailable.cos", source),
      Cosmo0CteFunctionCallGraphOptions(
        enabled = true,
        entries = List(Cosmo0CteFunctionCallGraphEntry("caller", List(1))),
      ),
      runner,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteFunctionCallGraph.UnavailableCode),
    )
    assert(runner.requests.isEmpty)

  test("provider entry compile failures are surfaced deterministically"):
    val runner = RecordingRunner(
      Nil,
      Some(
        failedEvalResult(
          "cosmo.eval.provider-entry.compile-failed",
          "structured clang:: diagnostic summary",
        ),
      ),
    )
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("compile-failed.cos", "def caller(n: i32): i32 = n"),
      graphOptions(),
      runner,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteFunctionCallGraph.CompileFailedCode),
    )
    assert(
      result.diagnostics.head.message.contains("structured clang diagnostic"),
    )
    assert(!result.diagnostics.head.message.contains("clang::"))

  test("provider entry execution failures are surfaced deterministically"):
    val runner = RecordingRunner(
      Nil,
      Some(
        failedEvalResult(
          "cosmo.eval.provider-entry.execution-failed",
          "entry point failed",
        ),
      ),
    )
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("execution-failed.cos", "def caller(n: i32): i32 = n"),
      graphOptions(),
      runner,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteFunctionCallGraph.ExecutionFailedCode),
    )
    assert(result.diagnostics.head.message.contains("entry point failed"))

  test("recursive execution bounds are enforced before host execution"):
    val source =
      """def sum(n: i32): i32 = if (n == 0) { 0 } else { n + sum(n - 1) }
        |""".stripMargin
    val runner = RecordingRunner(List("6"))
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("bound.cos", source),
      graphOptions(
        entries = List(Cosmo0CteFunctionCallGraphEntry("sum", List(3))),
        recursionStepLimit = 0,
      ),
      runner,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteFunctionCallGraph.RecursionBoundCode),
    )
    assert(runner.requests.isEmpty)

  private final case class GraphRun(
      result: Result[Cosmo0CteFunctionCallGraphModule],
      runner: RecordingRunner,
  ):
    def module: Cosmo0CteFunctionCallGraphModule = result.value.get
    def request: CosmoEvalRequest = runner.requests.head

  private final class RecordingRunner(
      outputs: List[String],
      failure: Option[CosmoEvalResult] = None,
  ) extends Cosmo0CteFunctionCallGraphRunner:
    private val recorded =
      scala.collection.mutable.ListBuffer.empty[CosmoEvalRequest]
    private var remainingOutputs = outputs

    def requests: List[CosmoEvalRequest] =
      recorded.toList

    def evaluate(
        request: CosmoEvalRequest,
    ): Cosmo0CteCompileProbeEvalOutcome =
      recorded += request
      failure match
        case Some(result) =>
          Cosmo0CteCompileProbeEvalOutcome.Completed(result)
        case None =>
          val output = remainingOutputs.headOption.getOrElse("0")
          remainingOutputs = remainingOutputs.drop(1)
          Cosmo0CteCompileProbeEvalOutcome.Completed(
            CosmoEvalSession.cosmo0().complete(request, output),
          )

  private def runGraph(source: String, output: String): GraphRun =
    val runner = RecordingRunner(List(output))
    val result = Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("graph.cos", source),
      graphOptions(),
      runner,
    )
    GraphRun(result, runner)

  private def unresolvedResult(
      source: String,
  ): Result[Cosmo0CteFunctionCallGraphModule] =
    Cosmo0().checkCompileTimeFunctionCallGraph(
      SourceFile("unresolved.cos", source),
      graphOptions(),
      RecordingRunner(List("0")),
    )

  private def graphOptions(
      enabled: Boolean = true,
      entries: List[Cosmo0CteFunctionCallGraphEntry] = List(
        Cosmo0CteFunctionCallGraphEntry("caller", List(1)),
      ),
      recursionStepLimit: Int = 64,
  ): Cosmo0CteFunctionCallGraphOptions =
    Cosmo0CteFunctionCallGraphOptions(
      enabled = enabled,
      entries = entries,
      toolchainIdentity = Some(toolchainIdentity),
      recursionStepLimit = recursionStepLimit,
    )

  private def failedEvalResult(
      code: String,
      message: String,
  ): CosmoEvalResult =
    CosmoEvalResult(
      CosmoEvalStatus.Failed,
      List(
        CosmoEvalDiagnostic(
          DiagnosticSeverity.Error,
          code,
          message,
        ),
      ),
      None,
      Nil,
      Nil,
      Nil,
      CosmoEvalCacheSummary(
        "cte-function-graph",
        CosmoEvalCacheState.Unknown,
        "",
        "failed",
      ),
      CosmoEvalCapturedOutputSummary(0, 0, "", ""),
    )

  private def toolchainIdentity: CosmoEvalToolchainIdentity =
    CosmoEvalToolchainIdentity(
      llvmVersion = "21.1.8",
      clangExecutable = "/opt/llvm/bin/clang++",
      clangVersion = "clang version 21.1.8",
      manifestPath = "packages/cosmo-clang-sys/config/llvm-manifest.json",
      cachePath = "target/cosmo/llvm",
      offlineMode = false,
      targetPlatform = "linux",
      targetArchitecture = "x86_64",
      gnuToolchain = Some("/usr/local/gcc-14.3.0"),
    )
end Cosmo0CteFunctionCallGraphTests
