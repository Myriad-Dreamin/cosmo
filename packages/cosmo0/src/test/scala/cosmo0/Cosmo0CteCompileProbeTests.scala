package cosmo0

class Cosmo0CteCompileProbeTests extends munit.FunSuite:
  test("normal cosmo0 check keeps compile-time eval probe disabled"):
    val result = Cosmo0().check("type x = 1 + 1")

    assertEquals(result.phase, Phase.Check)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assertEquals(
      result.diagnostics.map(_.code),
      List("cosmo0.elaborate.unsupported.path"),
    )

  test("disabled probe option preserves ordinary unsupported behavior"):
    var invoked = false
    val runner = new Cosmo0CteCompileProbeRunner:
      def evaluate(
          request: CosmoEvalRequest,
      ): Cosmo0CteCompileProbeEvalOutcome =
        invoked = true
        Cosmo0CteCompileProbeEvalOutcome.Completed(
          CosmoEvalSession.cosmo0().complete(request, "2"),
        )

    val result = Cosmo0().checkCompileTimeEvalProbe(
      SourceFile("cte.cos", "type x = 1 + 1"),
      Cosmo0CteCompileProbeOptions(enabled = false),
      runner,
    )

    assert(!invoked)
    assertEquals(result.status, PhaseStatus.Unsupported)
    assertEquals(
      result.diagnostics.map(_.code),
      List("cosmo0.elaborate.unsupported.path"),
    )

  test("enabled probe routes type x equals one plus one through cosmo0 eval"):
    var captured: Option[CosmoEvalRequest] = None
    val runner = new Cosmo0CteCompileProbeRunner:
      def evaluate(
          request: CosmoEvalRequest,
      ): Cosmo0CteCompileProbeEvalOutcome =
        captured = Some(request)
        Cosmo0CteCompileProbeEvalOutcome.Completed(
          CosmoEvalSession.cosmo0().complete(request, "2"),
        )

    val result = Cosmo0().checkCompileTimeEvalProbe(
      SourceFile("cte.cos", "type x = 1 + 1"),
      enabledOptions,
      runner,
    )

    assertEquals(result.status, PhaseStatus.Succeeded)
    assert(result.diagnostics.isEmpty)
    assertEquals(result.value.get.aliasValue("x"), Some(BigInt(2)))

    val request = captured.get
    assertEquals(request.compilerId, "cosmo0")
    assertEquals(
      request.providerIdentity,
      "cosmo0.cte-compile-probe.integer-addition",
    )
    assertEquals(request.backend, CosmoEval.CompiledProviderBackend)
    assert(request.serializedInput.contains("\"sourceIdentity\":\"cte.cos\""))
    assert(request.serializedInput.contains("\"declarationIdentity\":\"x\""))
    assert(request.serializedInput.contains("\"expressionPayload\":\"1 + 1\""))
    assert(request.generatedEntrySource.contains("return 1 + 1"))
    assert(!request.stableJson.contains("clangInterpreter"))
    assert(!request.stableJson.contains("clang::"))
    assert(!request.stableJson.contains("llvm::"))

  test("enabled probe rejects expressions outside the temporary smoke shape"):
    var invoked = false
    val runner = new Cosmo0CteCompileProbeRunner:
      def evaluate(
          request: CosmoEvalRequest,
      ): Cosmo0CteCompileProbeEvalOutcome =
        invoked = true
        Cosmo0CteCompileProbeEvalOutcome.Completed(
          CosmoEvalSession.cosmo0().complete(request, "2"),
        )

    val result = Cosmo0().checkCompileTimeEvalProbe(
      SourceFile("cte.cos", "type x = 1 + 2"),
      enabledOptions,
      runner,
    )

    assert(!invoked)
    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteCompileProbe.UnsupportedExpressionCode),
    )

  test("enabled probe reports unavailable eval configuration"):
    var invoked = false
    val runner = new Cosmo0CteCompileProbeRunner:
      def evaluate(
          request: CosmoEvalRequest,
      ): Cosmo0CteCompileProbeEvalOutcome =
        invoked = true
        Cosmo0CteCompileProbeEvalOutcome.Unavailable("eval is not configured")

    val result = Cosmo0().checkCompileTimeEvalProbe(
      SourceFile("cte.cos", "type x = 1 + 1"),
      Cosmo0CteCompileProbeOptions(enabled = true),
      runner,
    )

    assert(!invoked)
    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteCompileProbe.UnavailableCode),
    )
    assertEquals(result.diagnostics.head.span.map(_.fileName), Some("cte.cos"))

  test("enabled probe reports provider entry compile failures"):
    val runner = completedRunner(
      failedEvalResult(
        "cosmo.eval.provider-entry.compile-failed",
        "structured clang diagnostic summary",
      ),
    )

    val result = Cosmo0().checkCompileTimeEvalProbe(
      SourceFile("cte.cos", "type x = 1 + 1"),
      enabledOptions,
      runner,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteCompileProbe.CompileFailedCode),
    )
    assert(
      result.diagnostics.head.message.contains("structured clang diagnostic"),
    )
    assert(!result.diagnostics.head.message.contains("clang::"))
    assert(!result.diagnostics.head.message.contains("llvm::"))

  test("enabled probe reports provider entry execution failures"):
    val runner = completedRunner(
      failedEvalResult(
        "cosmo.eval.provider-entry.execution-failed",
        "entry point returned a failing execution result",
      ),
    )

    val result = Cosmo0().checkCompileTimeEvalProbe(
      SourceFile("cte.cos", "type x = 1 + 1"),
      enabledOptions,
      runner,
    )

    assertEquals(result.status, PhaseStatus.Failed)
    assertEquals(
      result.diagnostics.map(_.code),
      List(Cosmo0CteCompileProbe.ExecutionFailedCode),
    )
    assert(result.diagnostics.head.message.contains("failing execution result"))

  private def enabledOptions: Cosmo0CteCompileProbeOptions =
    Cosmo0CteCompileProbeOptions(
      enabled = true,
      toolchainIdentity = Some(toolchainIdentity),
    )

  private def completedRunner(
      result: CosmoEvalResult,
  ): Cosmo0CteCompileProbeRunner =
    new Cosmo0CteCompileProbeRunner:
      def evaluate(
          request: CosmoEvalRequest,
      ): Cosmo0CteCompileProbeEvalOutcome =
        Cosmo0CteCompileProbeEvalOutcome.Completed(result)

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
        "cte-probe",
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
end Cosmo0CteCompileProbeTests
