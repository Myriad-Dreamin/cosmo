package cosmo0

class CosmoEvalTests extends munit.FunSuite:
  test("cosmo0 eval request serialization and context key are deterministic"):
    val first = integerRequest("cosmo0")
    val second = integerRequest("cosmo0")

    assertEquals(first.stableJson, second.stableJson)
    assertEquals(
      first.precompiledContextKey.deterministicId,
      second.precompiledContextKey.deterministicId,
    )
    assert(first.stableJson.contains("\"compilerId\":\"cosmo0\""))
    assert(first.stableJson.contains("\"headers\":[\"<iostream>\"]"))
    assert(!first.stableJson.contains("clang::"))
    assert(!first.stableJson.contains("llvm::"))

  test("cosmo0 eval session reports same-key context reuse"):
    val session = CosmoEvalSession.cosmo0()
    val request = integerRequest("cosmo0")

    val first = session.complete(request, "2")
    val second = session.complete(request, "2")

    assertEquals(first.status, CosmoEvalStatus.Succeeded)
    assertEquals(first.cacheSummary.state, CosmoEvalCacheState.Created)
    assertEquals(second.cacheSummary.state, CosmoEvalCacheState.Reused)
    assertEquals(second.serializedOutput, Some("2"))

  test("cosmo0 eval rejects clang interpreter backends"):
    val session = CosmoEvalSession.cosmo0()
    val request = integerRequest("cosmo0", backend = "clangInterpreter")
    val result = session.complete(request, "2")

    assertEquals(result.status, CosmoEvalStatus.Unsupported)
    assertEquals(
      result.diagnostics.map(_.code),
      List("cosmo.eval.unsupported-interpreter-backend"),
    )
    assert(result.diagnostics.head.message.contains("ordinary Clang"))

  test("cosmoc eval source compiles through cosmo0 stage profile"):
    val source = combineSources(
      List("packages/cosmoc/src/eval/eval.cos"),
      "",
    )
    val compiled = Cosmo0().compile(SourceFile("eval.cos", source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmoc eval source failed with diagnostics: ${compiled.diagnostics
          .map(d => d.code -> d.message)}",
    )
    assert(compiled.value.get.output.contains("struct CosmoEvalRequest"))
    assert(compiled.value.get.output.contains("struct CosmoEvalResult"))
    assert(
      compiled.value.get.output.contains(
        "inline bool cosmo_eval_rejects_interpreter_backend()",
      ),
    )

  private def integerRequest(
      compilerId: String,
      backend: String = CosmoEval.CompiledProviderBackend,
  ): CosmoEvalRequest =
    CosmoEval.request(
      compilerId = compilerId,
      evalIdentity = "smoke.integer",
      providerIdentity = "test.provider",
      serializedInput = "1+1",
      headers = List("<iostream>"),
      generatedEntrySource =
        """extern "C" int cosmo_eval_entry() { return 1 + 1; }""",
      toolchainIdentity = toolchainIdentity,
      requestedFacts = List("return:i32"),
      backend = backend,
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

  private def combineSources(paths: List[String], extra: String): String =
    (paths.map(ParserFixtureManifest.readFile) :+ extra).mkString("\n")
end CosmoEvalTests
