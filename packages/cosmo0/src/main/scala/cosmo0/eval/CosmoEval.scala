package cosmo0

enum CosmoEvalStatus:
  case Succeeded, Failed, Unsupported

  def id: String =
    this match
      case Succeeded   => "succeeded"
      case Failed      => "failed"
      case Unsupported => "unsupported"

enum CosmoEvalCacheState:
  case Created, Reused, Invalidated, Disabled, Unknown

  def id: String =
    this match
      case Created     => "created"
      case Reused      => "reused"
      case Invalidated => "invalidated"
      case Disabled    => "disabled"
      case Unknown     => "unknown"

final case class CosmoEvalDiagnostic(
    severity: DiagnosticSeverity,
    code: String,
    message: String,
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "severity" -> CosmoEvalStableJson.string(severity.toString),
        "code" -> CosmoEvalStableJson.string(code),
        "message" -> CosmoEvalStableJson.string(message),
      ),
    )

final case class CosmoEvalToolchainIdentity(
    llvmVersion: String,
    clangExecutable: String,
    clangVersion: String,
    manifestPath: String,
    cachePath: String,
    offlineMode: Boolean,
    targetPlatform: String,
    targetArchitecture: String,
    gnuToolchain: Option[String],
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "llvmVersion" -> CosmoEvalStableJson.string(llvmVersion),
        "clangExecutable" -> CosmoEvalStableJson.string(clangExecutable),
        "clangVersion" -> CosmoEvalStableJson.string(clangVersion),
        "manifestPath" -> CosmoEvalStableJson.string(manifestPath),
        "cachePath" -> CosmoEvalStableJson.string(cachePath),
        "offlineMode" -> CosmoEvalStableJson.bool(offlineMode),
        "targetPlatform" -> CosmoEvalStableJson.string(targetPlatform),
        "targetArchitecture" -> CosmoEvalStableJson.string(
          targetArchitecture,
        ),
        "gnuToolchain" -> gnuToolchain
          .map(CosmoEvalStableJson.string)
          .getOrElse("null"),
      ),
    )

final case class CosmoEvalTargetSettings(
    cxxStandard: String,
    targetTriple: String,
    buildProfile: String,
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "cxxStandard" -> CosmoEvalStableJson.string(cxxStandard),
        "targetTriple" -> CosmoEvalStableJson.string(targetTriple),
        "buildProfile" -> CosmoEvalStableJson.string(buildProfile),
      ),
    )

final case class CosmoEvalResourceLimits(
    timeoutMillis: Int,
    maxOutputBytes: Int,
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "timeoutMillis" -> CosmoEvalStableJson.number(timeoutMillis),
        "maxOutputBytes" -> CosmoEvalStableJson.number(maxOutputBytes),
      ),
    )

final case class CosmoEvalPrecompiledContextKey(
    cxxStandard: String,
    targetTriple: String,
    toolchainIdentity: CosmoEvalToolchainIdentity,
    includePaths: List[String],
    headers: List[String],
    imports: List[String],
    compileOptions: List[String],
    supportLibraryIdentities: List[String],
    evalSessionProfile: String,
):
  lazy val stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "cxxStandard" -> CosmoEvalStableJson.string(cxxStandard),
        "targetTriple" -> CosmoEvalStableJson.string(targetTriple),
        "toolchainIdentity" -> toolchainIdentity.stableJson,
        "includePaths" -> CosmoEvalStableJson.stringArray(includePaths),
        "headers" -> CosmoEvalStableJson.stringArray(headers),
        "imports" -> CosmoEvalStableJson.stringArray(imports),
        "compileOptions" -> CosmoEvalStableJson.stringArray(compileOptions),
        "supportLibraryIdentities" -> CosmoEvalStableJson.stringArray(
          supportLibraryIdentities,
        ),
        "evalSessionProfile" -> CosmoEvalStableJson.string(
          evalSessionProfile,
        ),
      ),
    )

  lazy val deterministicId: String =
    CosmoEvalStableJson.hashHex(stableJson)

final case class CosmoEvalRequest(
    compilerId: String,
    evalIdentity: String,
    providerIdentity: String,
    serializedInput: String,
    imports: List[String],
    headers: List[String],
    includePaths: List[String],
    librarySearchPaths: List[String],
    compileOptions: List[String],
    generatedEntrySource: String,
    target: CosmoEvalTargetSettings,
    resourceLimits: CosmoEvalResourceLimits,
    precompiledContextKey: CosmoEvalPrecompiledContextKey,
    toolchainIdentity: CosmoEvalToolchainIdentity,
    backend: String,
    requestedFacts: List[String],
    supportLibraryIdentities: List[String],
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "compilerId" -> CosmoEvalStableJson.string(compilerId),
        "evalIdentity" -> CosmoEvalStableJson.string(evalIdentity),
        "providerIdentity" -> CosmoEvalStableJson.string(providerIdentity),
        "serializedInput" -> CosmoEvalStableJson.string(serializedInput),
        "imports" -> CosmoEvalStableJson.stringArray(imports),
        "headers" -> CosmoEvalStableJson.stringArray(headers),
        "includePaths" -> CosmoEvalStableJson.stringArray(includePaths),
        "librarySearchPaths" -> CosmoEvalStableJson.stringArray(
          librarySearchPaths,
        ),
        "compileOptions" -> CosmoEvalStableJson.stringArray(compileOptions),
        "generatedEntrySource" -> CosmoEvalStableJson.string(
          generatedEntrySource,
        ),
        "target" -> target.stableJson,
        "resourceLimits" -> resourceLimits.stableJson,
        "precompiledContextKey" -> precompiledContextKey.stableJson,
        "toolchainIdentity" -> toolchainIdentity.stableJson,
        "backend" -> CosmoEvalStableJson.string(backend),
        "requestedFacts" -> CosmoEvalStableJson.stringArray(requestedFacts),
        "supportLibraryIdentities" -> CosmoEvalStableJson.stringArray(
          supportLibraryIdentities,
        ),
      ),
    )

final case class CosmoEvalArtifactSummary(
    kind: String,
    path: String,
    sha256: String,
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "kind" -> CosmoEvalStableJson.string(kind),
        "path" -> CosmoEvalStableJson.string(path),
        "sha256" -> CosmoEvalStableJson.string(sha256),
      ),
    )

final case class CosmoEvalCacheSummary(
    keyId: String,
    state: CosmoEvalCacheState,
    artifactPath: String,
    message: String,
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "keyId" -> CosmoEvalStableJson.string(keyId),
        "state" -> CosmoEvalStableJson.string(state.id),
        "artifactPath" -> CosmoEvalStableJson.string(artifactPath),
        "message" -> CosmoEvalStableJson.string(message),
      ),
    )

final case class CosmoEvalCapturedOutputSummary(
    stdoutBytes: Int,
    stderrBytes: Int,
    stdoutPreview: String,
    stderrPreview: String,
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "stdoutBytes" -> CosmoEvalStableJson.number(stdoutBytes),
        "stderrBytes" -> CosmoEvalStableJson.number(stderrBytes),
        "stdoutPreview" -> CosmoEvalStableJson.string(stdoutPreview),
        "stderrPreview" -> CosmoEvalStableJson.string(stderrPreview),
      ),
    )

final case class CosmoEvalResult(
    status: CosmoEvalStatus,
    diagnostics: List[CosmoEvalDiagnostic],
    serializedOutput: Option[String],
    requestedCppFacts: List[String],
    supportBindingMetadata: List[String],
    artifacts: List[CosmoEvalArtifactSummary],
    cacheSummary: CosmoEvalCacheSummary,
    capturedOutput: CosmoEvalCapturedOutputSummary,
):
  def stableJson: String =
    CosmoEvalStableJson.obj(
      List(
        "status" -> CosmoEvalStableJson.string(status.id),
        "diagnostics" -> CosmoEvalStableJson.array(
          diagnostics.map(_.stableJson),
        ),
        "serializedOutput" -> serializedOutput
          .map(CosmoEvalStableJson.string)
          .getOrElse("null"),
        "requestedCppFacts" -> CosmoEvalStableJson.stringArray(
          requestedCppFacts,
        ),
        "supportBindingMetadata" -> CosmoEvalStableJson.stringArray(
          supportBindingMetadata,
        ),
        "artifacts" -> CosmoEvalStableJson.array(artifacts.map(_.stableJson)),
        "cacheSummary" -> cacheSummary.stableJson,
        "capturedOutput" -> capturedOutput.stableJson,
      ),
    )

final class CosmoEvalSession private (
    val compilerId: String,
    private var cachedContextIds: Set[String],
):
  def prepare(request: CosmoEvalRequest): Either[
    CosmoEvalDiagnostic,
    CosmoEvalCacheSummary,
  ] =
    CosmoEval.unsupportedBackendDiagnostic(request.backend) match
      case Some(diagnostic) => Left(diagnostic)
      case None             => Right(prepareContext(request))

  def complete(
      request: CosmoEvalRequest,
      serializedOutput: String,
      artifacts: List[CosmoEvalArtifactSummary] = Nil,
  ): CosmoEvalResult =
    prepare(request) match
      case Left(diagnostic) =>
        CosmoEvalResult(
          CosmoEvalStatus.Unsupported,
          List(diagnostic),
          None,
          Nil,
          Nil,
          Nil,
          CosmoEvalCacheSummary(
            request.precompiledContextKey.deterministicId,
            CosmoEvalCacheState.Disabled,
            "",
            "eval backend is not supported",
          ),
          CosmoEvalCapturedOutputSummary(0, 0, "", ""),
        )
      case Right(cacheSummary) =>
        CosmoEvalResult(
          CosmoEvalStatus.Succeeded,
          Nil,
          Some(serializedOutput),
          request.requestedFacts,
          request.supportLibraryIdentities,
          artifacts,
          cacheSummary,
          CosmoEvalCapturedOutputSummary(serializedOutput.length, 0, "", ""),
        )

  private def prepareContext(
      request: CosmoEvalRequest,
  ): CosmoEvalCacheSummary =
    val keyId = request.precompiledContextKey.deterministicId
    val state =
      if cachedContextIds.contains(keyId) then CosmoEvalCacheState.Reused
      else
        cachedContextIds = cachedContextIds + keyId
        CosmoEvalCacheState.Created

    CosmoEvalCacheSummary(
      keyId,
      state,
      s"target/cosmo/eval/cache/$keyId/context.hpp.pch",
      s"precompiled context ${state.id}",
    )

object CosmoEvalSession:
  def cosmo0(): CosmoEvalSession =
    CosmoEvalSession("cosmo0")

  def cosmoc(): CosmoEvalSession =
    CosmoEvalSession("cosmoc")

  def apply(compilerId: String): CosmoEvalSession =
    new CosmoEvalSession(compilerId, Set.empty)

object CosmoEval:
  val CompiledProviderBackend: String = "clang-pch-executable"

  private val unsupportedInterpreterBackends =
    Set("clang-repl", "clangInterpreter", "clang-interpreter")

  def unsupportedBackendDiagnostic(
      backend: String,
  ): Option[CosmoEvalDiagnostic] =
    if unsupportedInterpreterBackends.contains(backend) then
      Some(
        CosmoEvalDiagnostic(
          DiagnosticSeverity.Error,
          "cosmo.eval.unsupported-interpreter-backend",
          "clang-repl and clangInterpreter are not supported eval backends; provider entry functions must compile through ordinary Clang",
        ),
      )
    else None

  def request(
      compilerId: String,
      evalIdentity: String,
      providerIdentity: String,
      serializedInput: String,
      headers: List[String],
      generatedEntrySource: String,
      toolchainIdentity: CosmoEvalToolchainIdentity,
      target: CosmoEvalTargetSettings = defaultTarget,
      imports: List[String] = Nil,
      includePaths: List[String] = Nil,
      librarySearchPaths: List[String] = Nil,
      compileOptions: List[String] = Nil,
      supportLibraryIdentities: List[String] = Nil,
      requestedFacts: List[String] = Nil,
      backend: String = CompiledProviderBackend,
  ): CosmoEvalRequest =
    val key = CosmoEvalPrecompiledContextKey(
      target.cxxStandard,
      target.targetTriple,
      toolchainIdentity,
      includePaths,
      headers,
      imports,
      compileOptions,
      supportLibraryIdentities,
      target.buildProfile,
    )

    CosmoEvalRequest(
      compilerId,
      evalIdentity,
      providerIdentity,
      serializedInput,
      imports,
      headers,
      includePaths,
      librarySearchPaths,
      compileOptions,
      generatedEntrySource,
      target,
      defaultResourceLimits,
      key,
      toolchainIdentity,
      backend,
      requestedFacts,
      supportLibraryIdentities,
    )

  def defaultTarget: CosmoEvalTargetSettings =
    CosmoEvalTargetSettings("c++17", "", "dev")

  def defaultResourceLimits: CosmoEvalResourceLimits =
    CosmoEvalResourceLimits(timeoutMillis = 10000, maxOutputBytes = 65536)

object CosmoEvalStableJson:
  def obj(fields: List[(String, String)]): String =
    fields
      .map { case (key, value) => s"${string(key)}:$value" }
      .mkString("{", ",", "}")

  def array(values: List[String]): String =
    values.mkString("[", ",", "]")

  def stringArray(values: List[String]): String =
    array(values.map(string))

  def string(value: String): String =
    val builder = StringBuilder()
    builder.append('"')
    value.foreach {
      case '"'  => builder.append("\\\"")
      case '\\' => builder.append("\\\\")
      case '\b' => builder.append("\\b")
      case '\f' => builder.append("\\f")
      case '\n' => builder.append("\\n")
      case '\r' => builder.append("\\r")
      case '\t' => builder.append("\\t")
      case char if char < ' ' =>
        builder.append(f"\\u${char.toInt}%04x")
      case char => builder.append(char)
    }
    builder.append('"')
    builder.toString

  def bool(value: Boolean): String =
    if value then "true" else "false"

  def number(value: Int): String =
    value.toString

  def hashHex(value: String): String =
    val hash = value.hashCode
    val unsigned = if hash == Int.MinValue then 0 else hash.abs
    f"$unsigned%08x"
