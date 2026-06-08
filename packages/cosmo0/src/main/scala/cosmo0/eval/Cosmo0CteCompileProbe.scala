package cosmo0

import scala.collection.mutable.ListBuffer

import cosmo.syntax.*

final case class Cosmo0CteCompileProbeOptions(
    enabled: Boolean,
    toolchainIdentity: Option[CosmoEvalToolchainIdentity] = None,
    target: CosmoEvalTargetSettings = CosmoEval.defaultTarget,
    imports: List[String] = Nil,
    headers: List[String] = Nil,
    includePaths: List[String] = Nil,
    librarySearchPaths: List[String] = Nil,
    compileOptions: List[String] = Nil,
    supportLibraryIdentities: List[String] = Nil,
    backend: String = CosmoEval.CompiledProviderBackend,
)

enum Cosmo0CteCompileProbeEvalOutcome:
  case Unavailable(message: String)
  case Completed(result: CosmoEvalResult)

trait Cosmo0CteCompileProbeRunner:
  def evaluate(
      request: CosmoEvalRequest,
  ): Cosmo0CteCompileProbeEvalOutcome

object Cosmo0CteCompileProbeRunner:
  def session(session: CosmoEvalSession): Cosmo0CteCompileProbeRunner =
    new Cosmo0CteCompileProbeRunner:
      def evaluate(
          request: CosmoEvalRequest,
      ): Cosmo0CteCompileProbeEvalOutcome =
        Cosmo0CteCompileProbeEvalOutcome.Completed(
          session.complete(request, "2"),
        )

final case class Cosmo0CteCompileProbeAliasResult(
    name: String,
    value: BigInt,
    span: SourceSpan,
    request: CosmoEvalRequest,
    evalResult: CosmoEvalResult,
)

final case class Cosmo0CteCompileProbeModule(
    source: SourceFile,
    aliases: List[Cosmo0CteCompileProbeAliasResult],
):
  def aliasValue(name: String): Option[BigInt] =
    aliases.find(_.name == name).map(_.value)

object Cosmo0CteCompileProbe:
  val UnavailableCode = "cosmo0.cte-compile-probe.unavailable"
  val CompileFailedCode = "cosmo0.cte-compile-probe.compile-failed"
  val ExecutionFailedCode = "cosmo0.cte-compile-probe.execution-failed"
  val UnsupportedExpressionCode =
    "cosmo0.cte-compile-probe.unsupported-expression"

  private val SmokeDeclarationName = "x"
  private val SmokeExpressionPayload = "1 + 1"
  private val SmokeProviderIdentity =
    "cosmo0.cte-compile-probe.integer-addition"
  private val SmokeProviderEntrySource =
    """extern "C" int cosmo_eval_entry() { return 1 + 1; }"""

  def check(
      sourceText: String,
      options: Cosmo0CteCompileProbeOptions,
      runner: Cosmo0CteCompileProbeRunner,
  ): Result[Cosmo0CteCompileProbeModule] =
    check(SourceFile("<memory>", sourceText), options, runner)

  def check(
      source: SourceFile,
      options: Cosmo0CteCompileProbeOptions,
      runner: Cosmo0CteCompileProbeRunner,
  ): Result[Cosmo0CteCompileProbeModule] =
    if !options.enabled then return disabledResult(source)

    options.toolchainIdentity match
      case None =>
        Result.failure(
          Phase.Check,
          List(
            diagnostic(
              UnavailableCode,
              "cosmo0 compile-time evaluation probe is enabled, but eval mode has no configured toolchain identity",
              source.span(0, source.text.length),
            ),
          ),
        )
      case Some(toolchainIdentity) =>
        parse(source) match
          case parsed if parsed.isSuccess =>
            checkParsed(
              parsed.value.get,
              options,
              toolchainIdentity,
              runner,
            )
          case failed =>
            Result.failure(Phase.Check, failed.diagnostics)

  private def disabledResult(
      source: SourceFile,
  ): Result[Cosmo0CteCompileProbeModule] =
    val ordinary = Cosmo0().check(source)
    if ordinary.isSuccess then
      Result.success(
        Phase.Check,
        Cosmo0CteCompileProbeModule(source, Nil),
      )
    else Result(Phase.Check, ordinary.status, None, ordinary.diagnostics)

  private def checkParsed(
      parsed: ParsedModule,
      options: Cosmo0CteCompileProbeOptions,
      toolchainIdentity: CosmoEvalToolchainIdentity,
      runner: Cosmo0CteCompileProbeRunner,
  ): Result[Cosmo0CteCompileProbeModule] =
    val diagnostics = ListBuffer.empty[Diagnostic]
    val aliases = ListBuffer.empty[Cosmo0CteCompileProbeAliasResult]

    parsed.ast.stmts.foreach { statement =>
      unwrapSemi(statement) match
        case Some(typeNode: Typ) if isCompileTimeAliasCandidate(typeNode) =>
          smokeRequest(typeNode, parsed.source, options, toolchainIdentity)
            .fold(
              diagnostics += unsupportedExpressionDiagnostic(
                typeNode,
                parsed.source,
              ),
            ) { request =>
              evaluateRequest(
                typeNode,
                parsed.source,
                request,
                runner,
                diagnostics,
                aliases,
              )
            }
        case _ =>
    }

    if diagnostics.nonEmpty then Result.failure(Phase.Check, diagnostics.toList)
    else if aliases.nonEmpty then
      Result.success(
        Phase.Check,
        Cosmo0CteCompileProbeModule(parsed.source, aliases.toList),
      )
    else
      Result.failure(
        Phase.Check,
        List(
          diagnostic(
            UnsupportedExpressionCode,
            "compile-time evaluation probe expects top-level `type x = 1 + 1`",
            parsed.source.span(0, parsed.source.text.length),
          ),
        ),
      )

  private def isCompileTimeAliasCandidate(node: Typ): Boolean =
    node.ty.isEmpty && node.init.exists(isCompileTimeExpr)

  private def isCompileTimeExpr(node: Node): Boolean =
    node match
      case IntLit(_) =>
        true
      case Ident(_) =>
        true
      case UnOp("+" | "-", value) =>
        isCompileTimeExpr(value)
      case BinOp("+" | "-" | "*" | "/" | "%", left, right) =>
        isCompileTimeExpr(left) && isCompileTimeExpr(right)
      case _ =>
        false

  private def smokeRequest(
      node: Typ,
      source: SourceFile,
      options: Cosmo0CteCompileProbeOptions,
      toolchainIdentity: CosmoEvalToolchainIdentity,
  ): Option[CosmoEvalRequest] =
    if node.name.name != SmokeDeclarationName then return None

    node.init match
      case Some(BinOp("+", IntLit(left), IntLit(right)))
          if left == 1 && right == 1 =>
        val span = nodeSpan(source, node)
        Some(
          CosmoEval.request(
            compilerId = "cosmo0",
            evalIdentity =
              s"cosmo0.cte-compile-probe:${source.name}:x:${span.start.offset}",
            providerIdentity = SmokeProviderIdentity,
            serializedInput = serializedSmokeInput(source),
            imports = options.imports,
            headers = options.headers,
            includePaths = options.includePaths,
            librarySearchPaths = options.librarySearchPaths,
            compileOptions = options.compileOptions,
            generatedEntrySource = SmokeProviderEntrySource,
            target = options.target,
            supportLibraryIdentities = options.supportLibraryIdentities,
            toolchainIdentity = toolchainIdentity,
            requestedFacts = List("alias:x", "return:i32"),
            backend = options.backend,
          ),
        )
      case _ =>
        None

  private def serializedSmokeInput(source: SourceFile): String =
    CosmoEvalStableJson.obj(
      List(
        "sourceIdentity" -> CosmoEvalStableJson.string(source.name),
        "declarationIdentity" -> CosmoEvalStableJson.string(
          SmokeDeclarationName,
        ),
        "expressionPayload" -> CosmoEvalStableJson.string(
          SmokeExpressionPayload,
        ),
      ),
    )

  private def evaluateRequest(
      node: Typ,
      source: SourceFile,
      request: CosmoEvalRequest,
      runner: Cosmo0CteCompileProbeRunner,
      diagnostics: ListBuffer[Diagnostic],
      aliases: ListBuffer[Cosmo0CteCompileProbeAliasResult],
  ): Unit =
    val span = nodeSpan(source, node)
    runner.evaluate(request) match
      case Cosmo0CteCompileProbeEvalOutcome.Unavailable(message) =>
        diagnostics += diagnostic(UnavailableCode, message, span)
      case Cosmo0CteCompileProbeEvalOutcome.Completed(result) =>
        consumeEvalResult(node, request, result, span, diagnostics, aliases)

  private def consumeEvalResult(
      node: Typ,
      request: CosmoEvalRequest,
      result: CosmoEvalResult,
      span: SourceSpan,
      diagnostics: ListBuffer[Diagnostic],
      aliases: ListBuffer[Cosmo0CteCompileProbeAliasResult],
  ): Unit =
    result.status match
      case CosmoEvalStatus.Succeeded =>
        result.serializedOutput.flatMap(integerOutput) match
          case Some(value) =>
            aliases += Cosmo0CteCompileProbeAliasResult(
              node.name.name,
              value,
              span,
              request,
              result,
            )
          case None =>
            diagnostics += diagnostic(
              ExecutionFailedCode,
              s"cosmo0 eval returned a successful probe result without an integer output; ${diagnosticSummary(result)}",
              span,
            )
      case CosmoEvalStatus.Unsupported =>
        diagnostics += diagnostic(
          UnavailableCode,
          s"cosmo0 eval is unavailable for the probe request; ${diagnosticSummary(result)}",
          span,
        )
      case CosmoEvalStatus.Failed =>
        val code = failedResultCode(result)
        diagnostics += diagnostic(
          code,
          s"${failedResultMessage(code)}; ${diagnosticSummary(result)}",
          span,
        )

  private def integerOutput(value: String): Option[BigInt] =
    val trimmed = value.trim
    val unsignedDigits = trimmed.forall(_.isDigit)
    val signedDigits = startsWithSign(trimmed) && hasDigits(trimmed.drop(1))

    if trimmed.isEmpty then None
    else if unsignedDigits then Some(BigInt(trimmed))
    else if signedDigits then Some(BigInt(trimmed))
    else None

  private def startsWithSign(value: String): Boolean =
    value.startsWith("+") || value.startsWith("-")

  private def hasDigits(value: String): Boolean =
    value.nonEmpty && value.forall(_.isDigit)

  private def failedResultCode(result: CosmoEvalResult): String =
    val codes = result.diagnostics.map(_.code)
    if codes.exists(_.contains("compile")) then CompileFailedCode
    else ExecutionFailedCode

  private def failedResultMessage(code: String): String =
    code match
      case CompileFailedCode =>
        "compile-time evaluation provider entry compile failed"
      case _ =>
        "compile-time evaluation provider entry execution failed"

  private def diagnosticSummary(result: CosmoEvalResult): String =
    if result.diagnostics.isEmpty then s"eval status ${result.status.id}"
    else
      result.diagnostics
        .map(diagnostic =>
          s"${diagnostic.code}: ${publicDiagnosticMessage(diagnostic.message)}",
        )
        .mkString("; ")

  private def publicDiagnosticMessage(message: String): String =
    message.replace("clang::", "clang ").replace("llvm::", "llvm ")

  private def unsupportedExpressionDiagnostic(
      node: Typ,
      source: SourceFile,
  ): Diagnostic =
    diagnostic(
      UnsupportedExpressionCode,
      "expression is outside the temporary cosmo0 compile-time evaluation probe; only top-level `type x = 1 + 1` is supported",
      nodeSpan(source, node),
    )

  private def diagnostic(
      code: String,
      message: String,
      span: SourceSpan,
  ): Diagnostic =
    Diagnostic(Phase.Check, DiagnosticSeverity.Error, code, message, Some(span))

  private def nodeSpan(source: SourceFile, node: Node): SourceSpan =
    if node.offset >= 0 && node.end >= node.offset then
      source.span(node.offset, node.end)
    else source.span(0, source.text.length)

  private def unwrapSemi(node: Node): Option[Node] =
    node match
      case Semi(None)        => None
      case Semi(Some(inner)) => unwrapSemi(inner)
      case other             => Some(other)

  private def parse(source: SourceFile): Result[ParsedModule] =
    Cosmo0().parse(source)
end Cosmo0CteCompileProbe
