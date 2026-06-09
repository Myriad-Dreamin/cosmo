package cosmo0

/** Registry of compiler-hosted expression macro providers.
  *
  * This is intentionally expression-scoped: callers ask about a parsed callee
  * while checking an expression. There is no module-wide expansion pass here.
  */
private type ExpressionProviderMap =
  Map[String, CompilerHostedExpressionProvider]

final class MacroExpressionProviderRegistry(providers: ExpressionProviderMap):
  def resolve(path: String): Option[CompilerHostedExpressionProvider] =
    providers.get(path)

  def isMacroCandidate(path: String): Boolean =
    providers.contains(path) || path.startsWith("example.")

object MacroExpressionProviderRegistry:
  val default: MacroExpressionProviderRegistry =
    new MacroExpressionProviderRegistry(
      List(
        ExampleAnswerExpressionProvider,
        ExampleIdentityExpressionProvider,
        ExampleInvalidExpressionProvider,
      ).map(provider => provider.id -> provider).toMap,
    )

/** Compiler-hosted expression provider contract. */
trait CompilerHostedExpressionProvider:
  def id: String
  def evaluate(input: MacroFunctionInput): MacroFunctionOutput

/** Adapter that keeps compiler-hosted providers on the same serialized
  * input/output boundary that self-hosted providers will use.
  */
object CompilerHostedMacroEvaluator:
  def evaluateExpression(
      provider: CompilerHostedExpressionProvider,
      input: MacroFunctionInput,
  ): MacroFunctionOutput =
    provider.evaluate(input)

/** Smoke expression macro used by tests.
  *
  * `example.answer()` expands to the untyped integer literal `42`. The literal
  * is checked later by the ordinary typer, so caller expected types still
  * control literal typing.
  */
object ExampleAnswerExpressionProvider extends CompilerHostedExpressionProvider:
  val id = "example.answer"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    input.payload match
      case Some(args: MacroExpr.Args) if args.positional.isEmpty =>
        MacroFunctionOutput(
          generatedDeclarations = Nil,
          generatedExpr = Some(
            MacroExpr.UntypedSource(UntypedIntLiteral(BigInt(42), input.span)),
          ),
          consumedAttributes = Nil,
          diagnostics = Nil,
          generatedSourceSummary = List("expr example.answer -> 42"),
        )
      case Some(_: MacroExpr.Args) =>
        MacroFunctionOutput(
          generatedDeclarations = Nil,
          generatedExpr = None,
          consumedAttributes = Nil,
          diagnostics = List(
            Diagnostic(
              Phase.Check,
              DiagnosticSeverity.Error,
              "cosmo0.macro.unsupported-input",
              "example.answer expects no arguments",
              Some(input.span),
            ),
          ),
          generatedSourceSummary = Nil,
        )
      case _ =>
        invalidInput(input, "example.answer expects an Expr.Args payload")

/** Smoke expression macro that returns its single untyped input expression. */
object ExampleIdentityExpressionProvider
    extends CompilerHostedExpressionProvider:
  val id = "example.identity"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    input.payload match
      case Some(args: MacroExpr.Args) if args.positional.length == 1 =>
        MacroFunctionOutput(
          generatedDeclarations = Nil,
          generatedExpr = Some(MacroExpr.UntypedSource(args.positional.head)),
          consumedAttributes = Nil,
          diagnostics = Nil,
          generatedSourceSummary = List("expr example.identity -> arg0"),
        )
      case Some(_: MacroExpr.Args) =>
        MacroFunctionOutput(
          generatedDeclarations = Nil,
          generatedExpr = None,
          consumedAttributes = Nil,
          diagnostics = List(
            Diagnostic(
              Phase.Check,
              DiagnosticSeverity.Error,
              "cosmo0.macro.unsupported-input",
              "example.identity expects exactly one argument",
              Some(input.span),
            ),
          ),
          generatedSourceSummary = Nil,
        )
      case _ =>
        invalidInput(input, "example.identity expects an Expr.Args payload")

/** Provider that deliberately returns no expression, so the checker can prove
  * invalid expression macro output is rejected at the expression site.
  */
object ExampleInvalidExpressionProvider
    extends CompilerHostedExpressionProvider:
  val id = "example.invalid"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    MacroFunctionOutput(
      generatedDeclarations = Nil,
      generatedExpr = None,
      consumedAttributes = Nil,
      diagnostics = Nil,
      generatedSourceSummary = List("expr example.invalid -> <invalid>"),
    )

private def invalidInput(
    input: MacroFunctionInput,
    message: String,
): MacroFunctionOutput =
  MacroFunctionOutput(
    generatedDeclarations = Nil,
    generatedExpr = None,
    consumedAttributes = Nil,
    diagnostics = List(
      Diagnostic(
        Phase.Check,
        DiagnosticSeverity.Error,
        "cosmo0.macro.unsupported-input",
        message,
        Some(input.span),
      ),
    ),
    generatedSourceSummary = Nil,
  )
