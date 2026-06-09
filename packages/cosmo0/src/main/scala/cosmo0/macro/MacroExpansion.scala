package cosmo0

/** Expression macro provider signature admitted by this compiler-hosted slice.
  */
final case class MacroProviderSignature(
    inputType: String,
    outputType: String,
):
  def isExpressionUntypedToUntyped: Boolean =
    inputType == "Expr[Untyped]" && outputType == "Expr[Untyped]"

object MacroProviderSignature:
  val expressionUntypedToUntyped: MacroProviderSignature =
    MacroProviderSignature("Expr[Untyped]", "Expr[Untyped]")

/** Provider binding selected by expression macro-call classification. */
sealed trait MacroExpressionProviderBinding:
  def stableKey: String
  def providerPath: String

object MacroExpressionProviderBinding:
  final case class Free(path: String) extends MacroExpressionProviderBinding:
    def stableKey: String = s"free:$path"
    def providerPath: String = path

  final case class Method(receiverType: String, selector: String)
      extends MacroExpressionProviderBinding:
    def stableKey: String = s"method:$receiverType.$selector"
    def providerPath: String = s"$receiverType.$selector"

  final case class Block(path: String) extends MacroExpressionProviderBinding:
    def stableKey: String = s"block:$path"
    def providerPath: String = path

  final case class Template(path: String)
      extends MacroExpressionProviderBinding:
    def stableKey: String = s"template:$path"
    def providerPath: String = path

/** One compiler-hosted provider entry. */
final case class MacroExpressionProviderEntry(
    binding: MacroExpressionProviderBinding,
    provider: CompilerHostedExpressionProvider,
)

/** Result of resolving a provider binding. */
sealed trait MacroProviderLookup

object MacroProviderLookup:
  final case class Found(provider: CompilerHostedExpressionProvider)
      extends MacroProviderLookup
  final case class Disabled(providerIdentity: String)
      extends MacroProviderLookup
  case object Missing extends MacroProviderLookup

/** Registry of compiler-hosted expression macro providers.
  *
  * The registry is intentionally expression-scoped. It acts as the ordinary
  * declaration source for compiler-hosted smoke providers until self-hosted
  * provider packages exist.
  */
final class MacroExpressionProviderRegistry(
    entries: List[MacroExpressionProviderEntry],
    disabledProviderIdentities: Set[String] = Set.empty,
    providerNamespacePrefixes: Set[String] = Set("example."),
):
  private val validation = RegistryValidation.build(
    entries,
    disabledProviderIdentities,
  )

  val diagnostics: List[Diagnostic] =
    validation.diagnostics

  def resolveFree(path: String): MacroProviderLookup =
    resolve(MacroExpressionProviderBinding.Free(path).stableKey)

  def resolveMethod(
      receiverType: String,
      selector: String,
  ): MacroProviderLookup =
    resolve(
      MacroExpressionProviderBinding.Method(receiverType, selector).stableKey,
    )

  def resolveBlock(path: String): MacroProviderLookup =
    resolve(MacroExpressionProviderBinding.Block(path).stableKey)

  def resolveTemplate(path: String): MacroProviderLookup =
    resolve(MacroExpressionProviderBinding.Template(path).stableKey)

  def isReservedProviderPath(path: String): Boolean =
    providerNamespacePrefixes.exists(path.startsWith)

  def hasFreeCandidate(path: String): Boolean =
    validation.available.contains(
      MacroExpressionProviderBinding.Free(path).stableKey,
    ) ||
      validation.disabled.contains(
        MacroExpressionProviderBinding.Free(path).stableKey,
      ) ||
      isReservedProviderPath(path)

  def hasBlockCandidate(path: String): Boolean =
    validation.available.contains(
      MacroExpressionProviderBinding.Block(path).stableKey,
    ) ||
      validation.disabled.contains(
        MacroExpressionProviderBinding.Block(path).stableKey,
      ) ||
      isReservedProviderPath(path)

  def hasTemplateCandidate(path: String): Boolean =
    validation.available.contains(
      MacroExpressionProviderBinding.Template(path).stableKey,
    ) ||
      validation.disabled.contains(
        MacroExpressionProviderBinding.Template(path).stableKey,
      ) ||
      isReservedProviderPath(path)

  private def resolve(key: String): MacroProviderLookup =
    validation.available.get(key) match
      case Some(provider) =>
        MacroProviderLookup.Found(provider)
      case None =>
        validation.disabled.get(key) match
          case Some(providerIdentity) =>
            MacroProviderLookup.Disabled(providerIdentity)
          case None =>
            MacroProviderLookup.Missing

object MacroExpressionProviderRegistry:
  val default: MacroExpressionProviderRegistry =
    new MacroExpressionProviderRegistry(
      List(
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.answer"),
          ExampleAnswerExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.identity"),
          ExampleIdentityExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.named"),
          ExampleNamedExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.invalid"),
          ExampleInvalidExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.typed"),
          ExampleTypedExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Free("example.recursive"),
          ExampleRecursiveExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Method("MacroBox", "expand"),
          ExampleMethodExpandExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Block("example.block"),
          ExampleBlockExpressionProvider,
        ),
        MacroExpressionProviderEntry(
          MacroExpressionProviderBinding.Template("example.text"),
          ExampleTextTemplateExpressionProvider,
        ),
      ),
    )

private final case class RegistryValidation(
    available: Map[String, CompilerHostedExpressionProvider],
    disabled: Map[String, String],
    diagnostics: List[Diagnostic],
)

private object RegistryValidation:
  def build(
      entries: List[MacroExpressionProviderEntry],
      disabledProviderIdentities: Set[String],
  ): RegistryValidation =
    val available =
      scala.collection.mutable.LinkedHashMap.empty[
        String,
        CompilerHostedExpressionProvider,
      ]
    val disabled = scala.collection.mutable.LinkedHashMap.empty[String, String]
    val diagnostics = scala.collection.mutable.ListBuffer.empty[Diagnostic]
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]

    entries.foreach { entry =>
      val key = entry.binding.stableKey
      val provider = entry.provider
      if seen.contains(key) then
        diagnostics += registryDiagnostic(
          "cosmo0.macro.duplicate-provider",
          s"expression macro provider binding $key is registered more than once",
        )
      else
        seen += key
        if disabledProviderIdentities.contains(provider.id) then
          disabled.update(key, provider.id)
          diagnostics += registryDiagnostic(
            "cosmo0.macro.disabled-provider",
            s"expression macro provider ${provider.id} is disabled",
          )
        else if !provider.signature.isExpressionUntypedToUntyped then
          diagnostics += registryDiagnostic(
            "cosmo0.macro.invalid-provider-signature",
            s"expression macro provider ${provider.id} must have signature @macro def provider(input: Expr[Untyped]): Expr[Untyped]",
          )
        else available.update(key, provider)
    }

    RegistryValidation(
      available.toMap,
      disabled.toMap,
      diagnostics.toList,
    )

  private def registryDiagnostic(code: String, message: String): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      code,
      message,
      None,
    )

/** Compiler-hosted expression provider contract. */
trait CompilerHostedExpressionProvider:
  def id: String
  def signature: MacroProviderSignature =
    MacroProviderSignature.expressionUntypedToUntyped
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
      case Some(args: MacroExpr.Args)
          if args.receiver.isEmpty && args.positional.isEmpty && args.named.isEmpty =>
        expressionOutput(
          input,
          UntypedIntLiteral(BigInt(42), input.span),
          "expr example.answer -> 42",
        )
      case Some(_: MacroExpr.Args) =>
        diagnosticOutput(input, "example.answer expects no arguments")
      case _ =>
        diagnosticOutput(input, "example.answer expects an Expr.Args payload")

/** Smoke expression macro that returns its single untyped input expression. */
object ExampleIdentityExpressionProvider
    extends CompilerHostedExpressionProvider:
  val id = "example.identity"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    input.payload match
      case Some(args: MacroExpr.Args)
          if args.receiver.isEmpty && args.positional.length == 1 && args.named.isEmpty =>
        expressionOutput(
          input,
          args.positional.head,
          "expr example.identity -> arg0",
        )
      case Some(_: MacroExpr.Args) =>
        diagnosticOutput(
          input,
          "example.identity expects exactly one positional argument",
        )
      case _ =>
        diagnosticOutput(input, "example.identity expects an Expr.Args payload")

/** Smoke expression macro that returns a named argument value. */
object ExampleNamedExpressionProvider extends CompilerHostedExpressionProvider:
  val id = "example.named"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    input.payload match
      case Some(args: MacroExpr.Args)
          if args.receiver.isEmpty && args.positional.isEmpty &&
            args.named.map(_.name) == List("value") =>
        expressionOutput(
          input,
          args.named.head.value,
          "expr example.named -> value",
        )
      case Some(_: MacroExpr.Args) =>
        diagnosticOutput(
          input,
          "example.named expects one named argument `value`",
        )
      case _ =>
        diagnosticOutput(input, "example.named expects an Expr.Args payload")

/** Smoke method-like macro that returns its first non-receiver argument. */
object ExampleMethodExpandExpressionProvider
    extends CompilerHostedExpressionProvider:
  val id = "example.MacroBox.expand"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    input.payload match
      case Some(args: MacroExpr.Args)
          if args.receiver.nonEmpty && args.positional.length == 1 && args.named.isEmpty =>
        expressionOutput(
          input,
          args.positional.head,
          "expr example.MacroBox.expand -> receiver.arg0",
        )
      case Some(_: MacroExpr.Args) =>
        diagnosticOutput(
          input,
          "example.MacroBox.expand expects a receiver and one argument",
        )
      case _ =>
        diagnosticOutput(
          input,
          "example.MacroBox.expand expects an Expr.Args payload",
        )

/** Smoke block macro that proves `Expr.Block` payload dispatch. */
object ExampleBlockExpressionProvider extends CompilerHostedExpressionProvider:
  val id = "example.block"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    input.payload match
      case Some(_: MacroExpr.Block) =>
        expressionOutput(
          input,
          UntypedIntLiteral(BigInt(42), input.span),
          "expr example.block -> 42",
        )
      case _ =>
        diagnosticOutput(input, "example.block expects an Expr.Block payload")

/** Smoke template macro that turns literal template text into a string. */
object ExampleTextTemplateExpressionProvider
    extends CompilerHostedExpressionProvider:
  val id = "example.text"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    input.payload match
      case Some(template: MacroExpr.Template) =>
        expressionOutput(
          input,
          UntypedStringLiteral(templateText(template.parts), input.span),
          "expr example.text -> string",
        )
      case _ =>
        diagnosticOutput(input, "example.text expects an Expr.Template payload")

  private def templateText(parts: List[UntypedTemplatePart]): String =
    parts.map { part =>
      val holeText = part.hole
        .map(hole => MacroStableDisplay.expr(hole.value))
        .getOrElse("")
      part.text + holeText
    }.mkString

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

/** Provider that deliberately attempts typed-expression injection. */
object ExampleTypedExpressionProvider extends CompilerHostedExpressionProvider:
  val id = "example.typed"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    MacroFunctionOutput(
      generatedDeclarations = Nil,
      generatedExpr = Some(MacroExpr.TypedArtifact("trusted-i32", input.span)),
      consumedAttributes = Nil,
      diagnostics = Nil,
      generatedSourceSummary = List("expr example.typed -> <typed>"),
    )

/** Provider that returns the same macro call, proving cycle detection. */
object ExampleRecursiveExpressionProvider
    extends CompilerHostedExpressionProvider:
  val id = "example.recursive"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    val root = UntypedName(UntypedPath(List("example"), input.span), input.span)
    val callee = UntypedSelect(root, "recursive", input.span)
    MacroFunctionOutput(
      generatedDeclarations = Nil,
      generatedExpr = Some(
        MacroExpr.UntypedSource(UntypedCall(callee, Nil, input.span)),
      ),
      consumedAttributes = Nil,
      diagnostics = Nil,
      generatedSourceSummary = List("expr example.recursive -> recurse"),
    )

private def expressionOutput(
    input: MacroFunctionInput,
    expr: UntypedExpr,
    summary: String,
): MacroFunctionOutput =
  MacroFunctionOutput(
    generatedDeclarations = Nil,
    generatedExpr = Some(MacroExpr.UntypedSource(expr)),
    consumedAttributes = Nil,
    diagnostics = Nil,
    generatedSourceSummary = List(summary),
  )

private def diagnosticOutput(
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
