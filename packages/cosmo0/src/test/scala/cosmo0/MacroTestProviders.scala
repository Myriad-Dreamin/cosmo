package cosmo0

/** Test-only compiler-hosted providers. Production registries stay empty; macro
  * tests inject these registries explicitly.
  */
object MacroTestProviders:
  val expressionRegistry: MacroExpressionProviderRegistry =
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
      providerNamespacePrefixes = Set("example."),
    )

  val deriveRegistry: MacroDeriveProviderRegistry =
    new MacroDeriveProviderRegistry(
      List(
        MacroDeriveProviderEntry(
          "example.FieldCount",
          ExampleFieldCountDeriveProvider,
        ),
        MacroDeriveProviderEntry(
          "example.PartialAttributes",
          ExamplePartialAttributesDeriveProvider,
        ),
        MacroDeriveProviderEntry(
          "example.InvalidDerive",
          ExampleInvalidDeriveProvider,
        ),
        MacroDeriveProviderEntry(
          "example.TypedDerive",
          ExampleTypedDeriveProvider,
        ),
        MacroDeriveProviderEntry(
          "example.UnsupportedTrait",
          ExampleUnsupportedTraitDeriveProvider,
        ),
      ),
      providerNamespacePrefixes = Set("example."),
    )

  object ExampleAnswerExpressionProvider
      extends CompilerHostedExpressionProvider:
    val id = "example.answer"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      input.payload match
        case Some(args: MacroExpr.Args)
            if args.receiver.isEmpty && args.positional.isEmpty &&
              args.named.isEmpty =>
          expressionOutput(
            input,
            UntypedIntLiteral(BigInt(42), input.span),
            "expr example.answer -> 42",
          )
        case Some(_: MacroExpr.Args) =>
          diagnosticOutput(input, "example.answer expects no arguments")
        case _ =>
          diagnosticOutput(input, "example.answer expects an Expr.Args payload")

  object ExampleIdentityExpressionProvider
      extends CompilerHostedExpressionProvider:
    val id = "example.identity"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      input.payload match
        case Some(args: MacroExpr.Args)
            if args.receiver.isEmpty && args.positional.length == 1 &&
              args.named.isEmpty =>
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
          diagnosticOutput(
            input,
            "example.identity expects an Expr.Args payload",
          )

  object ExampleNamedExpressionProvider
      extends CompilerHostedExpressionProvider:
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

  object ExampleMethodExpandExpressionProvider
      extends CompilerHostedExpressionProvider:
    val id = "example.MacroBox.expand"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      input.payload match
        case Some(args: MacroExpr.Args)
            if args.receiver.nonEmpty && args.positional.length == 1 &&
              args.named.isEmpty =>
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

  object ExampleBlockExpressionProvider
      extends CompilerHostedExpressionProvider:
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
          diagnosticOutput(
            input,
            "example.text expects an Expr.Template payload",
          )

    private def templateText(parts: List[UntypedTemplatePart]): String =
      parts.map { part =>
        val holeText = part.hole
          .map(hole => MacroStableDisplay.expr(hole.value))
          .getOrElse("")
        part.text + holeText
      }.mkString

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

  object ExampleTypedExpressionProvider
      extends CompilerHostedExpressionProvider:
    val id = "example.typed"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      MacroFunctionOutput(
        generatedDeclarations = Nil,
        generatedExpr =
          Some(MacroExpr.TypedArtifact("trusted-i32", input.span)),
        consumedAttributes = Nil,
        diagnostics = Nil,
        generatedSourceSummary = List("expr example.typed -> <typed>"),
      )

  object ExampleRecursiveExpressionProvider
      extends CompilerHostedExpressionProvider:
    val id = "example.recursive"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      val root =
        UntypedName(UntypedPath(List("example"), input.span), input.span)
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

  object ExampleFieldCountDeriveProvider
      extends CompilerHostedDeriveProvider:
    val id = "example.FieldCount"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      fieldCountDeriveOutput(input, allInputAttributes(input))

  object ExamplePartialAttributesDeriveProvider
      extends CompilerHostedDeriveProvider:
    val id = "example.PartialAttributes"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      fieldCountDeriveOutput(input, input.target.attributes)

  object ExampleUnsupportedTraitDeriveProvider
      extends CompilerHostedDeriveProvider:
    val id = "example.UnsupportedTrait"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      fieldCountDeriveOutput(input, allInputAttributes(input))

  object ExampleInvalidDeriveProvider extends CompilerHostedDeriveProvider:
    val id = "example.InvalidDerive"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      MacroFunctionOutput(
        generatedDeclarations = Nil,
        generatedExpr = Some(
          MacroExpr.UntypedSource(UntypedIntLiteral(BigInt(0), input.span)),
        ),
        consumedAttributes = input.target.attributes,
        diagnostics = Nil,
        generatedSourceSummary =
          List("derive example.InvalidDerive -> <invalid>"),
      )

  object ExampleTypedDeriveProvider extends CompilerHostedDeriveProvider:
    val id = "example.TypedDerive"

    def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
      MacroFunctionOutput(
        generatedDeclarations = Nil,
        generatedExpr =
          Some(MacroExpr.TypedArtifact("trusted-derive", input.span)),
        consumedAttributes = input.target.attributes,
        diagnostics = Nil,
        generatedSourceSummary = List("derive example.TypedDerive -> <typed>"),
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

  private def fieldCountDeriveOutput(
      input: MacroFunctionInput,
      consumedAttributes: List[UntypedMacroAttribute],
  ): MacroFunctionOutput =
    input.selectedTrait match
      case Some(selectedTrait) =>
        val impl = fieldCountImpl(input, selectedTrait.identity)
        MacroFunctionOutput(
          generatedDeclarations = List(
            GeneratedDeclaration.TraitImplementationAttachment(
              impl,
              input.span,
            ),
          ),
          generatedExpr = None,
          consumedAttributes = consumedAttributes,
          diagnostics = Nil,
          generatedSourceSummary = List(
            s"derive ${input.providerIdentity} -> impl ${selectedTrait.identity} for ${input.target.name}",
          ),
        )
      case None =>
        diagnosticOutput(
          input,
          s"derive provider ${input.providerIdentity} requires a selected trait",
        )

  private def fieldCountImpl(
      input: MacroFunctionInput,
      traitName: String,
  ): UntypedImpl =
    val span = input.span
    val selfType =
      UntypedRefType(namedType("Self", span), mut = false, span)
    val method =
      UntypedFunction(
        "field_count",
        List(UntypedParam("self", Some(selfType), None, span)),
        Some(namedType("i32", span)),
        Some(
          UntypedBlock(
            List(UntypedIntLiteral(BigInt(input.target.fields.length), span)),
            span,
          ),
        ),
        span,
        extern = None,
        vis = UntypedVisibility.Private,
      )
    UntypedImpl(
      UntypedPath(List(traitName), span),
      UntypedPath(List(input.target.name), span),
      List(method),
      span,
    )

  private def namedType(name: String, span: SourceSpan): UntypedNamedType =
    UntypedNamedType(UntypedPath(List(name), span), span)

  private def allInputAttributes(
      input: MacroFunctionInput,
  ): List[UntypedMacroAttribute] =
    input.target.attributes :::
      input.target.fields.flatMap(_.attributes) :::
      input.target.variants.flatMap(_.attributes) :::
      input.target.functions.flatMap(_.attributes)
