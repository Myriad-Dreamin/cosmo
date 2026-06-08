package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Deterministic cosmo0 macro expansion pass.
  *
  * The first implementation slice supports compiler-hosted derive providers
  * that return trait implementation attachments. The pass preserves ordinary
  * declaration names: generated impls are appended for trait checking, but no
  * generated top-level class/function/value binding is admitted.
  */
object MacroExpander:
  def expand(
      module: UntypedModule,
      profile: CheckerProfile,
      sourcePackageIdentity: String = "<memory>",
      modulePath: List[String] = Nil,
      registry: MacroProviderRegistry = MacroProviderRegistry.default,
  ): Result[MacroExpandedModule] =
    val attributes = MacroAttributeCollector.collect(module)
    if attributes.isEmpty then
      return Result.success(
        Phase.Check,
        MacroExpandedModule(module, MacroExpansionSummary.empty),
      )

    if !profile.supports(CheckerProfiles.MacrosFeature) then
      val diagnostic = CheckerProfiles.unsupportedDiagnostic(
        profile,
        CheckerProfiles.MacrosFeature,
        Some(attributes.head.attribute.span),
      )
      return Result.unsupported(Phase.Check, diagnostic)

    val diagnostics = ListBuffer.empty[Diagnostic]
    val generated = ListBuffer.empty[GeneratedDeclaration]
    val consumedAttributes = ListBuffer.empty[UntypedMacroAttribute]
    val inputs = ListBuffer.empty[MacroFunctionInput]
    val outputs = ListBuffer.empty[MacroFunctionOutput]

    module.decls.foreach:
      case cls: UntypedClass =>
        deriveAttributes(cls.macroAttributes).foreach: attribute =>
          deriveProviderPath(attribute) match
            case Left(diagnostic) =>
              diagnostics += diagnostic
            case Right(providerPath) =>
              registry.resolve(providerPath) match
                case None =>
                  diagnostics += diagnosticAt(
                    "cosmo0.macro.unresolved-provider",
                    s"derive provider $providerPath is not registered",
                    attribute.span,
                  )
                case Some(provider) =>
                  val input = MacroFunctionInput(
                    providerIdentity = provider.id,
                    sourcePackageIdentity = sourcePackageIdentity,
                    invocationIdentity =
                      s"${modulePath.mkString("::")}::${cls.name}:${attribute.span.start.offset}",
                    target = reflectionTarget(cls, modulePath),
                    cxxContext = MacroCxxExecutionContext(),
                    span = attribute.span,
                  )
                  val output =
                    CompilerHostedMacroEvaluator.evaluate(provider, input)
                  inputs += input
                  outputs += output
                  diagnostics ++= output.diagnostics
                  generated ++= output.generatedDeclarations
                  consumedAttributes ++= output.consumedAttributes
      case decl =>
        deriveAttributes(declarationAttributes(decl)).foreach: attribute =>
          diagnostics += diagnosticAt(
            "cosmo0.macro.unsupported-target",
            s"derive macros cannot target ${decl.getClass.getSimpleName.stripSuffix("$")}",
            attribute.span,
          )

    if diagnostics.nonEmpty then
      return Result.failure(Phase.Check, diagnostics.toList)

    diagnostics ++= duplicateGeneratedImplDiagnostics(module, generated.toList)
    if diagnostics.nonEmpty then
      return Result.failure(Phase.Check, diagnostics.toList)

    val consumedSet = consumedAttributes.toSet
    val unconsumed =
      attributes
        .map(_.attribute)
        .filterNot(consumedSet.contains)
        .sortBy(attribute =>
          (
            attribute.span.fileName,
            attribute.span.start.offset,
            attribute.stableDisplay,
          ),
        )
    unconsumed.foreach { attribute =>
      diagnostics += diagnosticAt(
        "cosmo0.macro.unconsumed-attribute",
        s"macro attribute ${attribute.stableDisplay} was not consumed by expansion",
        attribute.span,
      )
    }
    if diagnostics.nonEmpty then
      return Result.failure(Phase.Check, diagnostics.toList)

    val generatedImpls =
      generated.toList.collect {
        case GeneratedDeclaration.TraitImplementationAttachment(impl, _) =>
          impl
      }
    val expandedModule =
      module.copy(decls = module.decls ::: generatedImpls)
    val summary = MacroExpansionSummary(
      inputs.toList.map(_.stableDisplay),
      outputs.toList.flatMap(_.generatedSourceSummary),
      consumedAttributes.toList
        .map(_.stableDisplay)
        .distinct
        .sorted,
    )
    Result.success(
      Phase.Check,
      MacroExpandedModule(expandedModule, summary),
    )

  private def deriveAttributes(
      attributes: List[UntypedMacroAttribute],
  ): List[UntypedMacroAttribute] =
    attributes.filter(attribute =>
      MacroStableDisplay.path(attribute.path) == "derive",
    )

  private def deriveProviderPath(
      attribute: UntypedMacroAttribute,
  ): Either[Diagnostic, String] =
    attribute.args match
      case UntypedMacroAttributeArg(
            None,
            UntypedMacroAttributeValue.PathValue(path),
            _,
          ) :: Nil =>
        Right(MacroStableDisplay.path(path))
      case _ =>
        Left(
          diagnosticAt(
            "cosmo0.macro.invalid-derive-attribute",
            "@derive(...) expects exactly one provider path argument",
            attribute.span,
          ),
        )

  private def reflectionTarget(
      cls: UntypedClass,
      modulePath: List[String],
  ): MacroReflectionTarget =
    MacroReflectionTarget(
      kind = MacroReflectionTargetKind.Class,
      name = cls.name,
      modulePath = modulePath,
      visibility = cls.vis,
      fields = cls.members.collect { case field: UntypedValueDecl =>
        MacroReflectionField(
          field.name,
          field.ty.map(MacroStableDisplay.ty).getOrElse("<inferred>"),
          field.init.map(MacroExpr.UntypedSource.apply),
          field.macroAttributes,
          field.vis,
          field.span,
        )
      },
      variants = cls.members.collect { case variant: UntypedVariant =>
        MacroReflectionVariant(
          variant.name,
          variant.fields.map(field =>
            MacroReflectionVariantField(
              field.name,
              MacroStableDisplay.ty(field.ty),
              field.span,
            ),
          ),
          variant.macroAttributes,
          variant.span,
        )
      },
      functions = cls.members.collect { case fn: UntypedFunction =>
        MacroReflectionFunction(
          fn.name,
          fn.params.map(param =>
            s"${param.name}:${param.ty.map(MacroStableDisplay.ty).getOrElse("<missing>")}",
          ),
          fn.retTy.map(MacroStableDisplay.ty),
          fn.macroAttributes,
          fn.vis,
          fn.span,
        )
      },
      attributes = cls.macroAttributes,
      span = cls.span,
    )

  private def declarationAttributes(
      decl: UntypedDecl,
  ): List[UntypedMacroAttribute] =
    decl match
      case cls: UntypedClass       => cls.macroAttributes
      case trt: UntypedTrait       => trt.macroAttributes
      case fn: UntypedFunction     => fn.macroAttributes
      case value: UntypedValueDecl => value.macroAttributes
      case _                       => Nil

  private def duplicateGeneratedImplDiagnostics(
      module: UntypedModule,
      generated: List[GeneratedDeclaration],
  ): List[Diagnostic] =
    val sourcePairs =
      module.decls.collect { case impl: UntypedImpl =>
        implPair(impl) -> impl.span
      }
    val generatedPairs =
      generated.collect {
        case GeneratedDeclaration.TraitImplementationAttachment(impl, _) =>
          implPair(impl) -> impl.span
      }
    val allPairs = sourcePairs ::: generatedPairs
    allPairs
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .collect {
        case ((traitName, targetName), values) if values.length > 1 =>
          diagnosticAt(
            "cosmo0.macro.duplicate-impl",
            s"macro expansion would duplicate impl $traitName for $targetName",
            values.head._2,
          )
      }

  private def implPair(impl: UntypedImpl): (String, String) =
    (
      MacroStableDisplay.path(impl.traitName),
      MacroStableDisplay.path(impl.target),
    )

  private def diagnosticAt(
      code: String,
      message: String,
      span: SourceSpan,
  ): Diagnostic =
    Diagnostic(
      Phase.Check,
      DiagnosticSeverity.Error,
      code,
      message,
      Some(span),
    )

/** Collects all macro attributes that remain on supported untyped shapes. */
private object MacroAttributeCollector:
  final case class Occurrence(attribute: UntypedMacroAttribute)

  def collect(module: UntypedModule): List[Occurrence] =
    module.decls.flatMap(declarationAttributes).sortBy { occurrence =>
      val span = occurrence.attribute.span
      (span.fileName, span.start.offset, occurrence.attribute.stableDisplay)
    }

  private def declarationAttributes(decl: UntypedDecl): List[Occurrence] =
    val own =
      decl match
        case cls: UntypedClass       => cls.macroAttributes
        case trt: UntypedTrait       => trt.macroAttributes
        case fn: UntypedFunction     => fn.macroAttributes
        case value: UntypedValueDecl => value.macroAttributes
        case _                       => Nil
    val memberAttributes =
      decl match
        case cls: UntypedClass =>
          cls.members.flatMap(classMemberAttributes)
        case trt: UntypedTrait =>
          trt.methods.flatMap(fn => fn.macroAttributes.map(Occurrence.apply))
        case _ =>
          Nil
    own.map(Occurrence.apply) ::: memberAttributes

  private def classMemberAttributes(
      member: UntypedClassMember,
  ): List[Occurrence] =
    member match
      case fn: UntypedFunction =>
        fn.macroAttributes.map(Occurrence.apply)
      case value: UntypedValueDecl =>
        value.macroAttributes.map(Occurrence.apply)
      case variant: UntypedVariant =>
        variant.macroAttributes.map(Occurrence.apply)
      case _ =>
        Nil

/** Registry of compiler-hosted macro providers. */
private type DeriveProviderMap = Map[String, CompilerHostedDeriveProvider]

final class MacroProviderRegistry(providers: DeriveProviderMap):
  def resolve(path: String): Option[CompilerHostedDeriveProvider] =
    providers.get(path)

object MacroProviderRegistry:
  val default: MacroProviderRegistry =
    new MacroProviderRegistry(
      List(
        FieldCountDeriveProvider,
      ).map(provider => provider.id -> provider).toMap,
    )

/** Compiler-hosted derive provider contract. */
trait CompilerHostedDeriveProvider:
  def id: String
  def evaluate(input: MacroFunctionInput): MacroFunctionOutput

/** Adapter that keeps compiler-hosted providers on the same input/output
  * boundary as later self-hosted providers.
  */
object CompilerHostedMacroEvaluator:
  def evaluate(
      provider: CompilerHostedDeriveProvider,
      input: MacroFunctionInput,
  ): MacroFunctionOutput =
    provider.evaluate(input)

/** Smoke derive provider used by tests and by the first macro-system slice.
  *
  * `@derive(example.FieldCount)` generates an implementation of the existing
  * local trait `FieldCount` for the target class. The implementation returns
  * the number of reflected fields. The provider also consumes field `@arg(...)`
  * attributes with keyed string payloads, proving that field metadata reaches
  * the provider and that misspelled attributes are rejected as unconsumed.
  */
object FieldCountDeriveProvider extends CompilerHostedDeriveProvider:
  val id = "example.FieldCount"

  def evaluate(input: MacroFunctionInput): MacroFunctionOutput =
    if input.target.kind != MacroReflectionTargetKind.Class then
      return withDiagnostic(
        input,
        "cosmo0.macro.unsupported-target",
        s"$id can only derive classes",
      )

    val unsupportedArgAttributes =
      input.target.fields.flatMap(field =>
        field.attributes.filter(attribute =>
          MacroStableDisplay.path(attribute.path) == "arg" &&
            !isSupportedArgAttribute(attribute),
        ),
      )
    if unsupportedArgAttributes.nonEmpty then
      return MacroFunctionOutput(
        generatedDeclarations = Nil,
        generatedExpr = None,
        consumedAttributes = input.target.attributes.filter(isDeriveAttribute),
        diagnostics = unsupportedArgAttributes.map(attribute =>
          Diagnostic(
            Phase.Check,
            DiagnosticSeverity.Error,
            "cosmo0.macro.unsupported-attribute-payload",
            "@arg(...) attributes accepted by example.FieldCount must use keyed string arguments long and short",
            Some(attribute.span),
          ),
        ),
        generatedSourceSummary = Nil,
      )

    val span = input.span
    val impl = UntypedImpl(
      traitName = UntypedPath(List("FieldCount"), span),
      target = UntypedPath(List(input.target.name), input.target.span),
      members = List(
        fieldCountMethod(input.target.name, input.target.fields.length, span),
      ),
      span = span,
      vis = UntypedVisibility.Private,
    )
    val consumed =
      input.target.attributes.filter(isDeriveAttribute) :::
        input.target.fields.flatMap(_.attributes.filter(isArgAttribute))
    MacroFunctionOutput(
      generatedDeclarations =
        List(GeneratedDeclaration.TraitImplementationAttachment(impl, span)),
      generatedExpr = None,
      consumedAttributes = consumed,
      diagnostics = Nil,
      generatedSourceSummary = List(
        s"derive example.FieldCount -> impl FieldCount for ${input.target.name}",
      ),
    )

  private def fieldCountMethod(
      targetName: String,
      fieldCount: Int,
      span: SourceSpan,
  ): UntypedFunction =
    val selfType =
      UntypedRefType(
        UntypedNamedType(UntypedPath(List("Self"), span), span),
        mut = false,
        span,
      )
    val i32Type = UntypedNamedType(UntypedPath(List("i32"), span), span)
    UntypedFunction(
      name = "field_count",
      params = List(
        UntypedParam(
          "self",
          Some(selfType),
          None,
          span,
        ),
      ),
      retTy = Some(i32Type),
      body = Some(UntypedIntLiteral(BigInt(fieldCount), span)),
      span = span,
      extern = None,
      vis = UntypedVisibility.Private,
      macroAttributes = Nil,
    )

  private def withDiagnostic(
      input: MacroFunctionInput,
      code: String,
      message: String,
  ): MacroFunctionOutput =
    MacroFunctionOutput(
      generatedDeclarations = Nil,
      generatedExpr = None,
      consumedAttributes = input.target.attributes.filter(isDeriveAttribute),
      diagnostics = List(
        Diagnostic(
          Phase.Check,
          DiagnosticSeverity.Error,
          code,
          message,
          Some(input.span),
        ),
      ),
      generatedSourceSummary = Nil,
    )

  private def isDeriveAttribute(attribute: UntypedMacroAttribute): Boolean =
    MacroStableDisplay.path(attribute.path) == "derive"

  private def isArgAttribute(attribute: UntypedMacroAttribute): Boolean =
    MacroStableDisplay.path(attribute.path) == "arg"

  private def isSupportedArgAttribute(
      attribute: UntypedMacroAttribute,
  ): Boolean =
    attribute.args.nonEmpty &&
      attribute.args.forall {
        case UntypedMacroAttributeArg(
              Some("long" | "short"),
              UntypedMacroAttributeValue.StringValue(_),
              _,
            ) =>
          true
        case _ =>
          false
      }
