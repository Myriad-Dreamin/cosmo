package cosmo0

/** Structured macro attribute preserved by elaboration.
  *
  * The source parser already recognizes decorators broadly. This record is the
  * cosmo0-owned subset that can cross the parsing/elaboration boundary for
  * macro expansion instead of being flattened back to raw source text.
  */
final case class UntypedMacroAttribute(
    path: UntypedPath,
    args: List[UntypedMacroAttributeArg],
    span: SourceSpan,
):
  def stableDisplay: String =
    val renderedArgs = args.map(_.stableDisplay).mkString(",")
    if renderedArgs.isEmpty then s"@${MacroStableDisplay.path(path)}"
    else s"@${MacroStableDisplay.path(path)}($renderedArgs)"

/** Positional or keyed macro attribute argument. */
final case class UntypedMacroAttributeArg(
    name: Option[String],
    value: UntypedMacroAttributeValue,
    span: SourceSpan,
):
  def stableDisplay: String =
    name match
      case Some(key) => s"$key=${value.stableDisplay}"
      case None      => value.stableDisplay

/** Attribute values admitted by the first macro metadata boundary. */
enum UntypedMacroAttributeValue:
  case StringValue(value: String)
  case IntValue(value: BigInt)
  case BoolValue(value: Boolean)
  case PathValue(path: UntypedPath)

  def stableDisplay: String =
    this match
      case StringValue(value) => "\"" + escape(value) + "\""
      case IntValue(value)    => value.toString
      case BoolValue(value)   => value.toString
      case PathValue(path)    => MacroStableDisplay.path(path)

  private def escape(value: String): String =
    value.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch   => ch.toString
    }

/** Public macro expression value. The stable macro API is `Expr[T = Untyped]`;
  * this compiler-side mirror keeps it distinct from serialized macro function
  * records and from `TypedExpr`.
  */
sealed trait MacroExpr:
  def span: SourceSpan
  def stableDisplay: String

object MacroExpr:
  final case class UntypedSource(value: UntypedExpr) extends MacroExpr:
    def span: SourceSpan = value.span
    def stableDisplay: String = MacroStableDisplay.expr(value)

/** Reflection target kind admitted by the first macro expansion slice. */
enum MacroReflectionTargetKind:
  case Class, Function, Field, Variant

/** Reflected class field metadata visible to derive providers. */
final case class MacroReflectionField(
    name: String,
    ty: String,
    default: Option[MacroExpr],
    attributes: List[UntypedMacroAttribute],
    visibility: UntypedVisibility,
    span: SourceSpan,
):
  def stableDisplay: String =
    val attrText = attributes.map(_.stableDisplay).mkString("[", ",", "]")
    val defaultText =
      default.map(value => s"=${value.stableDisplay}").getOrElse("")
    s"field:$name:$ty$defaultText:$attrText:${MacroStableDisplay.visibility(visibility)}"

/** Reflected variant payload field metadata. */
final case class MacroReflectionVariantField(
    name: Option[String],
    ty: String,
    span: SourceSpan,
):
  def stableDisplay: String =
    name.map(value => s"$value:").getOrElse("") + ty

/** Reflected enum-style variant metadata visible to derive providers. */
final case class MacroReflectionVariant(
    name: String,
    fields: List[MacroReflectionVariantField],
    attributes: List[UntypedMacroAttribute],
    span: SourceSpan,
):
  def stableDisplay: String =
    val fieldText = fields.map(_.stableDisplay).mkString("(", ",", ")")
    val attrText = attributes.map(_.stableDisplay).mkString("[", ",", "]")
    s"variant:$name$fieldText:$attrText"

/** Reflected function metadata visible to macro providers. */
final case class MacroReflectionFunction(
    name: String,
    params: List[String],
    returnType: Option[String],
    attributes: List[UntypedMacroAttribute],
    visibility: UntypedVisibility,
    span: SourceSpan,
):
  def stableDisplay: String =
    val paramText = params.mkString("(", ",", ")")
    val returnText = returnType.getOrElse("Unit")
    val attrText = attributes.map(_.stableDisplay).mkString("[", ",", "]")
    s"function:$name$paramText:$returnText:$attrText:${MacroStableDisplay.visibility(visibility)}"

/** Reflection facts selected by cosmo0 for one macro target. */
final case class MacroReflectionTarget(
    kind: MacroReflectionTargetKind,
    name: String,
    modulePath: List[String],
    visibility: UntypedVisibility,
    fields: List[MacroReflectionField],
    variants: List[MacroReflectionVariant],
    functions: List[MacroReflectionFunction],
    attributes: List[UntypedMacroAttribute],
    span: SourceSpan,
):
  def stableDisplay: String =
    val moduleText =
      if modulePath.isEmpty then "<memory>" else modulePath.mkString("::")
    val attrText = attributes.map(_.stableDisplay).mkString("[", ",", "]")
    val fieldText = fields.map(_.stableDisplay).mkString("[", ",", "]")
    val variantText = variants.map(_.stableDisplay).mkString("[", ",", "]")
    val functionText = functions.map(_.stableDisplay).mkString("[", ",", "]")
    s"target:${kind.toString}:$moduleText::$name:${MacroStableDisplay
        .visibility(visibility)}:$attrText:fields=$fieldText:variants=$variantText:functions=$functionText"

/** Explicit C++ execution context included in macro function input. The first
  * compiler-hosted providers use the empty context, while later self-hosted
  * providers can route requests through cosmo0 eval with the same record shape.
  */
final case class MacroCxxExecutionContext(
    imports: List[String] = Nil,
    headers: List[String] = Nil,
    targetTriple: Option[String] = None,
    cppStandard: Option[String] = None,
    precompiledContextKey: Option[String] = None,
    toolchainIdentity: Option[String] = None,
):
  def stableDisplay: String =
    val importText = imports.sorted.mkString("[", ",", "]")
    val headerText = headers.sorted.mkString("[", ",", "]")
    val targetText = targetTriple.getOrElse("")
    val standardText = cppStandard.getOrElse("")
    val pchText = precompiledContextKey.getOrElse("")
    val toolchainText = toolchainIdentity.getOrElse("")
    s"cxx(imports=$importText,headers=$headerText,target=$targetText,std=$standardText,pch=$pchText,toolchain=$toolchainText)"

/** Serialized provider input selected by the compiler. */
final case class MacroFunctionInput(
    providerIdentity: String,
    sourcePackageIdentity: String,
    invocationIdentity: String,
    target: MacroReflectionTarget,
    cxxContext: MacroCxxExecutionContext,
    span: SourceSpan,
):
  def stableDisplay: String =
    s"input(provider=$providerIdentity,package=$sourcePackageIdentity,invocation=$invocationIdentity,${target.stableDisplay},${cxxContext.stableDisplay})"

/** Structured generated declaration output. */
sealed trait GeneratedDeclaration:
  def span: SourceSpan
  def stableDisplay: String

object GeneratedDeclaration:
  final case class TraitImplementationAttachment(
      impl: UntypedImpl,
      origin: SourceSpan,
  ) extends GeneratedDeclaration:
    def span: SourceSpan = impl.span
    def stableDisplay: String =
      s"impl:${MacroStableDisplay.path(impl.traitName)} for ${MacroStableDisplay
          .path(impl.target)}"

/** Serialized provider output accepted by the macro expansion boundary. */
final case class MacroFunctionOutput(
    generatedDeclarations: List[GeneratedDeclaration],
    generatedExpr: Option[MacroExpr],
    consumedAttributes: List[UntypedMacroAttribute],
    diagnostics: List[Diagnostic],
    generatedSourceSummary: List[String],
    nativeSupportBindings: List[String] = Nil,
):
  def stableDisplay: String =
    val generated =
      generatedDeclarations.map(_.stableDisplay).mkString("[", ",", "]")
    val consumed =
      consumedAttributes.map(_.stableDisplay).mkString("[", ",", "]")
    val summary = generatedSourceSummary.mkString("[", ",", "]")
    val support = nativeSupportBindings.sorted.mkString("[", ",", "]")
    s"output(generated=$generated,consumed=$consumed,summary=$summary,support=$support)"

/** Result of running the macro expansion phase over an untyped module. */
final case class MacroExpandedModule(
    module: UntypedModule,
    summary: MacroExpansionSummary,
)

/** Deterministic generated-source inspection summary for tests and debugging.
  */
final case class MacroExpansionSummary(
    invocations: List[String],
    generated: List[String],
    consumedAttributes: List[String],
):
  def isEmpty: Boolean =
    invocations.isEmpty && generated.isEmpty && consumedAttributes.isEmpty

  def stableDisplay: String =
    if isEmpty then "macro-expansion:none"
    else
      val invocationText = invocations.mkString("[", ",", "]")
      val generatedText = generated.mkString("[", ",", "]")
      val consumedText = consumedAttributes.mkString("[", ",", "]")
      s"macro-expansion(invocations=$invocationText,generated=$generatedText,consumed=$consumedText)"

object MacroExpansionSummary:
  val empty: MacroExpansionSummary =
    MacroExpansionSummary(Nil, Nil, Nil)

/** Stable rendering helpers used by macro records and golden-style tests. */
object MacroStableDisplay:
  def path(path: UntypedPath): String =
    path.parts.mkString(".")

  def visibility(value: UntypedVisibility): String =
    value match
      case UntypedVisibility.Public  => "public"
      case UntypedVisibility.Private => "private"

  def ty(value: UntypedType): String =
    value match
      case UntypedNamedType(path, _) => path.parts.mkString(".")
      case UntypedAppliedType(base, args, _) =>
        s"${path(base)}[${args.map(ty).mkString(",")}]"
      case UntypedRefType(target, mut, _) =>
        if mut then s"&mut ${ty(target)}" else s"&${ty(target)}"

  def expr(value: UntypedExpr): String =
    value match
      case UntypedName(path, _) => path.parts.mkString(".")
      case UntypedSelect(recv, field, _) =>
        s"${expr(recv)}.$field"
      case UntypedCall(callee, args, _) =>
        s"${expr(callee)}(${args.map(expr).mkString(",")})"
      case UntypedBoolLiteral(value, _) =>
        value.toString
      case UntypedIntLiteral(value, _) =>
        value.toString
      case UntypedFloatLiteral(value, _) =>
        value.toString
      case UntypedStringLiteral(value, _) =>
        "\"" + value + "\""
      case UntypedAsciiLiteral(value, _) =>
        "a\"" + value + "\""
      case UntypedRuneLiteral(value, _) =>
        "c\"" + value + "\""
      case UntypedUnitLiteral(_) =>
        "()"
      case UntypedUnary(op, inner, _) =>
        s"$op${expr(inner)}"
      case UntypedBinary(op, left, right, _) =>
        s"(${expr(left)} $op ${expr(right)})"
      case other =>
        other.getClass.getSimpleName.stripSuffix("$")
