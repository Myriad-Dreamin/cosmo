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

  final case class NamedArg(
      name: String,
      value: UntypedExpr,
      span: SourceSpan,
  ):
    def stableDisplay: String =
      s"$name=${MacroStableDisplay.expr(value)}"

  final case class Args(
      receiver: Option[UntypedExpr],
      positional: List[UntypedExpr],
      named: List[NamedArg],
      span: SourceSpan,
  ) extends MacroExpr:
    def stableDisplay: String =
      val receiverText = receiver.map(MacroStableDisplay.expr).getOrElse("")
      val positionalText =
        positional.map(MacroStableDisplay.expr).mkString("[", ",", "]")
      val namedText = named.map(_.stableDisplay).mkString("[", ",", "]")
      s"Expr.Args(receiver=$receiverText,positional=$positionalText,named=$namedText)"

  final case class Block(value: UntypedBlock) extends MacroExpr:
    def span: SourceSpan = value.span
    def stableDisplay: String =
      s"Expr.Block(${MacroStableDisplay.expr(value)})"

  final case class Template(
      tag: UntypedPath,
      parts: List[UntypedTemplatePart],
      span: SourceSpan,
  ) extends MacroExpr:
    def stableDisplay: String =
      val partText =
        parts.map(MacroStableDisplay.templatePart).mkString("[", ",", "]")
      s"Expr.Template(tag=${MacroStableDisplay.path(tag)},parts=$partText)"

  final case class TypedArtifact(summary: String, span: SourceSpan)
      extends MacroExpr:
    def stableDisplay: String = s"TypedExpr($summary)"

/** Reflection target kind admitted by the first macro expansion slice. */
enum MacroReflectionTargetKind:
  case Class, Function, Field, Variant, Expression

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

/** Trait requirement metadata selected for derive implementation validation.
  */
final case class MacroTraitRequirement(
    name: String,
    params: List[String],
    returnType: String,
    receiver: Option[String],
    span: SourceSpan,
):
  def stableDisplay: String =
    val receiverText = receiver.map(value => s"receiver=$value,").getOrElse("")
    val paramText = params.mkString("(", ",", ")")
    s"requirement:$name(${receiverText}params=$paramText,return=$returnType)"

/** Selected trait facts supplied to a derive provider. */
final case class MacroSelectedTrait(
    identity: String,
    path: UntypedPath,
    requirements: List[MacroTraitRequirement],
    span: SourceSpan,
):
  def stableDisplay: String =
    val requirementText =
      requirements.map(_.stableDisplay).mkString("[", ",", "]")
    s"trait:$identity:${MacroStableDisplay.path(path)}:$requirementText"

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
    val spanText = MacroStableDisplay.span(span)
    val attrText = attributes.map(_.stableDisplay).mkString("[", ",", "]")
    val fieldText = fields.map(_.stableDisplay).mkString("[", ",", "]")
    val variantText = variants.map(_.stableDisplay).mkString("[", ",", "]")
    val functionText = functions.map(_.stableDisplay).mkString("[", ",", "]")
    s"target:${kind.toString}:$moduleText::$name:${MacroStableDisplay
        .visibility(visibility)}:$spanText:$attrText:fields=$fieldText:variants=$variantText:functions=$functionText"

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
    payload: Option[MacroExpr] = None,
    selectedTrait: Option[MacroSelectedTrait] = None,
):
  def stableDisplay: String =
    val payloadText = payload.map(_.stableDisplay).getOrElse("")
    val traitText =
      selectedTrait.map(value => s",${value.stableDisplay}").getOrElse("")
    s"input(provider=$providerIdentity,package=$sourcePackageIdentity,invocation=$invocationIdentity,payload=$payloadText,${target.stableDisplay}$traitText,${cxxContext.stableDisplay})"

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

  def span(span: SourceSpan): String =
    s"${span.fileName}:${span.start.line}:${span.start.column}-${span.end.line}:${span.end.column}"

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
        s"${expr(callee)}(${args.map(callArg).mkString(",")})"
      case UntypedBlockCall(callee, block, _) =>
        s"${expr(callee)} ${expr(block)}"
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
      case UntypedBlock(items, _) =>
        s"{${items.map(blockItem).mkString(";")}}"
      case UntypedTemplate(tag, parts, _) =>
        val partText = parts.map(templatePart).mkString("[", ",", "]")
        s"${path(tag)}$partText"
      case other =>
        other.getClass.getSimpleName.stripSuffix("$")

  def callArg(value: UntypedCallArg): String =
    value match
      case UntypedCallArg.Positional(expr, _) =>
        MacroStableDisplay.expr(expr)
      case UntypedCallArg.Named(name, expr, _) =>
        s"$name=${MacroStableDisplay.expr(expr)}"

  def blockItem(value: UntypedBlockItem): String =
    value match
      case expr: UntypedExpr => MacroStableDisplay.expr(expr)
      case UntypedExprStmt(expr, _) =>
        MacroStableDisplay.expr(expr)
      case UntypedLocal(kind, name, ty, init, _) =>
        val kindText = kind.toString.toLowerCase
        val tyText =
          ty.map(value => s":${MacroStableDisplay.ty(value)}").getOrElse("")
        val initText =
          init.map(value => s"=${MacroStableDisplay.expr(value)}").getOrElse("")
        s"$kindText $name$tyText$initText"
      case UntypedCompileTimeIntAlias(name, value, _) =>
        s"type $name=${MacroStableDisplay.expr(value)}"

  def templatePart(value: UntypedTemplatePart): String =
    val text = value.text
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
    value.hole match
      case Some(hole) =>
        val format = hole.format.map(value => s":$value").getOrElse("")
        s""""$text"$${${expr(hole.value)}$format}"""
      case None =>
        s""""$text""""
