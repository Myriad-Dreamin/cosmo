package cosmo0

sealed trait UntypedNode:
  def span: SourceSpan

final case class UntypedModule(
    source: SourceFile,
    declarations: List[UntypedDecl],
    span: SourceSpan,
    cIncludes: List[SourceCInclude] = Nil,
    cppNamespaceImports: List[SourceCppNamespaceImport] = Nil,
) extends UntypedNode

sealed trait UntypedDecl extends UntypedNode:
  def name: String
  def visibility: UntypedVisibility

enum UntypedVisibility:
  case Public, Private

sealed trait UntypedClassMember extends UntypedNode

sealed trait UntypedBlockItem extends UntypedNode

sealed trait UntypedStmt extends UntypedBlockItem

sealed trait UntypedExpr extends UntypedBlockItem

sealed trait UntypedPattern extends UntypedNode

sealed trait UntypedType extends UntypedNode

enum UntypedValueKind:
  case Val, Var

final case class UntypedPath(
    parts: List[String],
    span: SourceSpan,
) extends UntypedNode:
  def text: String = parts.mkString("::")

final case class UntypedImport(
    path: UntypedPath,
    dest: Option[UntypedPath],
    span: SourceSpan,
    visibility: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl:
  def name: String = dest.orElse(Some(path)).fold("<import>")(_.text)

final case class UntypedCppNamespaceImport(
    value: SourceCppNamespaceImport,
    visibility: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl:
  def name: String = value.alias
  def span: SourceSpan = value.span

final case class UntypedClass(
    name: String,
    members: List[UntypedClassMember],
    span: SourceSpan,
    visibility: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl

final case class UntypedTrait(
    name: String,
    methods: List[UntypedFunction],
    span: SourceSpan,
    visibility: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl

final case class UntypedFunction(
    name: String,
    params: List[UntypedParam],
    returnType: Option[UntypedType],
    body: Option[UntypedExpr],
    span: SourceSpan,
    externBinding: Option[SourceExternBinding] = None,
    visibility: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl
    with UntypedClassMember

final case class UntypedValueDecl(
    kind: UntypedValueKind,
    name: String,
    valueType: Option[UntypedType],
    init: Option[UntypedExpr],
    span: SourceSpan,
    visibility: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl
    with UntypedClassMember

final case class UntypedTypeAlias(
    name: String,
    target: UntypedType,
    span: SourceSpan,
    visibility: UntypedVisibility = UntypedVisibility.Public,
    typeParams: List[String] = Nil,
) extends UntypedDecl
    with UntypedClassMember

final case class UntypedImpl(
    traitName: UntypedPath,
    target: UntypedPath,
    members: List[UntypedClassMember],
    span: SourceSpan,
    visibility: UntypedVisibility = UntypedVisibility.Private,
) extends UntypedDecl:
  def name: String = s"${traitName.text} for ${target.text}"

final case class UntypedVariant(
    name: String,
    fields: List[UntypedVariantField],
    span: SourceSpan,
) extends UntypedClassMember

final case class UntypedVariantField(
    name: Option[String],
    valueType: UntypedType,
    span: SourceSpan,
) extends UntypedNode

final case class UntypedParam(
    name: String,
    valueType: Option[UntypedType],
    default: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedNode

final case class UntypedNamedType(
    path: UntypedPath,
    span: SourceSpan,
) extends UntypedType

final case class UntypedAppliedType(
    base: UntypedPath,
    args: List[UntypedType],
    span: SourceSpan,
) extends UntypedType

final case class UntypedRefType(
    target: UntypedType,
    mutable: Boolean,
    span: SourceSpan,
) extends UntypedType

final case class UntypedLocal(
    kind: UntypedValueKind,
    name: String,
    valueType: Option[UntypedType],
    init: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedStmt

final case class UntypedExprStmt(
    expr: UntypedExpr,
    span: SourceSpan,
) extends UntypedStmt

final case class UntypedBlock(
    items: List[UntypedBlockItem],
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedName(
    path: UntypedPath,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedSelect(
    receiver: UntypedExpr,
    field: String,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedVariantConstructor(
    owner: UntypedType,
    variant: String,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedTypeConstructor(
    valueType: UntypedType,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedCall(
    callee: UntypedExpr,
    args: List[UntypedExpr],
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedAssign(
    target: UntypedExpr,
    value: UntypedExpr,
    op: String,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedUnary(
    op: String,
    expr: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedBinary(
    op: String,
    left: UntypedExpr,
    right: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedIf(
    cond: UntypedExpr,
    thenBranch: UntypedExpr,
    elseBranch: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedLoop(
    body: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedWhile(
    cond: UntypedExpr,
    body: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedFor(
    name: String,
    iter: UntypedExpr,
    body: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedMatch(
    scrutinee: UntypedExpr,
    arms: List[UntypedMatchArm],
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedMatchArm(
    pattern: UntypedPattern,
    body: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedNode

final case class UntypedReturn(
    value: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedBreak(
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedContinue(
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedBoolLiteral(
    value: Boolean,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

final case class UntypedIntLiteral(
    value: BigInt,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

final case class UntypedFloatLiteral(
    value: BigDecimal,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

final case class UntypedStringLiteral(
    value: String,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

final case class UntypedAsciiLiteral(
    value: String,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

final case class UntypedRuneLiteral(
    value: String,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

final case class UntypedUnitLiteral(
    span: SourceSpan,
) extends UntypedExpr

final case class UntypedWildcardPattern(
    span: SourceSpan,
) extends UntypedPattern

final case class UntypedBindingPattern(
    name: String,
    span: SourceSpan,
) extends UntypedPattern

final case class UntypedVariantPattern(
    constructor: UntypedExpr,
    args: List[UntypedPattern],
    span: SourceSpan,
) extends UntypedPattern
