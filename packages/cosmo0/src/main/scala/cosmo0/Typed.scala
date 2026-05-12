package cosmo0

sealed trait TypedNode:
  def span: SourceSpan

final case class TypedModule(
    source: SourceFile,
    declarations: List[TypedDecl],
    span: SourceSpan,
) extends TypedNode

sealed trait TypedDecl extends TypedNode:
  def name: String

sealed trait TypedClassMember extends TypedNode

sealed trait TypedBlockItem extends TypedNode

sealed trait TypedStmt extends TypedBlockItem

sealed trait TypedExpr extends TypedBlockItem:
  def valueType: SourceType

sealed trait TypedPattern extends TypedNode

final case class TypedImport(
    path: UntypedPath,
    dest: Option[UntypedPath],
    span: SourceSpan,
) extends TypedDecl:
  def name: String = dest.orElse(Some(path)).fold("<import>")(_.text)

final case class TypedClass(
    name: String,
    fields: List[TypedValueDecl],
    aliases: List[TypedTypeAlias],
    variants: List[TypedVariant],
    methods: List[TypedFunction],
    span: SourceSpan,
) extends TypedDecl

final case class TypedFunction(
    name: String,
    params: List[TypedParam],
    returnType: SourceType,
    body: Option[TypedExpr],
    signature: CallableSignature,
    owner: Option[String],
    span: SourceSpan,
    externBinding: Option[SourceExternBinding] = None,
) extends TypedDecl
    with TypedClassMember

final case class TypedValueDecl(
    kind: UntypedValueKind,
    name: String,
    valueType: SourceType,
    init: Option[TypedExpr],
    span: SourceSpan,
) extends TypedDecl
    with TypedClassMember

final case class TypedTypeAlias(
    name: String,
    target: SourceType,
    span: SourceSpan,
) extends TypedDecl
    with TypedClassMember

final case class TypedVariant(
    name: String,
    fields: List[TypedVariantField],
    span: SourceSpan,
) extends TypedClassMember

final case class TypedVariantField(
    name: Option[String],
    valueType: SourceType,
    span: SourceSpan,
) extends TypedNode

final case class TypedParam(
    name: String,
    valueType: SourceType,
    default: Option[TypedExpr],
    span: SourceSpan,
) extends TypedNode

final case class TypedLocal(
    kind: UntypedValueKind,
    name: String,
    valueType: SourceType,
    init: Option[TypedExpr],
    span: SourceSpan,
) extends TypedStmt

final case class TypedExprStmt(
    expr: TypedExpr,
    span: SourceSpan,
) extends TypedStmt

final case class TypedBlock(
    items: List[TypedBlockItem],
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedName(
    path: UntypedPath,
    valueType: SourceType,
    mutableBinding: Boolean,
    mutationAllowed: Boolean,
    span: SourceSpan,
) extends TypedExpr

final case class TypedTypeConstructorExpr(
    constructedType: SourceType,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedSelect(
    receiver: TypedExpr,
    field: String,
    valueType: SourceType,
    mutableBinding: Boolean,
    mutationAllowed: Boolean,
    span: SourceSpan,
) extends TypedExpr

final case class TypedVariantConstructorExpr(
    owner: SourceType,
    variant: String,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedCall(
    callee: TypedExpr,
    args: List[TypedExpr],
    valueType: SourceType,
    signature: CallableSignature,
    span: SourceSpan,
) extends TypedExpr

final case class TypedAssign(
    target: TypedExpr,
    value: TypedExpr,
    op: String,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedUnary(
    op: String,
    expr: TypedExpr,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedBinary(
    op: String,
    left: TypedExpr,
    right: TypedExpr,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedIf(
    cond: TypedExpr,
    thenBranch: TypedExpr,
    elseBranch: Option[TypedExpr],
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedLoop(
    body: TypedExpr,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedWhile(
    cond: TypedExpr,
    body: TypedExpr,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedFor(
    name: String,
    itemType: SourceType,
    iter: TypedExpr,
    body: TypedExpr,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedMatch(
    scrutinee: TypedExpr,
    arms: List[TypedMatchArm],
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedMatchArm(
    pattern: TypedPattern,
    body: Option[TypedExpr],
    span: SourceSpan,
) extends TypedNode

final case class TypedReturn(
    value: TypedExpr,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedBreak(
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedContinue(
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedBoolLiteral(
    value: Boolean,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

final case class TypedIntLiteral(
    value: BigInt,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

final case class TypedFloatLiteral(
    value: BigDecimal,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

final case class TypedStringLiteral(
    value: String,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

final case class TypedUnitLiteral(
    valueType: SourceType,
    span: SourceSpan,
) extends TypedExpr

final case class TypedWildcardPattern(
    expectedType: SourceType,
    span: SourceSpan,
) extends TypedPattern

final case class TypedBindingPattern(
    name: String,
    valueType: SourceType,
    span: SourceSpan,
) extends TypedPattern

final case class TypedVariantPattern(
    constructor: TypedExpr,
    args: List[TypedPattern],
    valueType: SourceType,
    span: SourceSpan,
) extends TypedPattern
