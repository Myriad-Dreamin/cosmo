package cosmo0

sealed trait Cosmo0UntypedNode:
  def span: Cosmo0SourceSpan

final case class Cosmo0UntypedModule(
    source: Cosmo0SourceFile,
    declarations: List[Cosmo0UntypedDecl],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedNode

sealed trait Cosmo0UntypedDecl extends Cosmo0UntypedNode:
  def name: String

sealed trait Cosmo0UntypedClassMember extends Cosmo0UntypedNode

sealed trait Cosmo0UntypedBlockItem extends Cosmo0UntypedNode

sealed trait Cosmo0UntypedStmt extends Cosmo0UntypedBlockItem

sealed trait Cosmo0UntypedExpr extends Cosmo0UntypedBlockItem

sealed trait Cosmo0UntypedPattern extends Cosmo0UntypedNode

sealed trait Cosmo0UntypedType extends Cosmo0UntypedNode

enum Cosmo0UntypedValueKind:
  case Val, Var

final case class Cosmo0UntypedPath(
    parts: List[String],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedNode:
  def text: String = parts.mkString("::")

final case class Cosmo0UntypedImport(
    path: Cosmo0UntypedPath,
    dest: Option[Cosmo0UntypedPath],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedDecl:
  def name: String = dest.orElse(Some(path)).fold("<import>")(_.text)

final case class Cosmo0UntypedClass(
    name: String,
    members: List[Cosmo0UntypedClassMember],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedDecl

final case class Cosmo0UntypedFunction(
    name: String,
    params: List[Cosmo0UntypedParam],
    returnType: Option[Cosmo0UntypedType],
    body: Option[Cosmo0UntypedExpr],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedDecl
    with Cosmo0UntypedClassMember

final case class Cosmo0UntypedValueDecl(
    kind: Cosmo0UntypedValueKind,
    name: String,
    valueType: Option[Cosmo0UntypedType],
    init: Option[Cosmo0UntypedExpr],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedDecl
    with Cosmo0UntypedClassMember

final case class Cosmo0UntypedTypeAlias(
    name: String,
    target: Cosmo0UntypedType,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedDecl
    with Cosmo0UntypedClassMember

final case class Cosmo0UntypedVariant(
    name: String,
    fields: List[Cosmo0UntypedVariantField],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedClassMember

final case class Cosmo0UntypedVariantField(
    name: Option[String],
    valueType: Cosmo0UntypedType,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedNode

final case class Cosmo0UntypedParam(
    name: String,
    valueType: Option[Cosmo0UntypedType],
    default: Option[Cosmo0UntypedExpr],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedNode

final case class Cosmo0UntypedNamedType(
    path: Cosmo0UntypedPath,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedType

final case class Cosmo0UntypedAppliedType(
    base: Cosmo0UntypedPath,
    args: List[Cosmo0UntypedType],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedType

final case class Cosmo0UntypedRefType(
    target: Cosmo0UntypedType,
    mutable: Boolean,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedType

final case class Cosmo0UntypedLocal(
    kind: Cosmo0UntypedValueKind,
    name: String,
    valueType: Option[Cosmo0UntypedType],
    init: Option[Cosmo0UntypedExpr],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedStmt

final case class Cosmo0UntypedExprStmt(
    expr: Cosmo0UntypedExpr,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedStmt

final case class Cosmo0UntypedBlock(
    items: List[Cosmo0UntypedBlockItem],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedName(
    path: Cosmo0UntypedPath,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedSelect(
    receiver: Cosmo0UntypedExpr,
    field: String,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedVariantConstructor(
    owner: Cosmo0UntypedType,
    variant: String,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedCall(
    callee: Cosmo0UntypedExpr,
    args: List[Cosmo0UntypedExpr],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedAssign(
    target: Cosmo0UntypedExpr,
    value: Cosmo0UntypedExpr,
    op: String,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedUnary(
    op: String,
    expr: Cosmo0UntypedExpr,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedBinary(
    op: String,
    left: Cosmo0UntypedExpr,
    right: Cosmo0UntypedExpr,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedIf(
    cond: Cosmo0UntypedExpr,
    thenBranch: Cosmo0UntypedExpr,
    elseBranch: Option[Cosmo0UntypedExpr],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedLoop(
    body: Cosmo0UntypedExpr,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedWhile(
    cond: Cosmo0UntypedExpr,
    body: Cosmo0UntypedExpr,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedFor(
    name: String,
    iter: Cosmo0UntypedExpr,
    body: Cosmo0UntypedExpr,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedMatch(
    scrutinee: Cosmo0UntypedExpr,
    arms: List[Cosmo0UntypedMatchArm],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedMatchArm(
    pattern: Cosmo0UntypedPattern,
    body: Option[Cosmo0UntypedExpr],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedNode

final case class Cosmo0UntypedReturn(
    value: Cosmo0UntypedExpr,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedBreak(
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedContinue(
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedBoolLiteral(
    value: Boolean,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr
    with Cosmo0UntypedPattern

final case class Cosmo0UntypedIntLiteral(
    value: BigInt,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr
    with Cosmo0UntypedPattern

final case class Cosmo0UntypedFloatLiteral(
    value: BigDecimal,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr
    with Cosmo0UntypedPattern

final case class Cosmo0UntypedStringLiteral(
    value: String,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr
    with Cosmo0UntypedPattern

final case class Cosmo0UntypedUnitLiteral(
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedExpr

final case class Cosmo0UntypedWildcardPattern(
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedPattern

final case class Cosmo0UntypedBindingPattern(
    name: String,
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedPattern

final case class Cosmo0UntypedVariantPattern(
    constructor: Cosmo0UntypedExpr,
    args: List[Cosmo0UntypedPattern],
    span: Cosmo0SourceSpan,
) extends Cosmo0UntypedPattern
