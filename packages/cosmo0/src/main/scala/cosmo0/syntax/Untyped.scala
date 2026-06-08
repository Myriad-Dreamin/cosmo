package cosmo0

/** Base node for the cosmo0 AST after elaboration and before type checking.
  *
  * `Untyped` means names and source type annotations are still source-shaped:
  * declarations may omit annotations, aliases have not been expanded, and
  * expressions do not yet carry `SourceType`.
  *
  * Example source:
  *
  * {{{
  * def add(left: i32, right: i32): i32 =
  *   left + right
  *
  * val answer = add(20, 22)
  * }}}
  *
  * Representative untyped shape:
  *
  * {{{
  * UntypedFunction(
  *   "add",
  *   params = List(left: i32, right: i32),
  *   retTy = Some(i32),
  *   body = Some(UntypedBinary("+", left, right, span)),
  *   span = span,
  * )
  *
  * UntypedValueDecl(
  *   Val,
  *   "answer",
  *   ty = None,
  *   init = Some(UntypedCall(add, List(20, 22), span)),
  *   span = span,
  * )
  * }}}
  *
  * Inference contract with `MlttTyper`:
  *
  *   - Value and local declaration types are explicit annotations when present,
  *     otherwise initializer types are inferred.
  *   - Function parameters require explicit types; return types default to
  *     Unit.
  *   - Integer and float literals may be typed from an expected type; otherwise
  *     they default to i32 and f64 respectively.
  *   - Blocks take the type of their final expression, or Unit when empty or
  *     ending in a statement.
  *   - If and match expressions require compatible branch result types.
  *   - Assignment, call arguments, returns, and declaration initializers are
  *     checked with the MLTT-backed source type assignability relation.
  */
sealed trait UntypedNode:
  def span: SourceSpan

/** A complete elaborated source file.
  *
  * `cIncludes` and `cppImports` are collected from file-level
  * decorators/imports and kept beside declarations so later lowering and
  * backend stages can emit required foreign headers.
  */
final case class UntypedModule(
    source: SourceFile,
    decls: List[UntypedDecl],
    span: SourceSpan,
    cIncludes: List[SourceCInclude] = Nil,
    cppImports: List[SourceCppNamespaceImport] = Nil,
) extends UntypedNode

/** Top-level declaration accepted by the cosmo0 checker. */
sealed trait UntypedDecl extends UntypedNode:
  def name: String
  def vis: UntypedVisibility

/** Source visibility as observed during elaboration. */
enum UntypedVisibility:
  case Public, Private

/** Member allowed inside a class, trait, or impl body. */
sealed trait UntypedClassMember extends UntypedNode

/** Item that can occur in a block before the final typed value is known. */
sealed trait UntypedBlockItem extends UntypedNode

/** Statement item in a block. */
sealed trait UntypedStmt extends UntypedBlockItem

/** Expression item in a block. */
sealed trait UntypedExpr extends UntypedBlockItem

/** Pattern accepted in a match arm. */
sealed trait UntypedPattern extends UntypedNode

/** Source type syntax before name resolution, alias expansion, and arity
  * checks.
  */
sealed trait UntypedType extends UntypedNode

/** Distinguishes immutable `val` bindings from mutable `var` bindings. */
enum UntypedValueKind:
  case Val, Var

/** A source path such as `foo`, `pkg::Name`, or an imported foreign alias path.
  */
final case class UntypedPath(
    parts: List[String],
    span: SourceSpan,
) extends UntypedNode:
  def text: String = parts.mkString("::")

/** Source import. `dest` is the optional alias/path used as the local binding.
  */
final case class UntypedImport(
    path: UntypedPath,
    dest: Option[UntypedPath],
    span: SourceSpan,
    vis: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl:
  def name: String = dest.orElse(Some(path)).fold("<import>")(_.text)

/** C++ namespace import produced from a trusted C++ header import form. */
final case class UntypedCppNamespaceImport(
    value: SourceCppNamespaceImport,
    vis: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl:
  def name: String = value.alias
  def span: SourceSpan = value.span

/** Concrete user type with fields, aliases, variants, and methods. */
final case class UntypedClass(
    name: String,
    members: List[UntypedClassMember],
    span: SourceSpan,
    vis: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl

/** Trait declaration. Method signatures have no bodies at this stage. */
final case class UntypedTrait(
    name: String,
    methods: List[UntypedFunction],
    span: SourceSpan,
    vis: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl

/** Function or method declaration.
  *
  * `retTy = None` means the typer will use Unit. `body = None` is valid for
  * trait methods and trusted extern declarations.
  */
final case class UntypedFunction(
    name: String,
    params: List[UntypedParam],
    retTy: Option[UntypedType],
    body: Option[UntypedExpr],
    span: SourceSpan,
    extern: Option[SourceExternBinding] = None,
    vis: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl
    with UntypedClassMember

/** Top-level or class-level value declaration. The typer resolves the declared
  * type, or infers it from `init` when allowed.
  */
final case class UntypedValueDecl(
    kind: UntypedValueKind,
    name: String,
    ty: Option[UntypedType],
    init: Option[UntypedExpr],
    span: SourceSpan,
    vis: UntypedVisibility = UntypedVisibility.Public,
) extends UntypedDecl
    with UntypedClassMember

/** Type alias. Generic aliases store parameter names and an untyped target
  * until `MlttTyper` substitutes actual type arguments.
  */
final case class UntypedTypeAlias(
    name: String,
    target: UntypedType,
    span: SourceSpan,
    vis: UntypedVisibility = UntypedVisibility.Public,
    tyParams: List[String] = Nil,
) extends UntypedDecl
    with UntypedClassMember

/** Trait implementation. It is private module structure and does not emit a
  * top-level typed declaration directly.
  */
final case class UntypedImpl(
    traitName: UntypedPath,
    target: UntypedPath,
    members: List[UntypedClassMember],
    span: SourceSpan,
    vis: UntypedVisibility = UntypedVisibility.Private,
) extends UntypedDecl:
  def name: String = s"${traitName.text} for ${target.text}"

/** Enum-style class variant, optionally with typed payload fields. */
final case class UntypedVariant(
    name: String,
    fields: List[UntypedVariantField],
    span: SourceSpan,
) extends UntypedClassMember

/** Payload field for a class variant. Unnamed fields are addressed by position
  * during constructor signature construction.
  */
final case class UntypedVariantField(
    name: Option[String],
    ty: UntypedType,
    span: SourceSpan,
) extends UntypedNode

/** Function parameter. Parameters require explicit types during type checking;
  * default values are checked against the resolved parameter type.
  */
final case class UntypedParam(
    name: String,
    ty: Option[UntypedType],
    default: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedNode

/** Named type syntax, for example `i32`, `String`, `Self`, or `foo.Bar`. */
final case class UntypedNamedType(
    path: UntypedPath,
    span: SourceSpan,
) extends UntypedType

/** Generic type syntax, for example `Vec[i32]` or `Map[String, i32]`. */
final case class UntypedAppliedType(
    base: UntypedPath,
    args: List[UntypedType],
    span: SourceSpan,
) extends UntypedType

/** Reference type syntax. `mut = true` corresponds to `&mut T` or `RefMut[T]`.
  */
final case class UntypedRefType(
    target: UntypedType,
    mut: Boolean,
    span: SourceSpan,
) extends UntypedType

/** Local value in a block. Locals follow the same inference rule as top-level
  * values: annotation first, otherwise initializer type.
  */
final case class UntypedLocal(
    kind: UntypedValueKind,
    name: String,
    ty: Option[UntypedType],
    init: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedStmt

/** Local compile-time integer alias used by the source-level CTE smoke path.
  * The alias has no runtime storage; the typer evaluates `value` while checking
  * the block and expands later references to integer literals.
  */
final case class UntypedCompileTimeIntAlias(
    name: String,
    value: UntypedExpr,
    span: SourceSpan,
) extends UntypedStmt

/** Expression in statement position, usually produced by a trailing semicolon.
  */
final case class UntypedExprStmt(
    expr: UntypedExpr,
    span: SourceSpan,
) extends UntypedStmt

/** Block expression. The typer evaluates items left-to-right and gives the
  * block the type of its last expression, or Unit if no value remains.
  */
final case class UntypedBlock(
    items: List[UntypedBlockItem],
    span: SourceSpan,
) extends UntypedExpr

/** Name expression. Only single-segment value names resolve as local/global
  * values; paths in type positions use `UntypedType`.
  */
final case class UntypedName(
    path: UntypedPath,
    span: SourceSpan,
) extends UntypedExpr

/** Field or method selection. The receiver type determines whether `field`
  * resolves as a data field, method, standard descriptor method, or foreign
  * method.
  */
final case class UntypedSelect(
    recv: UntypedExpr,
    field: String,
    span: SourceSpan,
) extends UntypedExpr

/** Variant constructor expression written with type-selection syntax. */
final case class UntypedVariantConstructor(
    owner: UntypedType,
    variant: String,
    span: SourceSpan,
) extends UntypedExpr

/** Direct type-constructor expression for constructible standard or foreign
  * types, for example `Vec[i32](...)`.
  */
final case class UntypedTypeConstructor(
    ty: UntypedType,
    span: SourceSpan,
) extends UntypedExpr

/** Function, method, constructor, or function-value call. Argument types are
  * checked against the resolved callable signature.
  */
final case class UntypedCall(
    callee: UntypedExpr,
    args: List[UntypedExpr],
    span: SourceSpan,
) extends UntypedExpr

/** Assignment expression. `op` is either `=` or a compound numeric assignment
  * operator such as `+=`.
  */
final case class UntypedAssign(
    target: UntypedExpr,
    value: UntypedExpr,
    op: String,
    span: SourceSpan,
) extends UntypedExpr

/** Unary operator expression. Supported typing includes `!Bool`, numeric
  * negation, immutable reference formation, and reference dereference.
  */
final case class UntypedUnary(
    op: String,
    expr: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

/** Binary operator expression. Boolean operators require Bool operands;
  * equality requires same types; ordering and arithmetic require matching
  * numeric operands.
  */
final case class UntypedBinary(
    op: String,
    left: UntypedExpr,
    right: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

/** Conditional expression. The condition is checked as Bool; branch types must
  * match when an else branch is present.
  */
final case class UntypedIf(
    cond: UntypedExpr,
    thenExp: UntypedExpr,
    elseExp: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedExpr

/** Canonical loop condition after elaboration. Source `loop`, `while`, and
  * restricted `for` forms share this representation so later phases can
  * preserve loop structure instead of recovering it from source-specific nodes.
  */
sealed trait UntypedLoopCondition:
  def span: SourceSpan

object UntypedLoopCondition:
  final case class Always(span: SourceSpan) extends UntypedLoopCondition
  final case class SourceCondition(value: UntypedExpr)
      extends UntypedLoopCondition:
    def span: SourceSpan = value.span
  final case class ForEach(
      name: String,
      iter: UntypedExpr,
      span: SourceSpan,
  ) extends UntypedLoopCondition

/** Canonical loop expression. The shape is `prologue; while condition { body;
  * epilogue }`; source loops currently elaborate with empty prologue and
  * epilogue slots.
  */
final case class UntypedLoop(
    prologue: List[UntypedExpr],
    condition: UntypedLoopCondition,
    body: UntypedExpr,
    epilogue: List[UntypedExpr],
    span: SourceSpan,
) extends UntypedExpr

/** Match expression. Pattern typing is driven by the scrutinee type; arm body
  * result types must match when bodies are present.
  */
final case class UntypedMatch(
    scrut: UntypedExpr,
    arms: List[UntypedMatchArm],
    span: SourceSpan,
) extends UntypedExpr

/** One match arm. A missing body is treated as a statement-only arm and does
  * not contribute to the match expression result type.
  */
final case class UntypedMatchArm(
    pat: UntypedPattern,
    body: Option[UntypedExpr],
    span: SourceSpan,
) extends UntypedNode

/** Return expression. The enclosing function return type supplies the expected
  * type for `value`; the expression itself has type Never after checking.
  */
final case class UntypedReturn(
    value: UntypedExpr,
    span: SourceSpan,
) extends UntypedExpr

/** Loop control expressions type as Never after checking. */
final case class UntypedBreak(
    span: SourceSpan,
) extends UntypedExpr

/** Loop control expressions type as Never after checking. */
final case class UntypedContinue(
    span: SourceSpan,
) extends UntypedExpr

/** Boolean literal expression and pattern. */
final case class UntypedBoolLiteral(
    value: Boolean,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

/** Integer literal expression and pattern. Without an expected integer type,
  * the typer defaults it to i32.
  */
final case class UntypedIntLiteral(
    value: BigInt,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

/** Float literal expression and pattern. Without an expected float type, the
  * typer defaults it to f64.
  */
final case class UntypedFloatLiteral(
    value: BigDecimal,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

/** String literal expression and pattern. */
final case class UntypedStringLiteral(
    value: String,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

/** ASCII literal. The typer validates that it contains exactly one ASCII code
  * point and lowers it to a u8 integer literal.
  */
final case class UntypedAsciiLiteral(
    value: String,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

/** Rune literal. The typer validates that it contains exactly one Unicode code
  * point and lowers it to a u32 integer literal.
  */
final case class UntypedRuneLiteral(
    value: String,
    span: SourceSpan,
) extends UntypedExpr
    with UntypedPattern

/** Unit literal, usually produced by an empty statement position. */
final case class UntypedUnitLiteral(
    span: SourceSpan,
) extends UntypedExpr

/** `_` pattern, typed as the expected scrutinee payload type. */
final case class UntypedWildcardPattern(
    span: SourceSpan,
) extends UntypedPattern

/** Binding pattern. The typer defines the binding with the expected payload
  * type in the arm scope.
  */
final case class UntypedBindingPattern(
    name: String,
    span: SourceSpan,
) extends UntypedPattern

/** Variant pattern. The constructor signature determines the expected type for
  * each payload sub-pattern.
  */
final case class UntypedVariantPattern(
    ctor: UntypedExpr,
    args: List[UntypedPattern],
    span: SourceSpan,
) extends UntypedPattern
