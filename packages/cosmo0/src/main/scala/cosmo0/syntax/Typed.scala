package cosmo0

/** Base node for the cosmo0 AST after source type checking.
  *
  * Typed nodes are the handoff from `tyck/mltt/Typer.scala` to lowering. Every
  * expression has a `SourceType`, declarations have resolved signatures, and
  * mutability information is attached to l-values that may be assigned through.
  *
  * Example source:
  *
  * {{{
  * var x: i32 = 1
  * x += 2
  * x
  * }}}
  *
  * Representative typed shape:
  *
  * {{{
  * TypedLocal(Var, "x", SourceType.I32, Some(TypedIntLiteral(1, i32, span)))
  * TypedExprStmt(TypedAssign(TypedName("x", i32, true, true), 2, "+=", Unit))
  * TypedName("x", SourceType.I32, mutBinding = true, mutAllowed = true)
  * }}}
  *
  * Type inference rules represented in this tree:
  *
  *   - Literal defaults are i32 for integers and f64 for floats unless an
  *     expected numeric type flows in from context.
  *   - Blocks take the type of their final expression, or Unit if no final
  *     value remains.
  *   - If and match expressions carry the common branch type, or Error after a
  *     reported mismatch.
  *   - Calls carry both the callee result type (`ty`) and the resolved callable
  *     surface (`sig`).
  *   - `return`, `break`, and `continue` carry Never so they can inhabit any
  *     expected type after diagnostics.
  *   - Assignment and declaration initialization use `SourceType.assignable`.
  */
sealed trait TypedNode:
  def span: SourceSpan

/** Checked source module. */
final case class TypedModule(
    source: SourceFile,
    decls: List[TypedDecl],
    span: SourceSpan,
    cIncludes: List[SourceCInclude] = Nil,
    cppImports: List[SourceCppNamespaceImport] = Nil,
) extends TypedNode:
  /** Stable summary used to compare checker artifacts without serializing the
    * whole typed tree.
    */
  def checkerArtifactSummary: String =
    val declarationSummary =
      decls.map(checkerDeclarationSummary).mkString(",")
    s"decls=${decls.length}|$declarationSummary"

  private def checkerDeclarationSummary(declaration: TypedDecl): String =
    declaration match
      case value: TypedValueDecl =>
        s"val:${value.name}:${value.ty.display}"
      case fn: TypedFunction =>
        s"def:${fn.name}:${fn.retTy.display}"
      case cls: TypedClass =>
        s"class:${cls.name}:${cls.fields.length}:${cls.methods.length}"
      case alias: TypedTypeAlias =>
        s"type:${alias.name}:${alias.target.display}"
      case importDecl: TypedImport =>
        s"import:${importDecl.name}"
      case cppImport: TypedCppNamespaceImport =>
        s"cpp-import:${cppImport.name}"

/** Checked top-level declaration. */
sealed trait TypedDecl extends TypedNode:
  def name: String

/** Checked class member. */
sealed trait TypedClassMember extends TypedNode

/** Checked item that can occur inside a block. */
sealed trait TypedBlockItem extends TypedNode

/** Checked statement item. */
sealed trait TypedStmt extends TypedBlockItem

/** Checked expression. `ty` is the type inferred or checked by the typer.
  */
sealed trait TypedExpr extends TypedBlockItem:
  def ty: SourceType

/** Checked pattern. Patterns are typed against the scrutinee type rather than
  * inferred independently.
  */
sealed trait TypedPattern extends TypedNode

/** Checked import. Imports remain source-level declarations for downstream
  * package and lowering stages.
  */
final case class TypedImport(
    path: UntypedPath,
    dest: Option[UntypedPath],
    span: SourceSpan,
) extends TypedDecl:
  def name: String = dest.orElse(Some(path)).fold("<import>")(_.text)

/** Checked C++ namespace import. The alias is in scope as a foreign namespace
  * value and type root.
  */
final case class TypedCppNamespaceImport(
    value: SourceCppNamespaceImport,
    span: SourceSpan,
) extends TypedDecl:
  def name: String = value.alias

/** Checked class with resolved field types, aliases, enum variants, and
  * methods.
  */
final case class TypedClass(
    name: String,
    fields: List[TypedValueDecl],
    aliases: List[TypedTypeAlias],
    variants: List[TypedVariant],
    methods: List[TypedFunction],
    span: SourceSpan,
) extends TypedDecl

/** Checked function or method. `sig` is the callable surface after receiver
  * normalization; `params` still include the source `self` parameter when one
  * was declared.
  */
final case class TypedFunction(
    name: String,
    params: List[TypedParam],
    retTy: SourceType,
    body: Option[TypedExpr],
    sig: CallableSignature,
    owner: Option[String],
    span: SourceSpan,
    extern: Option[SourceExternBinding] = None,
) extends TypedDecl
    with TypedClassMember

/** Checked value or field declaration. */
final case class TypedValueDecl(
    kind: UntypedValueKind,
    name: String,
    ty: SourceType,
    init: Option[TypedExpr],
    span: SourceSpan,
) extends TypedDecl
    with TypedClassMember

/** Checked type alias. `target` is resolved, while `tyParams` preserves the
  * generic alias parameter list for display and package metadata.
  */
final case class TypedTypeAlias(
    name: String,
    target: SourceType,
    span: SourceSpan,
    tyParams: List[String] = Nil,
) extends TypedDecl
    with TypedClassMember

/** Checked enum-style class variant. */
final case class TypedVariant(
    name: String,
    fields: List[TypedVariantField],
    span: SourceSpan,
) extends TypedClassMember

/** Checked variant payload field. */
final case class TypedVariantField(
    name: Option[String],
    ty: SourceType,
    span: SourceSpan,
) extends TypedNode

/** Checked function parameter. Default expressions have already been checked
  * against `ty`.
  */
final case class TypedParam(
    name: String,
    ty: SourceType,
    default: Option[TypedExpr],
    span: SourceSpan,
) extends TypedNode

/** Checked local declaration. */
final case class TypedLocal(
    kind: UntypedValueKind,
    name: String,
    ty: SourceType,
    init: Option[TypedExpr],
    span: SourceSpan,
) extends TypedStmt

/** Checked expression in statement position. */
final case class TypedExprStmt(
    expr: TypedExpr,
    span: SourceSpan,
) extends TypedStmt

/** Checked block expression with the inferred final value type. */
final case class TypedBlock(
    items: List[TypedBlockItem],
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked name reference.
  *
  * `mutBinding` means the binding itself may be assigned. `mutAllowed` means
  * mutation through the value is allowed, for example through a mutable
  * reference or mutable field receiver.
  */
final case class TypedName(
    path: UntypedPath,
    ty: SourceType,
    mutBinding: Boolean,
    mutAllowed: Boolean,
    span: SourceSpan,
) extends TypedExpr

/** Callable constructor expression for a resolved source type. */
final case class TypedTypeConstructorExpr(
    constructedTy: SourceType,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Foreign qualified symbol selected through an imported C++ namespace alias.
  */
final case class TypedForeignQualifiedName(
    root: SourceCppNamespaceImport,
    suffix: List[String],
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked field or method selection. For field selections, the mutability
  * flags describe whether assignment through the selection is legal.
  */
final case class TypedSelect(
    recv: TypedExpr,
    field: String,
    ty: SourceType,
    mutBinding: Boolean,
    mutAllowed: Boolean,
    span: SourceSpan,
) extends TypedExpr

/** Checked variant constructor expression. If the variant has payload fields,
  * `ty` is the constructor function type; otherwise it is the owner type.
  */
final case class TypedVariantConstructorExpr(
    owner: SourceType,
    variant: String,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked call with the exact callable signature used for argument checking.
  */
final case class TypedCall(
    callee: TypedExpr,
    args: List[TypedExpr],
    ty: SourceType,
    sig: CallableSignature,
    span: SourceSpan,
) extends TypedExpr

/** Checked assignment. Assignments type as Unit. */
final case class TypedAssign(
    target: TypedExpr,
    value: TypedExpr,
    op: String,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked unary operator expression. */
final case class TypedUnary(
    op: String,
    expr: TypedExpr,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked binary operator expression. */
final case class TypedBinary(
    op: String,
    left: TypedExpr,
    right: TypedExpr,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked conditional expression. Without an else branch, the result type is
  * Unit.
  */
final case class TypedIf(
    cond: TypedExpr,
    thenExp: TypedExpr,
    elseExp: Option[TypedExpr],
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked infinite loop expression. */
final case class TypedLoop(
    body: TypedExpr,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked while loop expression. */
final case class TypedWhile(
    cond: TypedExpr,
    body: TypedExpr,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked for loop. `itemTy` is inferred from the iterator expression. */
final case class TypedFor(
    name: String,
    itemTy: SourceType,
    iter: TypedExpr,
    body: TypedExpr,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked match expression. */
final case class TypedMatch(
    scrut: TypedExpr,
    arms: List[TypedMatchArm],
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked match arm. */
final case class TypedMatchArm(
    pat: TypedPattern,
    body: Option[TypedExpr],
    span: SourceSpan,
) extends TypedNode

/** Checked return expression. It has type Never after validating the returned
  * value against the enclosing function return type.
  */
final case class TypedReturn(
    value: TypedExpr,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked break expression. It has type Never. */
final case class TypedBreak(
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked continue expression. It has type Never. */
final case class TypedContinue(
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked Bool literal and Bool pattern. */
final case class TypedBoolLiteral(
    value: Boolean,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

/** Checked integer literal or pattern. The type is an expected integer type
  * when one was available, otherwise i32.
  */
final case class TypedIntLiteral(
    value: BigInt,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

/** Checked floating literal or pattern. The type is an expected float type when
  * one was available, otherwise f64.
  */
final case class TypedFloatLiteral(
    value: BigDecimal,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

/** Checked String literal and String pattern. */
final case class TypedStringLiteral(
    value: String,
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr
    with TypedPattern

/** Checked Unit literal. */
final case class TypedUnitLiteral(
    ty: SourceType,
    span: SourceSpan,
) extends TypedExpr

/** Checked wildcard pattern. `ty` is the matched payload type. */
final case class TypedWildcardPattern(
    ty: SourceType,
    span: SourceSpan,
) extends TypedPattern

/** Checked binding pattern. The binding is introduced with `ty` in the match
  * arm scope.
  */
final case class TypedBindingPattern(
    name: String,
    ty: SourceType,
    span: SourceSpan,
) extends TypedPattern

/** Checked variant pattern. `ty` is the constructor return type. */
final case class TypedVariantPattern(
    ctor: TypedExpr,
    args: List[TypedPattern],
    ty: SourceType,
    span: SourceSpan,
) extends TypedPattern
