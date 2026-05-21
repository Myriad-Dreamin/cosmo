package cosmo0

final case class LirModule(
    name: String,
    declarations: List[LirDeclaration],
    cIncludes: List[SourceCInclude] = Nil,
    cppNamespaceImports: List[SourceCppNamespaceImport] = Nil,
)

sealed trait LirDeclaration:
  def id: LirDeclId
  def name: String

final case class LirDeclId(value: String):
  require(value.nonEmpty, "LIR declaration identifiers must be non-empty")
  override def toString: String = s"@$value"

final case class LirLocalId(value: String):
  require(value.nonEmpty, "LIR local identifiers must be non-empty")
  override def toString: String = s"%$value"

final case class LirLabel(value: String):
  require(value.nonEmpty, "LIR labels must be non-empty")
  override def toString: String = s"^$value"

final case class LirTypeRef(source: SourceType):
  def display: String = source.display

final case class LirCallableSignature(
    params: List[LirTypeRef],
    returnType: LirTypeRef,
    sourceSignature: Option[CallableSignature] = None,
)

object LirCallableSignature:
  def fromSource(signature: CallableSignature): LirCallableSignature =
    LirCallableSignature(
      signature.params.map(param => LirTypeRef(param.valueType)),
      LirTypeRef(signature.returnType),
      Some(signature),
    )

final case class LirParam(
    id: LirLocalId,
    name: String,
    valueType: LirTypeRef,
    mutationAllowed: Boolean = false,
)

final case class LirLocal(
    id: LirLocalId,
    name: String,
    valueType: LirTypeRef,
    mutable: Boolean = false,
    mutationAllowed: Boolean = false,
)

final case class LirFunction(
    id: LirDeclId,
    name: String,
    params: List[LirParam],
    returnType: LirTypeRef,
    locals: List[LirLocal],
    blocks: List[LirBlock],
    owner: Option[LirDeclId] = None,
    sourceSignature: Option[CallableSignature] = None,
    externBinding: Option[LirExternBinding] = None,
) extends LirDeclaration:
  def signature: LirCallableSignature =
    LirCallableSignature(
      params.map(_.valueType),
      returnType,
      sourceSignature,
    )

final case class LirGlobal(
    id: LirDeclId,
    name: String,
    valueType: LirTypeRef,
    mutable: Boolean,
    initializer: Option[LirValue] = None,
    mutationAllowed: Boolean = false,
) extends LirDeclaration

final case class LirTypeAliasDecl(
    id: LirDeclId,
    name: String,
    target: LirTypeRef,
    typeParams: List[String] = Nil,
) extends LirDeclaration

final case class LirTypeDecl(
    id: LirDeclId,
    name: String,
    fields: List[LirField],
    variants: List[LirVariant] = Nil,
) extends LirDeclaration

final case class LirField(
    name: String,
    valueType: LirTypeRef,
    mutable: Boolean = false,
)

final case class LirVariant(
    name: String,
    payload: List[LirVariantPayload] = Nil,
)

final case class LirVariantPayload(
    name: Option[String],
    valueType: LirTypeRef,
)

final case class LirBlock(
    label: LirLabel,
    operations: List[LirOp],
    terminator: LirTerminator,
)

sealed trait LirValue:
  def valueType: LirTypeRef

case object LirUnitValue extends LirValue:
  val valueType: LirTypeRef = LirTypeRef(SourceType.Unit)

final case class LirBoolValue(value: Boolean) extends LirValue:
  val valueType: LirTypeRef = LirTypeRef(SourceType.Bool)

final case class LirIntValue(
    value: BigInt,
    valueType: LirTypeRef = LirTypeRef(SourceType.I32),
) extends LirValue

final case class LirFloatValue(
    value: BigDecimal,
    valueType: LirTypeRef = LirTypeRef(SourceType.F64),
) extends LirValue

final case class LirStringValue(value: String) extends LirValue:
  val valueType: LirTypeRef = LirTypeRef(SourceType.String)

final case class LirCharValue(value: Char) extends LirValue:
  val valueType: LirTypeRef = LirTypeRef(SourceType.Char)

final case class LirLocalRef(
    id: LirLocalId,
    valueType: LirTypeRef,
) extends LirValue

final case class LirGlobalRef(
    id: LirDeclId,
    valueType: LirTypeRef,
) extends LirValue

final case class LirFunctionRef(
    id: LirDeclId,
    signature: LirCallableSignature,
) extends LirValue:
  def valueType: LirTypeRef =
    LirTypeRef(
      SourceType.Function(
        signature.params.map(_.source),
        signature.returnType.source,
      ),
    )

final case class LirBorrowValue(
    value: LirValue,
    mutable: Boolean,
) extends LirValue:
  def valueType: LirTypeRef =
    LirTypeRef(SourceType.Ref(value.valueType.source, mutable))

final case class LirDerefValue(
    value: LirValue,
    valueType: LirTypeRef,
) extends LirValue

final case class LirFieldRef(
    receiver: LirValue,
    field: String,
    valueType: LirTypeRef,
) extends LirValue

sealed trait LirPlace:
  def valueType: LirTypeRef

final case class LirLocalPlace(
    id: LirLocalId,
    valueType: LirTypeRef,
) extends LirPlace

final case class LirFieldPlace(
    receiver: LirValue,
    field: String,
    valueType: LirTypeRef,
) extends LirPlace

final case class LirDescriptorRef(
    name: String,
    args: List[LirTypeRef] = Nil,
)

sealed trait LirOp

final case class LirAllocLocal(
    local: LirLocal,
    init: Option[LirValue] = None,
) extends LirOp

final case class LirAssign(
    target: LirPlace,
    value: LirValue,
) extends LirOp

final case class LirFieldGet(
    output: LirLocalId,
    receiver: LirValue,
    field: String,
    valueType: LirTypeRef,
) extends LirOp

final case class LirFieldSet(
    receiver: LirValue,
    field: String,
    value: LirValue,
) extends LirOp

final case class LirDirectCall(
    output: Option[LirLocalId],
    callee: LirDeclId,
    args: List[LirValue],
    signature: LirCallableSignature,
) extends LirOp

final case class LirLoweredMethodCall(
    output: Option[LirLocalId],
    receiver: LirValue,
    method: String,
    args: List[LirValue],
    signature: LirCallableSignature,
) extends LirOp

final case class LirDescriptorIntrinsic(
    output: Option[LirLocalId],
    descriptor: LirDescriptorRef,
    name: String,
    args: List[LirValue],
    resultType: Option[LirTypeRef],
) extends LirOp

final case class LirFieldInit(
    name: String,
    value: LirValue,
)

final case class LirConstructType(
    output: LirLocalId,
    owner: LirTypeRef,
    fields: List[LirFieldInit],
) extends LirOp

final case class LirConstructVariant(
    output: LirLocalId,
    owner: LirTypeRef,
    variant: String,
    payload: List[LirValue],
) extends LirOp

final case class LirReadVariantTag(
    output: LirLocalId,
    scrutinee: LirValue,
    owner: LirTypeRef,
) extends LirOp

final case class LirCheckVariantTag(
    output: LirLocalId,
    scrutinee: LirValue,
    owner: LirTypeRef,
    variant: String,
) extends LirOp

final case class LirReadVariantPayload(
    output: LirLocalId,
    scrutinee: LirValue,
    variant: String,
    index: Int,
    valueType: LirTypeRef,
) extends LirOp

sealed trait LirTerminator

final case class LirReturn(value: Option[LirValue]) extends LirTerminator

final case class LirBranch(target: LirLabel) extends LirTerminator

final case class LirCondBranch(
    condition: LirValue,
    trueTarget: LirLabel,
    falseTarget: LirLabel,
) extends LirTerminator

final case class LirUnreachable(reason: Option[String] = None) extends LirTerminator

final case class LirErrorExit(
    code: String,
    message: Option[String] = None,
) extends LirTerminator

object Lir:
  def declId(value: String): LirDeclId = LirDeclId(value)

  def localId(value: String): LirLocalId = LirLocalId(value)

  def label(value: String): LirLabel = LirLabel(value)

  def t(source: SourceType): LirTypeRef = LirTypeRef(source)

  def signature(params: List[SourceType], returnType: SourceType): LirCallableSignature =
    LirCallableSignature(
      params.map(t),
      t(returnType),
    )

  def param(
      name: String,
      valueType: SourceType,
      id: String = "",
      mutationAllowed: Boolean = false,
  ): LirParam =
    LirParam(localId(stableId(id, name)), name, t(valueType), mutationAllowed)

  def local(
      name: String,
      valueType: SourceType,
      mutable: Boolean = false,
      id: String = "",
      mutationAllowed: Boolean = false,
  ): LirLocal =
    LirLocal(localId(stableId(id, name)), name, t(valueType), mutable, mutationAllowed)

  def ref(id: String, valueType: SourceType): LirLocalRef =
    LirLocalRef(localId(id), t(valueType))

  def localPlace(id: String, valueType: SourceType): LirLocalPlace =
    LirLocalPlace(localId(id), t(valueType))

  def int(value: Int, valueType: SourceType = SourceType.I32): LirIntValue =
    LirIntValue(BigInt(value), t(valueType))

  def bool(value: Boolean): LirBoolValue =
    LirBoolValue(value)

  def string(value: String): LirStringValue =
    LirStringValue(value)

  def block(
      label: String,
      operations: List[LirOp] = Nil,
      terminator: LirTerminator = LirUnreachable(),
  ): LirBlock =
    LirBlock(LirLabel(label), operations, terminator)

  def function(
      name: String,
      params: List[LirParam],
      returnType: SourceType,
      locals: List[LirLocal],
      blocks: List[LirBlock],
      id: String = "",
      owner: Option[LirDeclId] = None,
      sourceSignature: Option[CallableSignature] = None,
      externBinding: Option[LirExternBinding] = None,
  ): LirFunction =
    LirFunction(
      declId(stableId(id, name)),
      name,
      params,
      t(returnType),
      locals,
      blocks,
      owner,
      sourceSignature,
      externBinding,
    )

  private def stableId(value: String, fallback: String): String =
    if value.nonEmpty then value else fallback

object LirDebugRenderer:
  def renderModule(module: LirModule): String =
    val declarations = module.declarations.sortBy(_.id.value).map(renderDeclaration(_, 2))
    if declarations.isEmpty then s"module ${module.name} {\n}"
    else s"module ${module.name} {\n${declarations.mkString("\n\n")}\n}"

  def renderFunction(function: LirFunction): String =
    renderFunction(function, 0)

  private def renderDeclaration(declaration: LirDeclaration, indent: Int): String =
    declaration match
      case function: LirFunction =>
        renderFunction(function, indent)
      case global: LirGlobal =>
        val mutability = if global.mutable then " var" else ""
        val init = global.initializer.fold("")(value => s" = ${renderValue(value)}")
        line(indent, s"global${mutability} ${global.id} ${global.name}: ${renderType(global.valueType)}$init")
      case alias: LirTypeAliasDecl =>
        line(indent, s"typealias ${alias.id} ${alias.name} = ${renderType(alias.target)}")
      case ty: LirTypeDecl =>
        renderTypeDecl(ty, indent)

  private def renderTypeDecl(ty: LirTypeDecl, indent: Int): String =
    val fields = ty.fields.sortBy(_.name).map { field =>
      val mutability = if field.mutable then " var" else ""
      line(indent + 2, s"field${mutability} ${field.name}: ${renderType(field.valueType)}")
    }
    val variants = ty.variants.sortBy(_.name).map { variant =>
      val payload = variant.payload
        .map(payload =>
          payload.name match
            case Some(name) => s"$name: ${renderType(payload.valueType)}"
            case None       => renderType(payload.valueType)
        )
        .mkString(", ")
      line(indent + 2, s"variant ${variant.name}($payload)")
    }
    val body = (fields ++ variants).mkString("\n")
    if body.isEmpty then line(indent, s"type ${ty.id} ${ty.name} {}")
    else s"${line(indent, s"type ${ty.id} ${ty.name} {")}\n$body\n${line(indent, "}")}"

  private def renderFunction(function: LirFunction, indent: Int): String =
    val params = function.params.map(renderParam).mkString(", ")
    val owner = function.owner.fold("")(owner => s" owner $owner")
    val extern = function.externBinding.fold("") { binding =>
      s""" extern ${binding.abi} ${quote(binding.symbol.cppName)}"""
    }
    val header =
      line(
        indent,
        s"fn ${function.id} ${function.name}($params) -> ${renderType(function.returnType)}$owner$extern {",
      )
    val localLines =
      function.locals.sortBy(_.id.value).map(local => line(indent + 2, renderLocal(local)))
    val blockLines =
      function.blocks.sortBy(_.label.value).map(block => renderBlock(block, indent + 2))
    val body =
      (if localLines.isEmpty then blockLines else localLines ++ List("") ++ blockLines).mkString("\n")
    if body.isEmpty then s"$header\n${line(indent, "}")}"
    else s"$header\n$body\n${line(indent, "}")}"

  private def renderBlock(block: LirBlock, indent: Int): String =
    val operations = block.operations.map(op => line(indent + 2, renderOp(op)))
    val terminator = line(indent + 2, renderTerminator(block.terminator))
    s"${line(indent, s"${block.label}:")}\n${(operations :+ terminator).mkString("\n")}"

  private def renderParam(param: LirParam): String =
    s"${param.id} ${param.name}: ${renderType(param.valueType)}"

  private def renderLocal(local: LirLocal): String =
    val mutability = if local.mutable then " mut" else ""
    s"local ${local.id} ${local.name}: ${renderType(local.valueType)}$mutability"

  private def renderOp(op: LirOp): String =
    op match
      case LirAllocLocal(local, init) =>
        val initText = init.fold("")(value => s" = ${renderValue(value)}")
        s"alloc ${renderLocal(local).stripPrefix("local ")}$initText"
      case LirAssign(target, value) =>
        s"assign ${renderPlace(target)} = ${renderValue(value)}"
      case LirFieldGet(output, receiver, field, valueType) =>
        s"$output = field_get ${renderValue(receiver)}.$field: ${renderType(valueType)}"
      case LirFieldSet(receiver, field, value) =>
        s"field_set ${renderValue(receiver)}.$field = ${renderValue(value)}"
      case LirDirectCall(output, callee, args, signature) =>
        s"${renderOutput(output)}call $callee(${renderArgs(args)}) ${renderSignatureResult(signature)}"
      case LirLoweredMethodCall(output, receiver, method, args, signature) =>
        s"${renderOutput(output)}method_call ${renderValue(receiver)}.$method(${renderArgs(args)}) ${renderSignatureResult(signature)}"
      case LirDescriptorIntrinsic(output, descriptor, name, args, resultType) =>
        val result = resultType.fold("Unit")(renderType)
        s"${renderOutput(output)}descriptor ${renderDescriptor(descriptor)}::$name(${renderArgs(args)}) -> $result"
      case LirConstructType(output, owner, fields) =>
        val args = fields.map(field => s"${field.name} = ${renderValue(field.value)}").mkString(", ")
        s"$output = construct ${renderType(owner)}($args)"
      case LirConstructVariant(output, owner, variant, payload) =>
        s"$output = variant ${renderType(owner)}::$variant(${renderArgs(payload)})"
      case LirReadVariantTag(output, scrutinee, owner) =>
        s"$output = variant_tag ${renderValue(scrutinee)}: ${renderType(owner)}"
      case LirCheckVariantTag(output, scrutinee, owner, variant) =>
        s"$output = variant_is ${renderValue(scrutinee)}: ${renderType(owner)}::$variant"
      case LirReadVariantPayload(output, scrutinee, variant, index, valueType) =>
        s"$output = variant_payload ${renderValue(scrutinee)}.$variant[$index]: ${renderType(valueType)}"

  private def renderTerminator(terminator: LirTerminator): String =
    terminator match
      case LirReturn(value) =>
        value.fold("return")(value => s"return ${renderValue(value)}")
      case LirBranch(target) =>
        s"branch $target"
      case LirCondBranch(condition, trueTarget, falseTarget) =>
        s"cond_branch ${renderValue(condition)} ? $trueTarget : $falseTarget"
      case LirUnreachable(reason) =>
        reason.fold("unreachable")(value => s"unreachable ${quote(value)}")
      case LirErrorExit(code, message) =>
        message match
          case Some(value) => s"error ${quote(code)} ${quote(value)}"
          case None        => s"error ${quote(code)}"

  private def renderPlace(place: LirPlace): String =
    place match
      case LirLocalPlace(id, _) =>
        id.toString
      case LirFieldPlace(receiver, field, _) =>
        s"${renderValue(receiver)}.$field"

  private def renderValue(value: LirValue): String =
    value match
      case LirUnitValue =>
        "()"
      case LirBoolValue(value) =>
        value.toString
      case LirIntValue(value, valueType) =>
        s"$value:${renderType(valueType)}"
      case LirFloatValue(value, valueType) =>
        s"$value:${renderType(valueType)}"
      case LirStringValue(value) =>
        quote(value)
      case LirCharValue(value) =>
        s"'${escapeChar(value)}'"
      case LirLocalRef(id, _) =>
        id.toString
      case LirGlobalRef(id, _) =>
        id.toString
      case LirFunctionRef(id, _) =>
        s"fn $id"
      case LirBorrowValue(value, true) =>
        s"&mut ${renderValue(value)}"
      case LirBorrowValue(value, false) =>
        s"&${renderValue(value)}"
      case LirDerefValue(value, _) =>
        s"*${renderValue(value)}"
      case LirFieldRef(receiver, field, _) =>
        s"${renderValue(receiver)}.$field"

  private def renderType(valueType: LirTypeRef): String =
    valueType.display

  private def renderDescriptor(descriptor: LirDescriptorRef): String =
    if descriptor.args.isEmpty then descriptor.name
    else s"${descriptor.name}[${descriptor.args.map(renderType).mkString(", ")}]"

  private def renderSignatureResult(signature: LirCallableSignature): String =
    s"-> ${renderType(signature.returnType)}"

  private def renderOutput(output: Option[LirLocalId]): String =
    output.fold("")(id => s"$id = ")

  private def renderArgs(args: List[LirValue]): String =
    args.map(renderValue).mkString(", ")

  private def line(indent: Int, text: String): String =
    (" " * indent) + text

  private def quote(value: String): String =
    s""""${escapeString(value)}""""

  private def escapeString(value: String): String =
    val builder = new StringBuilder
    value.foreach { char =>
      builder.append(escapeChar(char))
    }
    builder.toString

  private def escapeChar(char: Char): String =
    char match
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\'' => "\\'"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case other if other.isControl =>
        "\\u%04x".format(other.toInt)
      case other =>
        other.toString
