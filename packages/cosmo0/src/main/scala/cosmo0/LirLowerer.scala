package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LirLowerer:
  def apply(): LirLowerer =
    new LirLowerer(LirTypeChecker())

final class LirLowerer(
    checker: LirTypeChecker,
):
  def lower(module: TypedModule): Result[LirModule] =
    val state = State(module)
    val lowered = state.lower()
    if state.diagnostics.nonEmpty then
      Result.failure(Phase.Compile, state.diagnostics)
    else
      checker.check(lowered) match
        case checked if checked.isSuccess =>
          Result.success(Phase.Compile, checked.value.get)
        case failed =>
          Result.failure(Phase.Compile, failed.diagnostics)

  private final case class DeclContext(
      typeIds: Map[String, LirDeclId],
      aliasIds: Map[(Option[String], String), LirDeclId],
      globalIds: Map[String, LirDeclId],
      functionIds: Map[(Option[String], String), LirDeclId],
      topLevelFunctions: Map[String, TypedFunction],
  ):
    def typeId(name: String): Option[LirDeclId] =
      typeIds.get(name)

    def aliasId(owner: Option[String], name: String): LirDeclId =
      aliasIds.getOrElse((owner, name), Lir.declId(qualifiedId(owner, name)))

    def globalId(name: String): Option[LirDeclId] =
      globalIds.get(name)

    def functionId(owner: Option[String], name: String): Option[LirDeclId] =
      functionIds.get((owner, name))

    def topLevelFunction(name: String): Option[TypedFunction] =
      topLevelFunctions.get(name)

  private object DeclContext:
    def from(module: TypedModule): DeclContext =
      val typeIds = mutable.LinkedHashMap.empty[String, LirDeclId]
      val aliasIds = mutable.LinkedHashMap.empty[(Option[String], String), LirDeclId]
      val globalIds = mutable.LinkedHashMap.empty[String, LirDeclId]
      val functionIds = mutable.LinkedHashMap.empty[(Option[String], String), LirDeclId]
      val topLevelFunctions = mutable.LinkedHashMap.empty[String, TypedFunction]

      module.declarations.foreach {
        case alias: TypedTypeAlias =>
          aliasIds.update((None, alias.name), Lir.declId(qualifiedId(None, alias.name)))
        case value: TypedValueDecl =>
          globalIds.update(value.name, Lir.declId(qualifiedId(None, value.name)))
        case fn: TypedFunction =>
          functionIds.update((None, fn.name), Lir.declId(qualifiedId(None, fn.name)))
          topLevelFunctions.update(fn.name, fn)
        case cls: TypedClass =>
          typeIds.update(cls.name, Lir.declId(stablePart(cls.name)))
          cls.aliases.foreach(alias =>
            aliasIds.update((Some(cls.name), alias.name), Lir.declId(qualifiedId(Some(cls.name), alias.name))),
          )
          cls.methods.foreach(method =>
            functionIds.update(
              (Some(cls.name), method.name),
              Lir.declId(qualifiedId(Some(cls.name), method.name)),
            ),
          )
        case _: TypedImport =>
      }

      DeclContext(
        typeIds.toMap,
        aliasIds.toMap,
        globalIds.toMap,
        functionIds.toMap,
        topLevelFunctions.toMap,
      )

  private final case class Binding(
      name: String,
      id: LirLocalId,
      valueType: SourceType,
      mutable: Boolean,
  )

  private final class FunctionBuilder(
      function: TypedFunction,
      context: DeclContext,
      report: (String, String, SourceSpan) => Unit,
  ):
    private val scopes = mutable.ArrayBuffer.empty[mutable.LinkedHashMap[String, Binding]]
    private val localOrdinals = mutable.LinkedHashMap.empty[String, Int]
    private val localBuffer = ListBuffer.empty[LirLocal]
    private val operations = ListBuffer.empty[LirOp]
    private var sealedTerminator: Option[LirTerminator] = None
    private var tempIndex = 0

    scopes += mutable.LinkedHashMap.empty[String, Binding]

    val params: List[LirParam] =
      function.params.map { param =>
        val id = nextLocalId(param.name)
        bind(Binding(param.name, id, param.valueType, mutable = false))
        LirParam(id, param.name, Lir.t(param.valueType))
      }

    def lower(): LirFunction =
      function.body match
        case Some(body) =>
          val result = lowerExpr(body)
          if !isTerminated then
            terminate(returnTerminator(result, function.returnType, function.span))
        case None =>
          report(
            "cosmo0.lir.lower.missing-body",
            s"function ${function.name} has no body to lower",
            function.span,
          )
          terminate(LirErrorExit("cosmo0.lir.lower.missing-body", Some(function.name)))

      LirFunction(
        context.functionId(function.owner, function.name).getOrElse(Lir.declId(qualifiedId(function.owner, function.name))),
        function.name,
        params,
        Lir.t(function.returnType),
        localBuffer.toList.sortBy(_.id.value),
        List(LirBlock(Lir.label("entry"), operations.toList, sealedTerminator.getOrElse(LirUnreachable()))),
        owner = function.owner.flatMap(context.typeId),
        sourceSignature = Some(function.signature),
      )

    private def lowerBlock(block: TypedBlock): Option[LirValue] =
      withScope {
        var result: Option[LirValue] = Some(LirUnitValue)
        block.items.foreach { item =>
          if !isTerminated then result = lowerBlockItem(item)
        }
        result
      }

    private def lowerBlockItem(item: TypedBlockItem): Option[LirValue] =
      item match
        case local: TypedLocal =>
          val init = local.init.flatMap(lowerExpr)
          val mutable = local.kind == UntypedValueKind.Var
          val binding = declareLocal(local.name, local.valueType, mutable)
          emit(LirAllocLocal(localFor(binding), init))
          bind(binding)
          Some(LirUnitValue)
        case stmt: TypedExprStmt =>
          lowerExpr(stmt.expr)
          Some(LirUnitValue)
        case expr: TypedExpr =>
          lowerExpr(expr)

    private def lowerExpr(expr: TypedExpr): Option[LirValue] =
      if isTerminated then None
      else
        expr match
          case block: TypedBlock =>
            lowerBlock(block)

          case name: TypedName =>
            lowerName(name)

          case select: TypedSelect =>
            if isFunctionType(select.valueType) then
              unsupported(select, s"method value ${select.field}")
              None
            else
              for receiver <- lowerExpr(select.receiver) yield
                val output = declareTemp(select.valueType)
                emit(LirFieldGet(output.id, receiver, select.field, Lir.t(select.valueType)))
                LirLocalRef(output.id, Lir.t(output.valueType))

          case call: TypedCall =>
            lowerCall(call)

          case assign: TypedAssign =>
            lowerAssign(assign)
            Some(LirUnitValue)

          case ret: TypedReturn =>
            val value = lowerExpr(ret.value)
            terminate(returnTerminator(value, function.returnType, ret.span))
            None

          case TypedUnitLiteral(_, _) =>
            Some(LirUnitValue)
          case TypedBoolLiteral(value, _, _) =>
            Some(LirBoolValue(value))
          case TypedIntLiteral(value, valueType, _) =>
            Some(LirIntValue(value, Lir.t(valueType)))
          case TypedFloatLiteral(value, valueType, _) =>
            Some(LirFloatValue(value, Lir.t(valueType)))
          case TypedStringLiteral(value, _, _) =>
            Some(LirStringValue(value))

          case value: TypedTypeConstructorExpr =>
            unsupported(value, s"type constructor ${value.constructedType.display}")
            None
          case value: TypedVariantConstructorExpr =>
            unsupported(value, s"variant constructor ${value.owner.display}::${value.variant}")
            None
          case value: TypedUnary =>
            unsupported(value, s"unary operator ${value.op}")
            None
          case value: TypedBinary =>
            unsupported(value, s"binary operator ${value.op}")
            None
          case value: TypedIf =>
            unsupported(value, "if expression")
            None
          case value: TypedLoop =>
            unsupported(value, "loop expression")
            None
          case value: TypedWhile =>
            unsupported(value, "while expression")
            None
          case value: TypedFor =>
            unsupported(value, "for expression")
            None
          case value: TypedMatch =>
            unsupported(value, "match expression")
            None
          case value: TypedBreak =>
            unsupported(value, "break expression")
            None
          case value: TypedContinue =>
            unsupported(value, "continue expression")
            None

    private def lowerName(name: TypedName): Option[LirValue] =
      name.path.parts match
        case valueName :: Nil =>
          resolve(valueName)
            .map(binding => LirLocalRef(binding.id, Lir.t(binding.valueType)))
            .orElse(
              context.globalId(valueName).map(id => LirGlobalRef(id, Lir.t(name.valueType))),
            )
            .orElse(
              context.topLevelFunction(valueName).flatMap(fn =>
                context.functionId(None, valueName).map(id =>
                  LirFunctionRef(id, LirCallableSignature.fromSource(fn.signature)),
                ),
              ),
            )
            .orElse {
              report(
                "cosmo0.lir.lower.unknown-name",
                s"cannot resolve typed name ${name.path.text} during lowering",
                name.span,
              )
              None
            }
        case _ =>
          report(
            "cosmo0.lir.lower.unknown-name",
            s"cannot lower qualified name ${name.path.text}",
            name.span,
          )
          None

    private def lowerCall(call: TypedCall): Option[LirValue] =
      val output = callOutput(call.valueType)
      call.callee match
        case name: TypedName if name.path.parts.length == 1 =>
          val args = call.args.flatMap(lowerExpr)
          if args.length != call.args.length then return None
          val calleeName = name.path.parts.head
          context.functionId(None, calleeName) match
            case Some(id) =>
              emit(
                LirDirectCall(
                  output.map(_.id),
                  id,
                  args,
                  LirCallableSignature.fromSource(call.signature),
                ),
              )
              callResult(output, call.valueType)
            case None =>
              unsupported(call, s"call to non-function value ${name.path.text}")
              None

        case select: TypedSelect =>
          lowerExpr(select.receiver) match
            case Some(receiver) =>
              val args = call.args.flatMap(lowerExpr)
              if args.length != call.args.length then return None
              emit(
                LirLoweredMethodCall(
                  output.map(_.id),
                  receiver,
                  select.field,
                  args,
                  LirCallableSignature.fromSource(call.signature),
                ),
              )
              callResult(output, call.valueType)
            case None =>
              None

        case constructor: TypedVariantConstructorExpr =>
          unsupported(constructor, s"variant constructor ${constructor.owner.display}::${constructor.variant}")
          None
        case constructor: TypedTypeConstructorExpr =>
          unsupported(constructor, s"type constructor ${constructor.constructedType.display}")
          None
        case other =>
          unsupported(other, "indirect function call")
          None

    private def lowerAssign(assign: TypedAssign): Unit =
      if assign.op != "=" then
        unsupported(assign, s"compound assignment ${assign.op}")
        return

      assign.target match
        case name: TypedName if name.path.parts.length == 1 =>
          lowerExpr(assign.value).foreach { loweredValue =>
            val valueName = name.path.parts.head
            resolve(valueName) match
              case Some(binding) =>
                emit(LirAssign(LirLocalPlace(binding.id, Lir.t(binding.valueType)), loweredValue))
              case None =>
                unsupported(assign.target, s"assignment to non-local ${name.path.text}")
          }

        case select: TypedSelect =>
          lowerExpr(select.receiver).foreach { receiver =>
            lowerExpr(assign.value).foreach { loweredValue =>
              emit(LirFieldSet(receiver, select.field, loweredValue))
            }
          }

        case other =>
          unsupported(other, "assignment target")

    private def returnTerminator(
        value: Option[LirValue],
        expectedType: SourceType,
        span: SourceSpan,
    ): LirTerminator =
      SourceType.dealias(expectedType) match
        case SourceType.Unit =>
          LirReturn(None)
        case _ =>
          value match
            case Some(actual) =>
              LirReturn(Some(actual))
            case None =>
              report(
                "cosmo0.lir.lower.missing-return",
                s"function ${function.name} did not produce a ${expectedType.display} return value",
                span,
              )
              LirErrorExit("cosmo0.lir.lower.missing-return", Some(function.name))

    private def callOutput(valueType: SourceType): Option[Binding] =
      SourceType.dealias(valueType) match
        case SourceType.Unit | SourceType.Never =>
          None
        case _ =>
          Some(declareTemp(valueType))

    private def callResult(output: Option[Binding], valueType: SourceType): Option[LirValue] =
      output match
        case Some(binding) =>
          Some(LirLocalRef(binding.id, Lir.t(valueType)))
        case None =>
          Some(LirUnitValue)

    private def declareLocal(
        name: String,
        valueType: SourceType,
        mutable: Boolean,
    ): Binding =
      val id = nextLocalId(name)
      val binding = Binding(name, id, valueType, mutable)
      localBuffer += localFor(binding)
      binding

    private def declareTemp(valueType: SourceType): Binding =
      val name = s"tmp$tempIndex"
      tempIndex += 1
      declareLocal(name, valueType, mutable = false)

    private def nextLocalId(name: String): LirLocalId =
      val base = stablePart(name)
      val ordinal = localOrdinals.getOrElse(base, 0)
      localOrdinals.update(base, ordinal + 1)
      if ordinal == 0 then Lir.localId(base)
      else Lir.localId(s"${base}_$ordinal")

    private def localFor(binding: Binding): LirLocal =
      LirLocal(binding.id, binding.name, Lir.t(binding.valueType), binding.mutable)

    private def bind(binding: Binding): Unit =
      scopes.last.update(binding.name, binding)

    private def resolve(name: String): Option[Binding] =
      scopes.reverseIterator.flatMap(_.get(name)).nextOption()

    private def withScope[A](body: => A): A =
      scopes += mutable.LinkedHashMap.empty[String, Binding]
      try body
      finally scopes.remove(scopes.length - 1)

    private def emit(op: LirOp): Unit =
      if !isTerminated then operations += op

    private def terminate(terminator: LirTerminator): Unit =
      if sealedTerminator.isEmpty then sealedTerminator = Some(terminator)

    private def isTerminated: Boolean =
      sealedTerminator.nonEmpty

    private def unsupported(node: TypedNode, construct: String): Unit =
      report(
        "cosmo0.lir.lower.unsupported-expression",
        s"declaration lowering does not support $construct yet",
        node.span,
      )

    private def isFunctionType(valueType: SourceType): Boolean =
      SourceType.dealias(valueType) match
        case SourceType.Function(_, _) => true
        case _                         => false

  private final class State(module: TypedModule):
    private val errors = ListBuffer.empty[Diagnostic]
    private val context = DeclContext.from(module)

    def diagnostics: List[Diagnostic] =
      errors.toList

    def lower(): LirModule =
      LirModule(
        moduleName(module.source),
        module.declarations.flatMap(lowerDecl).sortBy(_.id.value),
      )

    private def lowerDecl(declaration: TypedDecl): List[LirDeclaration] =
      declaration match
        case _: TypedImport =>
          Nil

        case alias: TypedTypeAlias =>
          List(
            LirTypeAliasDecl(
              context.aliasId(None, alias.name),
              alias.name,
              Lir.t(alias.target),
            ),
          )

        case value: TypedValueDecl =>
          List(
            LirGlobal(
              context.globalId(value.name).getOrElse(Lir.declId(qualifiedId(None, value.name))),
              value.name,
              Lir.t(value.valueType),
              mutable = value.kind == UntypedValueKind.Var,
              initializer = value.init.flatMap(staticValue),
            ),
          )

        case fn: TypedFunction =>
          List(lowerFunction(fn))

        case cls: TypedClass =>
          val typeDecl =
            LirTypeDecl(
              context.typeId(cls.name).getOrElse(Lir.declId(stablePart(cls.name))),
              cls.name,
              fields = cls.fields.map(field =>
                LirField(
                  field.name,
                  Lir.t(field.valueType),
                  mutable = field.kind == UntypedValueKind.Var,
                ),
              ),
              variants = cls.variants.map(variant =>
                LirVariant(
                  variant.name,
                  variant.fields.map(field =>
                    LirVariantPayload(
                      field.name,
                      Lir.t(field.valueType),
                    ),
                  ),
                ),
              ),
            )
          val aliases = cls.aliases.map(alias =>
            LirTypeAliasDecl(
              context.aliasId(Some(cls.name), alias.name),
              alias.name,
              Lir.t(alias.target),
            ),
          )
          typeDecl :: aliases ::: cls.methods.map(lowerFunction)

    private def lowerFunction(function: TypedFunction): LirFunction =
      FunctionBuilder(function, context, error).lower()

    private def staticValue(expr: TypedExpr): Option[LirValue] =
      expr match
        case TypedUnitLiteral(_, _) =>
          Some(LirUnitValue)
        case TypedBoolLiteral(value, _, _) =>
          Some(LirBoolValue(value))
        case TypedIntLiteral(value, valueType, _) =>
          Some(LirIntValue(value, Lir.t(valueType)))
        case TypedFloatLiteral(value, valueType, _) =>
          Some(LirFloatValue(value, Lir.t(valueType)))
        case TypedStringLiteral(value, _, _) =>
          Some(LirStringValue(value))
        case name: TypedName if name.path.parts.length == 1 =>
          val valueName = name.path.parts.head
          context.globalId(valueName)
            .map(id => LirGlobalRef(id, Lir.t(name.valueType)))
            .orElse(
              context.topLevelFunction(valueName).flatMap(fn =>
                context.functionId(None, valueName).map(id =>
                  LirFunctionRef(id, LirCallableSignature.fromSource(fn.signature)),
                ),
              ),
            )
        case other =>
          error(
            "cosmo0.lir.lower.unsupported-initializer",
            s"global ${other.valueType.display} initializer is not a static LIR value",
            other.span,
          )
          None

    private def error(
        code: String,
        message: String,
        span: SourceSpan,
    ): Unit =
      errors += Diagnostic(
        Phase.Compile,
        DiagnosticSeverity.Error,
        code,
        message,
        Some(span),
      )

  private def moduleName(source: SourceFile): String =
    val raw =
      source.name match
        case "" | "<memory>" => "memory"
        case other           => other.split('/').lastOption.getOrElse(other)
    stablePart(raw.stripSuffix(".cos"))

  private def qualifiedId(owner: Option[String], name: String): String =
    (owner.toList :+ name).map(stablePart).mkString(".")

  private def stablePart(value: String): String =
    val builder = new StringBuilder
    value.foreach { char =>
      if char.isLetterOrDigit || char == '_' then builder.append(char)
      else if builder.nonEmpty && builder.last != '_' then builder.append('_')
    }
    val raw = builder.toString.stripPrefix("_").stripSuffix("_")
    if raw.nonEmpty then raw else "anonymous"
