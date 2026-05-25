package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LirLowerer:
  def apply(module: TypedModule): LirLowerer =
    new LirLowerer(module, StandardGenericDescriptors.all)

final class LirLowerer(
    module: TypedModule,
    descriptors: Map[String, StandardGenericDescriptor] =
      StandardGenericDescriptors.all,
):
  def lower(): Result[LirModule] =
    val lowered = lowerModule()
    if diagnostics.nonEmpty then Result.failure(Phase.Compile, diagnostics)
    else
      LirTypeChecker(lowered, descriptors).check() match
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
      val aliasIds =
        mutable.LinkedHashMap.empty[(Option[String], String), LirDeclId]
      val globalIds = mutable.LinkedHashMap.empty[String, LirDeclId]
      val functionIds =
        mutable.LinkedHashMap.empty[(Option[String], String), LirDeclId]
      val topLevelFunctions = mutable.LinkedHashMap.empty[String, TypedFunction]

      module.decls.foreach {
        case alias: TypedTypeAlias =>
          aliasIds.update(
            (None, alias.name),
            Lir.declId(qualifiedId(None, alias.name)),
          )
        case value: TypedValueDecl =>
          globalIds.update(
            value.name,
            Lir.declId(qualifiedId(None, value.name)),
          )
        case fn: TypedFunction =>
          functionIds.update(
            (None, fn.name),
            Lir.declId(qualifiedId(None, fn.name)),
          )
          topLevelFunctions.update(fn.name, fn)
        case cls: TypedClass =>
          typeIds.update(cls.name, Lir.declId(stablePart(cls.name)))
          cls.aliases.foreach(alias =>
            aliasIds.update(
              (Some(cls.name), alias.name),
              Lir.declId(qualifiedId(Some(cls.name), alias.name)),
            ),
          )
          cls.methods.foreach(method =>
            functionIds.update(
              (Some(cls.name), method.name),
              Lir.declId(qualifiedId(Some(cls.name), method.name)),
            ),
          )
        case _: TypedImport             =>
        case _: TypedCppNamespaceImport =>
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
      ty: SourceType,
      mutable: Boolean,
      mutAllowed: Boolean,
  )

  private final class FunctionBuilder(
      function: TypedFunction,
      context: DeclContext,
      report: (String, String, SourceSpan) => Unit,
      useSyntheticExtern: String => LirDeclId,
  ):
    private final class OpenBlock(val label: LirLabel):
      val operations: ListBuffer[LirOp] = ListBuffer.empty
      var terminator: Option[LirTerminator] = None

    private final case class LoopTargets(
        breakTarget: LirLabel,
        continueTarget: LirLabel,
    )

    private sealed trait PatternCondition
    private case object AlwaysPattern extends PatternCondition
    private final case class ConditionalPattern(condition: LirValue)
        extends PatternCondition
    private final case class MutableReceiver(
        value: LirValue,
        copyBack: Option[() => Unit],
    )

    private val scopes =
      mutable.ArrayBuffer.empty[mutable.LinkedHashMap[String, Binding]]
    private val localOrdinals = mutable.LinkedHashMap.empty[String, Int]
    private val labelOrdinals = mutable.LinkedHashMap.empty[String, Int]
    private val localBuffer = ListBuffer.empty[LirLocal]
    private val blockBuffer = ListBuffer.empty[OpenBlock]
    private val loopStack = mutable.ArrayBuffer.empty[LoopTargets]
    private var tempIndex = 0
    private var current: OpenBlock = newBlock(Lir.label("entry"))

    scopes += mutable.LinkedHashMap.empty[String, Binding]

    val params: List[LirParam] =
      function.params.map { param =>
        val id = nextLocalId(param.name)
        val mutAllowed = mutationCapability(param.ty)
        bind(
          Binding(
            param.name,
            id,
            param.ty,
            mutable = false,
            mutAllowed,
          ),
        )
        LirParam(id, param.name, Lir.t(param.ty), mutAllowed)
      }

    def lower(): LirFunction =
      function.body match
        case Some(body) =>
          val result = lowerExpr(body)
          if !isTerminated then
            terminate(
              returnTerminator(result, function.retTy, function.span),
            )
        case None =>
          report(
            "cosmo0.lir.lower.missing-body",
            s"function ${function.name} has no body to lower",
            function.span,
          )
          terminate(
            LirErrorExit("cosmo0.lir.lower.missing-body", Some(function.name)),
          )

      LirFunction(
        context
          .functionId(function.owner, function.name)
          .getOrElse(Lir.declId(qualifiedId(function.owner, function.name))),
        function.name,
        params,
        Lir.t(function.retTy),
        localBuffer.toList.sortBy(_.id.value),
        blockBuffer.toList.map(block =>
          LirBlock(
            block.label,
            block.operations.toList,
            block.terminator.getOrElse(LirUnreachable()),
          ),
        ),
        owner = function.owner.flatMap(context.typeId),
        sourceSignature = Some(function.sig),
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
          val binding = declareLocal(local.name, local.ty, mutable)
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
            if isFunctionType(select.ty) then
              unsupported(select, s"method value ${select.field}")
              None
            else
              for recv <- lowerExpr(select.recv) yield
                val output = declareTemp(
                  select.ty,
                  mutAllowed = Some(select.mutAllowed),
                )
                emit(
                  LirFieldGet(
                    output.id,
                    recv,
                    select.field,
                    Lir.t(select.ty),
                  ),
                )
                LirLocalRef(output.id, Lir.t(output.ty))

          case call: TypedCall =>
            lowerCall(call)

          case assign: TypedAssign =>
            lowerAssign(assign)
            Some(LirUnitValue)

          case ret: TypedReturn =>
            val value = lowerExpr(ret.value)
            terminate(returnTerminator(value, function.retTy, ret.span))
            None

          case TypedUnitLiteral(_, _) =>
            Some(LirUnitValue)
          case TypedBoolLiteral(value, _, _) =>
            Some(LirBoolValue(value))
          case TypedIntLiteral(value, ty, _) =>
            Some(LirIntValue(value, Lir.t(ty)))
          case TypedFloatLiteral(value, ty, _) =>
            Some(LirFloatValue(value, Lir.t(ty)))
          case TypedStringLiteral(value, _, _) =>
            Some(LirStringValue(value))

          case value: TypedTypeConstructorExpr =>
            unsupported(
              value,
              s"type constructor ${value.constructedTy.display}",
            )
            None
          case value: TypedForeignQualifiedName =>
            unsupported(
              value,
              s"C++ foreign symbol ${value.root.alias}::${value.suffix.mkString("::")}",
            )
            None
          case value: TypedVariantConstructorExpr =>
            lowerVariantValue(value)
          case value: TypedUnary =>
            lowerUnary(value)
          case value: TypedBinary =>
            lowerBinary(value)
          case value: TypedIf =>
            lowerIf(value)
          case value: TypedLoop =>
            lowerLoop(value)
          case value: TypedWhile =>
            lowerWhile(value)
          case value: TypedFor =>
            lowerFor(value)
          case value: TypedMatch =>
            lowerMatch(value)
          case value: TypedBreak =>
            lowerBreak(value)
          case value: TypedContinue =>
            lowerContinue(value)

    private def lowerName(name: TypedName): Option[LirValue] =
      name.path.parts match
        case valueName :: Nil =>
          resolve(valueName)
            .map(binding => LirLocalRef(binding.id, Lir.t(binding.ty)))
            .orElse(
              context
                .globalId(valueName)
                .map(id => LirGlobalRef(id, Lir.t(name.ty))),
            )
            .orElse(
              context
                .topLevelFunction(valueName)
                .flatMap(fn =>
                  context
                    .functionId(None, valueName)
                    .map(id =>
                      LirFunctionRef(
                        id,
                        LirCallableSignature.fromSource(fn.sig),
                      ),
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
      val output = callOutput(call.ty)
      call.callee match
        case name: TypedName if name.path.parts.length == 1 =>
          lowerCallArgs(call) match
            case None => None
            case Some(args) =>
              val calleeName = name.path.parts.head
              context.functionId(None, calleeName) match
                case Some(id) =>
                  emit(
                    LirDirectCall(
                      output.map(_.id),
                      id,
                      args,
                      LirCallableSignature.fromSource(call.sig),
                    ),
                  )
                  callResult(output, call.ty)
                case None =>
                  if TrustedExternAbi.isTrustedSourceName(calleeName) then
                    emit(
                      LirDirectCall(
                        output.map(_.id),
                        useSyntheticExtern(calleeName),
                        args,
                        LirCallableSignature.fromSource(call.sig),
                      ),
                    )
                    callResult(output, call.ty)
                  else
                    unsupported(
                      call,
                      s"call to non-function value ${name.path.text}",
                    )
                    None

        case select: TypedSelect =>
          lowerCallReceiver(
            select.recv,
            call.sig.receiver.exists(_.mutable),
          ) match
            case Some(receiver) =>
              lowerCallArgs(call) match
                case None => None
                case Some(args) =>
                  descriptorRefForMethod(
                    receiver.value.valueType.source,
                    select.field,
                  ) match
                    case Some(descriptor) =>
                      val descriptorReceiver = descriptorReceiverArg(
                        receiver.value,
                      )
                      emit(
                        LirDescriptorIntrinsic(
                          output.map(_.id),
                          descriptor,
                          select.field,
                          descriptorReceiver :: args,
                          Some(Lir.t(call.ty)),
                        ),
                      )
                      receiver.copyBack.foreach(copyBack => copyBack())
                      callResult(output, call.ty)
                    case None
                        if isDescriptorBacked(
                          receiver.value.valueType.source,
                        ) =>
                      unsupportedDescriptor(
                        call,
                        descriptorName(receiver.value.valueType.source)
                          .getOrElse("<unknown>"),
                        select.field,
                      )
                      None
                    case None =>
                      emit(
                        LirLoweredMethodCall(
                          output.map(_.id),
                          receiver.value,
                          select.field,
                          args,
                          LirCallableSignature.fromSource(call.sig),
                        ),
                      )
                      receiver.copyBack.foreach(copyBack => copyBack())
                      callResult(output, call.ty)
            case None =>
              None

        case constructor: TypedVariantConstructorExpr =>
          lowerCallArgs(call) match
            case None => None
            case Some(args) =>
              output match
                case Some(binding) =>
                  emit(
                    LirConstructVariant(
                      binding.id,
                      Lir.t(constructor.owner),
                      constructor.variant,
                      args,
                    ),
                  )
                  callResult(output, call.ty)
                case None =>
                  report(
                    "cosmo0.lir.lower.invalid-variant-constructor",
                    s"variant constructor ${constructor.owner.display}::${constructor.variant} has no LIR output",
                    constructor.span,
                  )
                  None
        case constructor: TypedTypeConstructorExpr =>
          lowerCallArgs(call) match
            case None => None
            case Some(args) =>
              SourceType.dealias(constructor.constructedTy) match
                case SourceType.User(_) =>
                  lowerUserConstructor(
                    constructor,
                    output,
                    args,
                    call.ty,
                    call.sig,
                  )
                case _ =>
                  lowerDescriptorConstructor(
                    constructor,
                    output,
                    args,
                    call.ty,
                  )
        case other =>
          unsupported(other, "indirect function call")
          None

    private def lowerCallReceiver(
        expr: TypedExpr,
        mutableReceiver: Boolean,
    ): Option[MutableReceiver] =
      if !mutableReceiver then
        lowerExpr(expr).map(value => MutableReceiver(value, None))
      else
        expr match
          case select: TypedSelect if !isFunctionType(select.ty) =>
            lowerExpr(select.recv).map { owner =>
              MutableReceiver(
                LirFieldRef(owner, select.field, Lir.t(select.ty)),
                None,
              )
            }
          case _ =>
            lowerExpr(expr).map(value => MutableReceiver(value, None))

    private def lowerCallArgs(call: TypedCall): Option[List[LirValue]] =
      lowerArgs(call.args, call.sig.params.map(_.valueType))

    private def lowerArgs(
        args: List[TypedExpr],
        params: List[SourceType],
    ): Option[List[LirValue]] =
      if args.length != params.length then sequence(args.map(lowerExpr))
      else
        sequence(args.zip(params).map { case (arg, expected) =>
          lowerArgument(arg, expected)
        })

    private def lowerArgument(
        arg: TypedExpr,
        expected: SourceType,
    ): Option[LirValue] =
      lowerExpr(arg).flatMap(value => adaptArgument(value, expected, arg.span))

    private def adaptArgument(
        value: LirValue,
        expected: SourceType,
        span: SourceSpan,
    ): Option[LirValue] =
      SourceType.dealias(expected) match
        case SourceType.Ref(target, mutable) =>
          val expectedRef = SourceType.Ref(target, mutable)
          SourceType.dealias(value.valueType.source) match
            case actual @ SourceType.Ref(actualTarget, _)
                if SourceType.same(actualTarget, target) && SourceType
                  .assignable(actual, expectedRef) =>
              Some(value)
            case actual @ SourceType.Ref(actualTarget, _)
                if SourceType.same(actualTarget, target) =>
              report(
                "cosmo0.lir.lower.argument-mismatch",
                s"cannot pass ${actual.display} as ${expected.display}",
                span,
              )
              None
            case actual
                if SourceType.same(actual, target) && isBorrowable(value) =>
              Some(LirBorrowValue(value, mutable))
            case actual if SourceType.same(actual, target) =>
              report(
                "cosmo0.lir.lower.unsupported-borrow",
                s"cannot borrow non-place ${actual.display} as ${expected.display}",
                span,
              )
              None
            case _ =>
              Some(value)
        case _ =>
          Some(value)

    private def descriptorReceiverArg(value: LirValue): LirValue =
      SourceType.dealias(value.valueType.source) match
        case SourceType.Ref(target, _) => LirDerefValue(value, Lir.t(target))
        case _                         => value

    private def isBorrowable(value: LirValue): Boolean =
      value match
        case _: LirLocalRef   => true
        case _: LirGlobalRef  => true
        case _: LirDerefValue => true
        case _: LirFieldRef   => true
        case _                => false

    private def sequence[A](items: List[Option[A]]): Option[List[A]] =
      if items.forall(_.isDefined) then Some(items.flatten) else None

    private def lowerUserConstructor(
        constructor: TypedTypeConstructorExpr,
        output: Option[Binding],
        args: List[LirValue],
        ty: SourceType,
        sig: CallableSignature,
    ): Option[LirValue] =
      output match
        case Some(binding) =>
          emit(
            LirConstructType(
              binding.id,
              Lir.t(ty),
              sig.params.zip(args).map { case (param, arg) =>
                LirFieldInit(param.name, arg)
              },
            ),
          )
          callResult(output, ty)
        case None =>
          report(
            "cosmo0.lir.lower.invalid-constructor",
            s"type constructor ${constructor.constructedTy.display} has no LIR output",
            constructor.span,
          )
          None

    private def lowerDescriptorConstructor(
        constructor: TypedTypeConstructorExpr,
        output: Option[Binding],
        args: List[LirValue],
        ty: SourceType,
    ): Option[LirValue] =
      descriptorRefForConstructor(constructor.constructedTy) match
        case Some(descriptor) =>
          emit(
            LirDescriptorIntrinsic(
              output.map(_.id),
              descriptor,
              "<init>",
              args,
              Some(Lir.t(ty)),
            ),
          )
          callResult(output, ty)
        case None
            if isForeignDefaultConstructor(
              constructor.constructedTy,
            ) && args.isEmpty =>
          output match
            case Some(binding) =>
              emit(LirConstructType(binding.id, Lir.t(ty), Nil))
              callResult(output, ty)
            case None =>
              report(
                "cosmo0.lir.lower.invalid-constructor",
                s"type constructor ${constructor.constructedTy.display} has no LIR output",
                constructor.span,
              )
              None
        case None =>
          unsupportedDescriptor(
            constructor,
            descriptorName(constructor.constructedTy)
              .getOrElse(constructor.constructedTy.display),
            "<init>",
          )
          None

    private def isForeignDefaultConstructor(ty: SourceType): Boolean =
      SourceType.dealias(ty) match
        case SourceType.ForeignApplied(_, _) | SourceType.ForeignSymbol(_) =>
          true
        case _ => false

    private def lowerUnary(value: TypedUnary): Option[LirValue] =
      val opName =
        value.op match
          case "!" => Some("not")
          case "-" => Some("neg")
          case _   => None
      opName match
        case None if value.op == "*" =>
          lowerExpr(value.expr).map(arg => LirDerefValue(arg, Lir.t(value.ty)))
        case None if value.op == "&" =>
          lowerExpr(value.expr).flatMap { arg =>
            if isBorrowable(arg) then Some(LirBorrowValue(arg, mutable = false))
            else
              report(
                "cosmo0.lir.lower.unsupported-borrow",
                s"cannot borrow non-place ${arg.valueType.display}",
                value.span,
              )
              None
          }
        case Some(name) =>
          lowerExpr(value.expr).flatMap { arg =>
            descriptorRefForValue(arg.valueType.source, name) match
              case Some(descriptor) =>
                val output = declareTemp(value.ty)
                emit(
                  LirDescriptorIntrinsic(
                    Some(output.id),
                    descriptor,
                    name,
                    List(arg),
                    Some(Lir.t(value.ty)),
                  ),
                )
                Some(LirLocalRef(output.id, Lir.t(output.ty)))
              case None =>
                unsupportedDescriptor(
                  value,
                  descriptorName(arg.valueType.source)
                    .getOrElse(arg.valueType.display),
                  name,
                )
                None
          }
        case None =>
          unsupported(value, s"unary operator ${value.op}")
          None

    private def lowerBinary(value: TypedBinary): Option[LirValue] =
      value.op match
        case "and" | "&&" =>
          lowerShortCircuitBinary(value, isAnd = true)
        case "or" | "||" =>
          lowerShortCircuitBinary(value, isAnd = false)
        case _ =>
          lowerStrictBinary(value)

    private def lowerStrictBinary(value: TypedBinary): Option[LirValue] =
      val opName =
        value.op match
          case "+"  => Some("add")
          case "-"  => Some("sub")
          case "*"  => Some("mul")
          case "/"  => Some("div")
          case "%"  => Some("mod")
          case "==" => Some("eq")
          case "!=" => Some("ne")
          case "<"  => Some("lt")
          case "<=" => Some("le")
          case ">"  => Some("gt")
          case ">=" => Some("ge")
          case _    => None
      opName match
        case Some(name) =>
          for
            left <- lowerExpr(value.left)
            right <- lowerExpr(value.right)
            result <- lowerDescriptorBinary(value, name, left, right)
          yield result
        case None =>
          unsupported(value, s"binary operator ${value.op}")
          None

    private def lowerShortCircuitBinary(
        value: TypedBinary,
        isAnd: Boolean,
    ): Option[LirValue] =
      lowerExpr(value.left) match
        case Some(left) =>
          val group = nextLabelGroup(if isAnd then "and" else "or")
          val rhsLabel = numberedLabel(group, 0, "rhs")
          val shortLabel = numberedLabel(group, 1, "short")
          val joinLabel = numberedLabel(group, 2, "join")
          val output = declareTemp(value.ty, mutable = true)
          val shortValue =
            if isAnd then LirBoolValue(false) else LirBoolValue(true)

          if isAnd then {
            terminate(LirCondBranch(left, rhsLabel, shortLabel))
          } else {
            terminate(LirCondBranch(left, shortLabel, rhsLabel))
          }

          startBlock(rhsLabel)
          val right = lowerExpr(value.right)
          val rhsFallsThrough = finishStructuredBranch(
            Some(output),
            right,
            joinLabel,
            value.right.span,
          )

          startBlock(shortLabel)
          val shortFallsThrough = finishStructuredBranch(
            Some(output),
            Some(shortValue),
            joinLabel,
            value.left.span,
          )

          if rhsFallsThrough || shortFallsThrough then {
            startBlock(joinLabel)
            Some(LirLocalRef(output.id, Lir.t(output.ty)))
          } else {
            None
          }
        case None =>
          None

    private def lowerDescriptorBinary(
        value: TypedBinary,
        name: String,
        left: LirValue,
        right: LirValue,
    ): Option[LirValue] =
      descriptorRefForValue(left.valueType.source, name) match
        case Some(descriptor) =>
          val output = declareTemp(value.ty)
          emit(
            LirDescriptorIntrinsic(
              Some(output.id),
              descriptor,
              name,
              List(left, right),
              Some(Lir.t(value.ty)),
            ),
          )
          Some(LirLocalRef(output.id, Lir.t(output.ty)))
        case None =>
          unsupportedDescriptor(
            value,
            descriptorName(left.valueType.source)
              .getOrElse(left.valueType.display),
            name,
          )
          None

    private def lowerVariantValue(
        constructor: TypedVariantConstructorExpr,
    ): Option[LirValue] =
      SourceType.dealias(constructor.ty) match
        case SourceType.Function(_, _) =>
          unsupported(
            constructor,
            s"unapplied variant constructor ${constructor.owner.display}::${constructor.variant}",
          )
          None
        case SourceType.Unit | SourceType.Never =>
          Some(LirUnitValue)
        case _ =>
          val output = declareTemp(constructor.ty)
          emit(
            LirConstructVariant(
              output.id,
              Lir.t(constructor.owner),
              constructor.variant,
              Nil,
            ),
          )
          Some(LirLocalRef(output.id, Lir.t(output.ty)))

    private def lowerIf(value: TypedIf): Option[LirValue] =
      lowerExpr(value.cond) match
        case Some(condition) =>
          val group = nextLabelGroup("if")
          val thenLabel = numberedLabel(group, 0, "then")
          val elseLabel = numberedLabel(group, 1, "else")
          val joinLabel = numberedLabel(group, 2, "join")
          val resultSlot = joinOutput(value.ty)

          terminate(
            LirCondBranch(
              condition,
              thenLabel,
              value.elseExp.fold(joinLabel)(_ => elseLabel),
            ),
          )

          startBlock(thenLabel)
          val thenResult = lowerExpr(value.thenExp)
          val thenFallsThrough =
            finishStructuredBranch(
              resultSlot,
              thenResult,
              joinLabel,
              value.thenExp.span,
            )

          val elseFallsThrough =
            value.elseExp match
              case Some(elseExpr) =>
                startBlock(elseLabel)
                val elseResult = lowerExpr(elseExpr)
                finishStructuredBranch(
                  resultSlot,
                  elseResult,
                  joinLabel,
                  elseExpr.span,
                )
              case None =>
                true

          if thenFallsThrough || elseFallsThrough then
            startBlock(joinLabel)
            structuredResult(resultSlot)
          else None
        case None =>
          None

    private def lowerLoop(value: TypedLoop): Option[LirValue] =
      val group = nextLabelGroup("loop")
      val headerLabel = numberedLabel(group, 0, "header")
      val bodyLabel = numberedLabel(group, 1, "body")
      val continueLabel = numberedLabel(group, 2, "continue")
      val exitLabel = numberedLabel(group, 3, "exit")

      terminate(LirBranch(headerLabel))
      startBlock(headerLabel)
      terminate(LirBranch(bodyLabel))

      startBlock(bodyLabel)
      withLoop(LoopTargets(exitLabel, continueLabel)) {
        lowerExpr(value.body)
        if !isTerminated then terminate(LirBranch(continueLabel))
      }

      startBlock(continueLabel)
      terminate(LirBranch(headerLabel))

      startBlock(exitLabel)
      Some(LirUnitValue)

    private def lowerWhile(value: TypedWhile): Option[LirValue] =
      val group = nextLabelGroup("while")
      val headerLabel = numberedLabel(group, 0, "header")
      val bodyLabel = numberedLabel(group, 1, "body")
      val continueLabel = numberedLabel(group, 2, "continue")
      val exitLabel = numberedLabel(group, 3, "exit")

      terminate(LirBranch(headerLabel))
      startBlock(headerLabel)
      lowerExpr(value.cond) match
        case Some(condition) =>
          terminate(LirCondBranch(condition, bodyLabel, exitLabel))
        case None =>
          if !isTerminated then
            report(
              "cosmo0.lir.lower.missing-condition",
              "while condition did not produce a LIR value",
              value.cond.span,
            )
            terminate(
              LirErrorExit("cosmo0.lir.lower.missing-condition", Some("while")),
            )

      startBlock(bodyLabel)
      withLoop(LoopTargets(exitLabel, continueLabel)) {
        lowerExpr(value.body)
        if !isTerminated then terminate(LirBranch(continueLabel))
      }

      startBlock(continueLabel)
      terminate(LirBranch(headerLabel))

      startBlock(exitLabel)
      Some(LirUnitValue)

    private def lowerFor(value: TypedFor): Option[LirValue] =
      val iterAndDescriptor =
        lowerExpr(value.iter).flatMap(iterValue =>
          iterableDescriptor(
            iterValue.valueType.source,
            value.itemTy,
            value.span,
          ).map(iterValue -> _),
        )
      iterAndDescriptor match
        case None =>
          None
        case Some((iterValue, descriptor)) =>
          lowerForLoop(value, iterValue, descriptor)

    private def lowerForLoop(
        value: TypedFor,
        iterValue: LirValue,
        descriptor: LirDescriptorRef,
    ): Option[LirValue] =
      val group = nextLabelGroup("for")
      val headerLabel = numberedLabel(group, 0, "header")
      val bodyLabel = numberedLabel(group, 1, "body")
      val continueLabel = numberedLabel(group, 2, "continue")
      val exitLabel = numberedLabel(group, 3, "exit")
      val itemBinding =
        declareLocal(value.name, value.itemTy, mutable = false)

      terminate(LirBranch(headerLabel))
      startBlock(headerLabel)
      val hasNext = declareTemp(SourceType.Bool)
      emit(
        LirDescriptorIntrinsic(
          Some(hasNext.id),
          descriptor,
          "iter_has_next",
          List(iterValue),
          Some(Lir.t(SourceType.Bool)),
        ),
      )
      terminate(
        LirCondBranch(
          LirLocalRef(hasNext.id, Lir.t(SourceType.Bool)),
          bodyLabel,
          exitLabel,
        ),
      )

      startBlock(bodyLabel)
      withLoop(LoopTargets(exitLabel, continueLabel)) {
        withScope {
          bind(itemBinding)
          emit(
            LirDescriptorIntrinsic(
              Some(itemBinding.id),
              descriptor,
              "iter_next",
              List(iterValue),
              Some(Lir.t(value.itemTy)),
            ),
          )
          lowerExpr(value.body)
          if !isTerminated then terminate(LirBranch(continueLabel))
        }
      }

      startBlock(continueLabel)
      terminate(LirBranch(headerLabel))

      startBlock(exitLabel)
      Some(LirUnitValue)

    private def lowerMatch(value: TypedMatch): Option[LirValue] =
      lowerExpr(value.scrut) match
        case Some(scrut) =>
          lowerMatchWithScrut(value, scrut)
        case None =>
          None

    private def lowerMatchWithScrut(
        value: TypedMatch,
        scrut: LirValue,
    ): Option[LirValue] =
      val group = nextLabelGroup("match")
      val armLabels = value.arms.zipWithIndex.map { case (_, index) =>
        numberedLabel(group, 10 + index, s"arm$index")
      }
      val defaultLabel = numberedLabel(group, 80, "default")
      val joinLabel = numberedLabel(group, 90, "join")
      val resultSlot = joinOutput(value.ty)
      var needsDefault = true
      var chainOpen = true

      value.arms.zip(armLabels).zipWithIndex.foreach {
        case ((arm, armLabel), index) =>
          if chainOpen then
            patternCondition(arm.pat, scrut) match
              case Some(AlwaysPattern) =>
                terminate(LirBranch(armLabel))
                needsDefault = false
                chainOpen = false
              case Some(ConditionalPattern(condition)) =>
                val falseTarget =
                  if index == value.arms.length - 1 then defaultLabel
                  else numberedLabel(group, index + 1, "test")
                terminate(LirCondBranch(condition, armLabel, falseTarget))
                if index == value.arms.length - 1 then chainOpen = false
                else startBlock(falseTarget)
              case None =>
                if !isTerminated then
                  terminate(
                    LirErrorExit(
                      "cosmo0.lir.lower.unsupported-pattern",
                      Some("match"),
                    ),
                  )
                needsDefault = false
                chainOpen = false
      }

      if value.arms.isEmpty && !isTerminated then
        terminate(LirBranch(defaultLabel))

      var anyFallthrough = false
      value.arms.zip(armLabels).foreach { case (arm, armLabel) =>
        startBlock(armLabel)
        val armResult = withScope {
          bindPattern(arm.pat, scrut)
          arm.body.flatMap(lowerExpr).orElse(Some(LirUnitValue))
        }
        val fallsThrough =
          finishStructuredBranch(resultSlot, armResult, joinLabel, arm.span)
        anyFallthrough = anyFallthrough || fallsThrough
      }

      if needsDefault then
        startBlock(defaultLabel)
        terminate(
          LirErrorExit(
            "cosmo0.lir.lower.non-exhaustive-match",
            Some(function.name),
          ),
        )

      if anyFallthrough then
        startBlock(joinLabel)
        structuredResult(resultSlot)
      else None

    private def lowerBreak(value: TypedBreak): Option[LirValue] =
      loopStack.lastOption match
        case Some(targets) =>
          terminate(LirBranch(targets.breakTarget))
        case None =>
          report(
            "cosmo0.lir.lower.invalid-break",
            "break can only be lowered inside a loop",
            value.span,
          )
          terminate(
            LirErrorExit("cosmo0.lir.lower.invalid-break", Some(function.name)),
          )
      None

    private def lowerContinue(value: TypedContinue): Option[LirValue] =
      loopStack.lastOption match
        case Some(targets) =>
          terminate(LirBranch(targets.continueTarget))
        case None =>
          report(
            "cosmo0.lir.lower.invalid-continue",
            "continue can only be lowered inside a loop",
            value.span,
          )
          terminate(
            LirErrorExit(
              "cosmo0.lir.lower.invalid-continue",
              Some(function.name),
            ),
          )
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
                emit(
                  LirAssign(
                    LirLocalPlace(binding.id, Lir.t(binding.ty)),
                    loweredValue,
                  ),
                )
              case None =>
                unsupported(
                  assign.target,
                  s"assignment to non-local ${name.path.text}",
                )
          }

        case select: TypedSelect =>
          lowerExpr(select.recv).foreach { recv =>
            lowerExpr(assign.value).foreach { loweredValue =>
              emit(LirFieldSet(recv, select.field, loweredValue))
            }
          }

        case other =>
          unsupported(other, "assignment target")

    private def returnTerminator(
        value: Option[LirValue],
        expectedTy: SourceType,
        span: SourceSpan,
    ): LirTerminator =
      SourceType.dealias(expectedTy) match
        case SourceType.Unit =>
          LirReturn(None)
        case _ =>
          value match
            case Some(actual) =>
              LirReturn(Some(actual))
            case None =>
              report(
                "cosmo0.lir.lower.missing-return",
                s"function ${function.name} did not produce a ${expectedTy.display} return value",
                span,
              )
              LirErrorExit(
                "cosmo0.lir.lower.missing-return",
                Some(function.name),
              )

    private def callOutput(ty: SourceType): Option[Binding] =
      SourceType.dealias(ty) match
        case SourceType.Unit | SourceType.Never =>
          None
        case _ =>
          Some(declareTemp(ty))

    private def callResult(
        output: Option[Binding],
        ty: SourceType,
    ): Option[LirValue] =
      output match
        case Some(binding) =>
          Some(LirLocalRef(binding.id, Lir.t(ty)))
        case None =>
          Some(LirUnitValue)

    private def joinOutput(ty: SourceType): Option[Binding] =
      SourceType.dealias(ty) match
        case SourceType.Unit | SourceType.Never =>
          None
        case _ =>
          Some(declareTemp(ty, mutable = true))

    private def structuredResult(output: Option[Binding]): Option[LirValue] =
      output match
        case Some(binding) =>
          Some(LirLocalRef(binding.id, Lir.t(binding.ty)))
        case None =>
          Some(LirUnitValue)

    private def finishStructuredBranch(
        output: Option[Binding],
        result: Option[LirValue],
        joinLabel: LirLabel,
        span: SourceSpan,
    ): Boolean =
      if isTerminated then false
      else
        var canBranch = true
        output match
          case Some(binding) =>
            result match
              case Some(value) =>
                emit(
                  LirAssign(
                    LirLocalPlace(binding.id, Lir.t(binding.ty)),
                    value,
                  ),
                )
              case None =>
                report(
                  "cosmo0.lir.lower.missing-branch-value",
                  s"structured expression did not produce ${binding.ty.display}",
                  span,
                )
                terminate(
                  LirErrorExit(
                    "cosmo0.lir.lower.missing-branch-value",
                    Some(function.name),
                  ),
                )
                canBranch = false
          case None =>
        if canBranch && !isTerminated then
          terminate(LirBranch(joinLabel))
          true
        else false

    private def declareLocal(
        name: String,
        ty: SourceType,
        mutable: Boolean,
    ): Binding =
      val id = nextLocalId(name)
      val binding =
        Binding(name, id, ty, mutable, mutationCapability(ty))
      localBuffer += localFor(binding)
      binding

    private def declareTemp(
        ty: SourceType,
        mutable: Boolean = false,
        mutAllowed: Option[Boolean] = None,
    ): Binding =
      val name = s"tmp$tempIndex"
      tempIndex += 1
      val id = nextLocalId(name)
      val binding = Binding(
        name,
        id,
        ty,
        mutable,
        mutAllowed.getOrElse(mutationCapability(ty)),
      )
      localBuffer += localFor(binding)
      binding

    private def nextLocalId(name: String): LirLocalId =
      val base = stablePart(name)
      val ordinal = localOrdinals.getOrElse(base, 0)
      localOrdinals.update(base, ordinal + 1)
      if ordinal == 0 then Lir.localId(base)
      else Lir.localId(s"${base}_$ordinal")

    private def localFor(binding: Binding): LirLocal =
      LirLocal(
        binding.id,
        binding.name,
        Lir.t(binding.ty),
        binding.mutable,
        binding.mutAllowed,
      )

    private def bind(binding: Binding): Unit =
      scopes.last.update(binding.name, binding)

    private def resolve(name: String): Option[Binding] =
      scopes.reverseIterator.flatMap(_.get(name)).nextOption()

    private def withScope[A](body: => A): A =
      scopes += mutable.LinkedHashMap.empty[String, Binding]
      try body
      finally scopes.remove(scopes.length - 1)

    private def withLoop[A](targets: LoopTargets)(body: => A): A =
      loopStack += targets
      try body
      finally loopStack.remove(loopStack.length - 1)

    private def emit(op: LirOp): Unit =
      if !isTerminated then current.operations += op

    private def terminate(terminator: LirTerminator): Unit =
      if current.terminator.isEmpty then current.terminator = Some(terminator)

    private def isTerminated: Boolean =
      current.terminator.nonEmpty

    private def newBlock(label: LirLabel): OpenBlock =
      val block = new OpenBlock(label)
      blockBuffer += block
      block

    private def startBlock(label: LirLabel): Unit =
      current = newBlock(label)

    private def nextLabelGroup(prefix: String): String =
      val base = stablePart(prefix)
      val ordinal = labelOrdinals.getOrElse(base, 0)
      labelOrdinals.update(base, ordinal + 1)
      s"$base$ordinal"

    private def numberedLabel(
        group: String,
        index: Int,
        suffix: String,
    ): LirLabel =
      Lir.label(s"${group}_${"%02d".format(index)}_${stablePart(suffix)}")

    private def descriptorRefForConstructor(
        ty: SourceType,
    ): Option[LirDescriptorRef] =
      descriptorRefFor(ty).filter { descriptor =>
        descriptors
          .get(descriptor.name)
          .exists(_.constructor("<init>").nonEmpty)
      }

    private def descriptorRefForMethod(
        ty: SourceType,
        method: String,
    ): Option[LirDescriptorRef] =
      descriptorRefFor(ty).filter { descriptor =>
        descriptors.get(descriptor.name).exists(_.method(method).nonEmpty)
      }

    private def descriptorRefForValue(
        ty: SourceType,
        operation: String,
    ): Option[LirDescriptorRef] =
      descriptorRefFor(ty).filter { descriptor =>
        descriptors.get(descriptor.name).exists(_.method(operation).nonEmpty)
      }

    private def descriptorRefFor(
        ty: SourceType,
    ): Option[LirDescriptorRef] =
      SourceType.dealias(ty) match
        case SourceType.Ref(target, _) =>
          descriptorRefFor(target)
        case SourceType.Standard(name, args)
            if descriptors.get(name).exists(_.arity == args.length) =>
          Some(LirDescriptorRef(name, args.map(Lir.t)))
        case SourceType.Builtin(name)
            if descriptors.get(name).exists(_.arity == 0) =>
          Some(LirDescriptorRef(name))
        case _ =>
          None

    private def isDescriptorBacked(ty: SourceType): Boolean =
      descriptorRefFor(ty).nonEmpty

    private def descriptorName(ty: SourceType): Option[String] =
      SourceType.dealias(ty) match
        case SourceType.Ref(target, _)    => descriptorName(target)
        case SourceType.Standard(name, _) => Some(name)
        case SourceType.Builtin(name)     => Some(name)
        case _                            => None

    private def iterableDescriptor(
        ty: SourceType,
        itemTy: SourceType,
        span: SourceSpan,
    ): Option[LirDescriptorRef] =
      SourceType.dealias(ty) match
        case SourceType.Standard(
              name @ ("Vec" | "Set" | "Arena"),
              item :: Nil,
            ) =>
          if !SourceType.same(item, itemTy) then
            report(
              "cosmo0.lir.lower.invalid-iterator",
              s"for loop item type ${itemTy.display} does not match ${ty.display} item ${item.display}",
              span,
            )
          Some(LirDescriptorRef(name, List(Lir.t(item))))
        case SourceType.Standard("Map", key :: value :: Nil) =>
          if !SourceType.same(key, itemTy) then
            report(
              "cosmo0.lir.lower.invalid-iterator",
              s"for loop item type ${itemTy.display} does not match ${ty.display} key ${key.display}",
              span,
            )
          Some(LirDescriptorRef("Map", List(Lir.t(key), Lir.t(value))))
        case other =>
          report(
            "cosmo0.lir.lower.invalid-iterator",
            s"type ${other.display} is not a descriptor-backed iterable",
            span,
          )
          None

    private def patternCondition(
        pattern: TypedPattern,
        scrut: LirValue,
    ): Option[PatternCondition] =
      pattern match
        case _: TypedWildcardPattern =>
          Some(AlwaysPattern)
        case _: TypedBindingPattern =>
          Some(AlwaysPattern)
        case TypedVariantPattern(
              constructor: TypedVariantConstructorExpr,
              _,
              ty,
              _,
            ) =>
          val output = declareTemp(SourceType.Bool)
          emit(
            LirCheckVariantTag(
              output.id,
              scrut,
              Lir.t(ty),
              constructor.variant,
            ),
          )
          Some(
            ConditionalPattern(LirLocalRef(output.id, Lir.t(SourceType.Bool))),
          )
        case value: TypedVariantPattern =>
          unsupportedPattern(value, "variant pattern with non-constructor head")
          None
        case value =>
          unsupportedPattern(value, "literal pattern")
          None

    private def bindPattern(pattern: TypedPattern, scrut: LirValue): Unit =
      pattern match
        case TypedWildcardPattern(_, _) =>
        case TypedBindingPattern(name, ty, _) =>
          val binding = declareLocal(name, ty, mutable = false)
          bind(binding)
          emit(LirAllocLocal(localFor(binding), Some(scrut)))
        case TypedVariantPattern(
              constructor: TypedVariantConstructorExpr,
              args,
              _,
              _,
            ) =>
          args.zipWithIndex.foreach { case (arg, index) =>
            arg match
              case TypedWildcardPattern(_, _) =>
              case TypedBindingPattern(name, ty, _) =>
                val binding = declareLocal(name, ty, mutable = false)
                bind(binding)
                emit(
                  LirReadVariantPayload(
                    binding.id,
                    scrut,
                    constructor.variant,
                    index,
                    Lir.t(ty),
                  ),
                )
              case other =>
                unsupportedPattern(
                  other,
                  "nested or literal variant payload pattern",
                )
          }
        case value: TypedVariantPattern =>
          unsupportedPattern(value, "variant pattern with non-constructor head")
        case _ =>

    private def unsupportedPattern(
        pattern: TypedPattern,
        construct: String,
    ): Unit =
      report(
        "cosmo0.lir.lower.unsupported-pattern",
        s"match lowering does not support $construct yet",
        pattern.span,
      )

    private def unsupported(node: TypedNode, construct: String): Unit =
      report(
        "cosmo0.lir.lower.unsupported-expression",
        s"declaration lowering does not support $construct yet",
        node.span,
      )

    private def unsupportedDescriptor(
        node: TypedNode,
        descriptor: String,
        operation: String,
    ): Unit =
      report(
        "cosmo0.lir.lower.unsupported-descriptor",
        s"descriptor $descriptor has no lowerable operation $operation",
        node.span,
      )

    private def isFunctionType(ty: SourceType): Boolean =
      SourceType.dealias(ty) match
        case SourceType.Function(_, _) => true
        case _                         => false

  private val errors = ListBuffer.empty[Diagnostic]
  private val context = DeclContext.from(module)
  private val syntheticExternNames = mutable.LinkedHashSet.empty[String]

  private def diagnostics: List[Diagnostic] =
    errors.toList

  private def lowerModule(): LirModule =
    LirModule(
      moduleName(module.source),
      (module.decls.flatMap(lowerDecl) ++ syntheticExternDeclarations)
        .sortBy(_.id.value),
      module.cIncludes,
      module.cppImports,
    )

  private def lowerDecl(declaration: TypedDecl): List[LirDeclaration] =
    declaration match
      case _: TypedImport =>
        Nil

      case _: TypedCppNamespaceImport =>
        Nil

      case alias: TypedTypeAlias =>
        List(
          LirTypeAliasDecl(
            context.aliasId(None, alias.name),
            alias.name,
            Lir.t(alias.target),
            alias.tyParams,
          ),
        )

      case value: TypedValueDecl =>
        List(
          LirGlobal(
            context
              .globalId(value.name)
              .getOrElse(Lir.declId(qualifiedId(None, value.name))),
            value.name,
            Lir.t(value.ty),
            mutable = value.kind == UntypedValueKind.Var,
            initializer = value.init.flatMap(staticValue),
            mutationAllowed = mutationCapability(value.ty),
          ),
        )

      case fn: TypedFunction =>
        List(lowerFunction(fn))

      case cls: TypedClass =>
        val typeDecl =
          LirTypeDecl(
            context
              .typeId(cls.name)
              .getOrElse(Lir.declId(stablePart(cls.name))),
            cls.name,
            fields = cls.fields.map(field =>
              LirField(
                field.name,
                Lir.t(field.ty),
                mutable = field.kind == UntypedValueKind.Var,
              ),
            ),
            variants = cls.variants.map(variant =>
              LirVariant(
                variant.name,
                variant.fields.map(field =>
                  LirVariantPayload(
                    field.name,
                    Lir.t(field.ty),
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
            alias.tyParams,
          ),
        )
        typeDecl :: aliases ::: cls.methods.map(lowerFunction)

  private def lowerFunction(function: TypedFunction): LirFunction =
    TrustedExternAbi.bindingForDeclaration(function) match
      case Some(binding) =>
        LirFunction(
          context
            .functionId(function.owner, function.name)
            .getOrElse(
              Lir.declId(qualifiedId(function.owner, function.name)),
            ),
          function.name,
          function.params.map { param =>
            LirParam(
              Lir.localId(stablePart(param.name)),
              param.name,
              Lir.t(param.ty),
              mutationCapability(param.ty),
            )
          },
          Lir.t(function.retTy),
          locals = Nil,
          blocks = Nil,
          owner = function.owner.flatMap(context.typeId),
          sourceSignature = Some(function.sig),
          externBinding = Some(binding),
        )
      case None if function.body.isEmpty =>
        error(
          "cosmo0.lir.lower.missing-extern-binding",
          s"function ${function.name} has no body and no trusted extern ABI binding",
          function.span,
        )
        FunctionBuilder(function, context, error, useSyntheticExtern).lower()
      case None =>
        FunctionBuilder(function, context, error, useSyntheticExtern).lower()

  private def useSyntheticExtern(name: String): LirDeclId =
    syntheticExternNames += name
    TrustedExternAbi.syntheticId(name)

  private def syntheticExternDeclarations: List[LirFunction] =
    syntheticExternNames.toList.sorted.flatMap(
      TrustedExternAbi.syntheticFunction,
    )

  private def staticValue(expr: TypedExpr): Option[LirValue] =
    expr match
      case TypedUnitLiteral(_, _) =>
        Some(LirUnitValue)
      case TypedBoolLiteral(value, _, _) =>
        Some(LirBoolValue(value))
      case TypedIntLiteral(value, ty, _) =>
        Some(LirIntValue(value, Lir.t(ty)))
      case TypedFloatLiteral(value, ty, _) =>
        Some(LirFloatValue(value, Lir.t(ty)))
      case TypedStringLiteral(value, _, _) =>
        Some(LirStringValue(value))
      case name: TypedName if name.path.parts.length == 1 =>
        val valueName = name.path.parts.head
        context
          .globalId(valueName)
          .map(id => LirGlobalRef(id, Lir.t(name.ty)))
          .orElse(
            context
              .topLevelFunction(valueName)
              .flatMap(fn =>
                context
                  .functionId(None, valueName)
                  .map(id =>
                    LirFunctionRef(
                      id,
                      LirCallableSignature.fromSource(fn.sig),
                    ),
                  ),
              ),
          )
      case other =>
        error(
          "cosmo0.lir.lower.unsupported-initializer",
          s"global ${other.ty.display} initializer is not a static LIR value",
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

  private def mutationCapability(ty: SourceType): Boolean =
    SourceType.dealias(ty) match
      case SourceType.Ref(_, mutable)          => mutable
      case SourceType.Never | SourceType.Error => false
      case _                                   => true

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
