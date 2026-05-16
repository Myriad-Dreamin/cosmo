package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object SourceTyper:
  def apply(): SourceTyper =
    new SourceTyper(StandardGenericDescriptors.all)

final class SourceTyper(
    standardGenerics: Map[String, StandardGenericDescriptor],
):
  def check(module: UntypedModule): Result[TypedModule] =
    val state = State(module)
    state.check()

  private final case class ValueSymbol(
      name: String,
      valueType: SourceType,
      mutableBinding: Boolean,
      mutationAllowed: Boolean,
      span: SourceSpan,
  )

  private final class Scope(parent: Option[Scope]):
    private val values = mutable.LinkedHashMap.empty[String, ValueSymbol]

    def define(symbol: ValueSymbol): Unit =
      values.update(symbol.name, symbol)

    def resolve(name: String): Option[ValueSymbol] =
      values.get(name).orElse(parent.flatMap(_.resolve(name)))

    def child: Scope =
      new Scope(Some(this))

  private final case class ExprInfo(
      expr: TypedExpr,
      mutableBinding: Boolean,
      mutationAllowed: Boolean,
  )

  private final case class FieldInfo(
      name: String,
      kind: UntypedValueKind,
      valueType: SourceType,
      init: Option[UntypedExpr],
      span: SourceSpan,
  )

  private final case class VariantInfo(
      name: String,
      fields: List[TypedVariantField],
      span: SourceSpan,
  ):
    def signature(owner: String): CallableSignature =
      CallableSignature(
        name,
        fields.zipWithIndex.map { case (field, index) =>
          CallableParam(field.name.getOrElse(s"field$index"), field.valueType, field.span)
        },
        SourceType.User(owner),
      )

  private final case class ParamInfo(
      name: String,
      valueType: SourceType,
      default: Option[UntypedExpr],
      span: SourceSpan,
  )

  private final case class FunctionInfo(
      name: String,
      params: List[ParamInfo],
      returnType: SourceType,
      body: Option[UntypedExpr],
      signature: CallableSignature,
      owner: Option[String],
      span: SourceSpan,
      externBinding: Option[SourceExternBinding],
  )

  private final case class TraitInfo(
      name: String,
      methods: Map[String, FunctionInfo],
      span: SourceSpan,
  )

  private final case class ClassInfo(
      name: String,
      fields: List[FieldInfo],
      aliases: List[TypedTypeAlias],
      variants: Map[String, VariantInfo],
      methods: Map[String, FunctionInfo],
      span: SourceSpan,
  ):
    def constructorSignature: CallableSignature =
      CallableSignature(
        name,
        fields.map(field => CallableParam(field.name, field.valueType, field.span)),
        SourceType.User(name),
      )

  private final class State(module: UntypedModule):
    private val diagnostics = ListBuffer.empty[Diagnostic]
    private val classNames =
      module.declarations.collect { case decl: UntypedClass => decl.name }.toSet
    private val rawAliases = mutable.LinkedHashMap.empty[String, UntypedType]
    private val aliasTypes = mutable.LinkedHashMap.empty[String, SourceType]
    private val traits = mutable.LinkedHashMap.empty[String, TraitInfo]
    private val classes = mutable.LinkedHashMap.empty[String, ClassInfo]
    private val functions = mutable.LinkedHashMap.empty[String, FunctionInfo]

    def check(): Result[TypedModule] =
      collectAliases()
      rawAliases.keys.foreach(resolveAlias)
      collectTraits()
      collectClasses()
      collectImpls()
      collectFunctions()

      val globalScope = new Scope(None)
      functions.values.foreach(info =>
        globalScope.define(
          ValueSymbol(
            info.name,
            info.signature.functionType,
            mutableBinding = false,
            mutationAllowed = false,
            info.span,
          ),
        ),
      )

      val declarations = module.declarations.flatMap(decl =>
        typedDecl(decl, globalScope),
      )

      val result = TypedModule(module.source, declarations, module.span, module.cIncludes)
      if diagnostics.isEmpty then Result.success(Phase.Check, result)
      else Result.failure(Phase.Check, diagnostics.toList)

    private def collectAliases(): Unit =
      module.declarations.foreach {
        case alias: UntypedTypeAlias =>
          rawAliases.update(alias.name, alias.target)
        case cls: UntypedClass =>
          cls.members.foreach {
            case alias: UntypedTypeAlias =>
              rawAliases.update(alias.name, alias.target)
            case _ =>
          }
        case _ =>
      }

    private def collectTraits(): Unit =
      module.declarations.collect { case trt: UntypedTrait => trt }.foreach { trt =>
        val methods = trt.methods.map(method => functionInfo(method, Some(trt.name)))
        duplicateMethodNames(methods, trt.name, trt.span)
        traits.update(
          trt.name,
          TraitInfo(
            trt.name,
            methods.groupBy(_.name).view.mapValues(_.head).toMap,
            trt.span,
          ),
        )
      }

    private def collectClasses(): Unit =
      module.declarations.collect { case cls: UntypedClass => cls }.foreach { cls =>
        val fields = cls.members.collect { case field: UntypedValueDecl =>
          FieldInfo(
            field.name,
            field.kind,
            field.valueType.map(resolveType(_, Some(cls.name))).getOrElse {
              error(
                "cosmo0.type.missing-annotation",
                s"field ${field.name} requires an explicit type",
                field.span,
              )
              SourceType.Error
            },
            field.init,
            field.span,
          )
        }
        val aliases = cls.members.collect { case alias: UntypedTypeAlias =>
          TypedTypeAlias(alias.name, resolveAlias(alias.name), alias.span)
        }
        val variants = cls.members.collect { case variant: UntypedVariant =>
          val fields = variant.fields.map(field =>
            TypedVariantField(
              field.name,
              resolveType(field.valueType, Some(cls.name)),
              field.span,
            ),
          )
          variant.name -> VariantInfo(variant.name, fields, variant.span)
        }.toMap
        val methods = cls.members.collect { case fn: UntypedFunction =>
          functionInfo(fn, Some(cls.name))
        }.map(info => info.name -> info).toMap

        classes.update(
          cls.name,
          ClassInfo(cls.name, fields, aliases, variants, methods, cls.span),
        )
      }

    private def collectImpls(): Unit =
      module.declarations.collect { case impl: UntypedImpl => impl }.foreach(collectImpl)

    private def collectImpl(impl: UntypedImpl): Unit =
      val targetName = impl.target.parts.headOption.getOrElse(impl.target.text)
      val traitName = impl.traitName.text
      val traitInfo = traits.get(traitName)
      val classInfo = classes.get(targetName)

      if traitInfo.isEmpty then
        error(
          "cosmo0.type.unknown-trait",
          s"impl trait $traitName is not a known trait",
          impl.traitName.span,
        )
      if classInfo.isEmpty then
        error(
          "cosmo0.type.unknown-type",
          s"impl target ${impl.target.text} is not a known class",
          impl.target.span,
        )
      if traitInfo.isEmpty || classInfo.isEmpty then return

      val target = classInfo.get
      val methods = impl.members.collect { case fn: UntypedFunction =>
        functionInfo(fn, Some(targetName))
      }
      validateTraitImplementation(impl, traitInfo.get, target, methods)

      val freshMethods = methods.filterNot(method =>
        target.methods.contains(method.name),
      )
      val mergedMethods = target.methods ++ freshMethods.map(method => method.name -> method)
      classes.update(targetName, target.copy(methods = mergedMethods))

    private def validateTraitImplementation(
        impl: UntypedImpl,
        traitInfo: TraitInfo,
        targetInfo: ClassInfo,
        methods: List[FunctionInfo],
    ): Unit =
      duplicateMethodNames(methods, s"${impl.traitName.text} impl for ${targetInfo.name}", impl.span)

      methods.foreach { method =>
        if targetInfo.methods.contains(method.name) then
          error(
            "cosmo0.type.duplicate-method",
            s"type ${targetInfo.name} already defines method ${method.name}",
            method.span,
          )
        if !traitInfo.methods.contains(method.name) then
          error(
            "cosmo0.type.impl-extra-method",
            s"trait ${traitInfo.name} does not declare method ${method.name}",
            method.span,
          )
      }

      traitInfo.methods.values.foreach { expected =>
        methods.find(_.name == expected.name) match
          case Some(actual) =>
            val specialized = specializeSignature(expected.signature, traitInfo.name, targetInfo.name)
            if !sameCallableSignature(actual.signature, specialized) then
              error(
                "cosmo0.type.impl-signature-mismatch",
                s"impl ${traitInfo.name} for ${targetInfo.name} method ${actual.name} does not match the trait signature",
                actual.span,
              )
          case None =>
            error(
              "cosmo0.type.impl-missing-method",
              s"impl ${traitInfo.name} for ${targetInfo.name} is missing method ${expected.name}",
              impl.span,
            )
      }

    private def duplicateMethodNames(
        methods: List[FunctionInfo],
        ownerName: String,
        span: SourceSpan,
    ): Unit =
      methods
        .groupBy(_.name)
        .toList
        .filter(_._2.length > 1)
        .foreach { case (name, duplicates) =>
          error(
            "cosmo0.type.duplicate-method",
            s"$ownerName declares method $name multiple times",
            duplicates.headOption.map(_.span).getOrElse(span),
          )
        }

    private def specializeSignature(
        signature: CallableSignature,
        traitName: String,
        targetName: String,
    ): CallableSignature =
      CallableSignature(
        signature.name,
        signature.params.map(param =>
          CallableParam(param.name, specializeSelfType(param.valueType, traitName, targetName), param.span),
        ),
        specializeSelfType(signature.returnType, traitName, targetName),
        signature.receiver.map(receiver =>
          CallableReceiver(
            specializeSelfType(receiver.valueType, traitName, targetName),
            receiver.mutable,
          ),
        ),
      )

    private def specializeSelfType(
        valueType: SourceType,
        traitName: String,
        targetName: String,
    ): SourceType =
      valueType match
        case SourceType.User(name) if name == traitName =>
          SourceType.User(targetName)
        case SourceType.Ref(target, mutable) =>
          SourceType.Ref(specializeSelfType(target, traitName, targetName), mutable)
        case SourceType.Standard(name, args) =>
          SourceType.Standard(name, args.map(specializeSelfType(_, traitName, targetName)))
        case SourceType.Function(params, returnType) =>
          SourceType.Function(
            params.map(specializeSelfType(_, traitName, targetName)),
            specializeSelfType(returnType, traitName, targetName),
          )
        case SourceType.Alias(name, target) =>
          SourceType.Alias(name, specializeSelfType(target, traitName, targetName))
        case other =>
          other

    private def sameCallableSignature(left: CallableSignature, right: CallableSignature): Boolean =
      left.name == right.name &&
        sameReceiver(left.receiver, right.receiver) &&
        left.params.length == right.params.length &&
        left.params.zip(right.params).forall { case (l, r) => SourceType.same(l.valueType, r.valueType) } &&
        SourceType.same(left.returnType, right.returnType)

    private def sameReceiver(left: Option[CallableReceiver], right: Option[CallableReceiver]): Boolean =
      (left, right) match
        case (None, None) =>
          true
        case (Some(l), Some(r)) =>
          l.mutable == r.mutable && SourceType.same(l.valueType, r.valueType)
        case _ =>
          false

    private def collectFunctions(): Unit =
      module.declarations.collect { case fn: UntypedFunction => fn }.foreach { fn =>
        val info = functionInfo(fn, None)
        functions.update(info.name, info)
      }

    private def typedDecl(decl: UntypedDecl, scope: Scope): Option[TypedDecl] =
      decl match
        case importDecl: UntypedImport =>
          Some(TypedImport(importDecl.path, importDecl.dest, importDecl.span))
        case alias: UntypedTypeAlias =>
          Some(TypedTypeAlias(alias.name, resolveAlias(alias.name), alias.span))
        case value: UntypedValueDecl =>
          val typed = typedValueDecl(value, scope)
          typed.foreach(valueDecl =>
            scope.define(valueSymbol(valueDecl.name, valueDecl.valueType, valueDecl.kind, valueDecl.span)),
          )
          typed
        case fn: UntypedFunction =>
          functions.get(fn.name).map(info => typedFunction(info, scope))
        case _: UntypedTrait =>
          None
        case cls: UntypedClass =>
          classes.get(cls.name).map(info => typedClass(info, scope))
        case _: UntypedImpl =>
          None

    private def typedClass(info: ClassInfo, globalScope: Scope): TypedClass =
      val classScope = globalScope.child
      info.methods.values.foreach(method =>
        classScope.define(
          ValueSymbol(
            method.name,
            method.signature.functionType,
            mutableBinding = false,
            mutationAllowed = false,
            method.span,
          ),
        ),
      )

      val fields = info.fields.map { field =>
        val init = field.init.map(expr(_, classScope, Some(field.valueType), FunctionContext.None))
        init.foreach(checkAssignable(_, field.valueType, field.span, "cosmo0.type.assignment-mismatch"))
        TypedValueDecl(field.kind, field.name, field.valueType, init.map(_.expr), field.span)
      }
      val methods = info.methods.values.toList.map(method => typedFunction(method, globalScope))
      TypedClass(
        info.name,
        fields,
        info.aliases,
        info.variants.values.toList.map(variant =>
          TypedVariant(variant.name, variant.fields, variant.span),
        ),
        methods,
        info.span,
      )

    private def typedFunction(info: FunctionInfo, outerScope: Scope): TypedFunction =
      val fnScope = outerScope.child
      info.params.foreach(param =>
        fnScope.define(
          ValueSymbol(
            param.name,
            param.valueType,
            mutableBinding = false,
            mutationAllowed = mutationCapability(param.valueType),
            param.span,
          ),
        ),
      )
      val context = FunctionContext.Some(info.returnType, info.owner)
      val defaults = info.params.map { param =>
        param.default.map(expr(_, fnScope, Some(param.valueType), context)).map { typed =>
          checkAssignable(typed, param.valueType, param.span, "cosmo0.type.assignment-mismatch")
          typed.expr
        }
      }
      val typedParams = info.params.zip(defaults).map { case (param, default) =>
        TypedParam(param.name, param.valueType, default, param.span)
      }
      val body = info.body.map(expr(_, fnScope, Some(info.returnType), context))
      body.foreach(checkAssignable(_, info.returnType, info.span, "cosmo0.type.return-mismatch"))
      TypedFunction(
        info.name,
        typedParams,
        info.returnType,
        body.map(_.expr),
        info.signature,
        info.owner,
        info.span,
        info.externBinding,
      )

    private def typedValueDecl(value: UntypedValueDecl, scope: Scope): Option[TypedValueDecl] =
      val explicitType = value.valueType.map(resolveType(_, None))
      val init = value.init.map(expr(_, scope, explicitType, FunctionContext.None))
      val valueType =
        explicitType.orElse(init.map(_.expr.valueType)).getOrElse {
          error(
            "cosmo0.type.missing-annotation",
            s"value ${value.name} requires a type annotation or initializer",
            value.span,
          )
          SourceType.Error
        }
      init.foreach(checkAssignable(_, valueType, value.span, "cosmo0.type.assignment-mismatch"))
      Some(TypedValueDecl(value.kind, value.name, valueType, init.map(_.expr), value.span))

    private def functionInfo(fn: UntypedFunction, owner: Option[String]): FunctionInfo =
      val params = fn.params.map(param =>
        val valueType = param.valueType.map(resolveType(_, owner)).getOrElse {
          error(
            "cosmo0.type.missing-annotation",
            s"parameter ${param.name} requires an explicit type",
            param.span,
          )
          SourceType.Error
        }
        ParamInfo(param.name, valueType, param.default, param.span)
      )
      val returnType = fn.returnType.map(resolveType(_, owner)).getOrElse(SourceType.Unit)
      val receiver = owner.flatMap: ownerName =>
        params.headOption.filter(_.name == "self").flatMap: self =>
          SourceType.dealias(self.valueType) match
            case SourceType.Ref(SourceType.User(name), mutable) if name == ownerName =>
              Some(CallableReceiver(SourceType.User(ownerName), mutable))
            case SourceType.User(name) if name == ownerName =>
              Some(CallableReceiver(SourceType.User(ownerName), mutable = true))
            case _ =>
              error(
                "cosmo0.type.invalid-self",
                s"method ${fn.name} has a self parameter that does not refer to $ownerName",
                self.span,
              )
              None
      val callableParams =
        if receiver.nonEmpty && params.headOption.exists(_.name == "self") then params.tail
        else params
      val signature = CallableSignature(
        fn.name,
        callableParams.map(param => CallableParam(param.name, param.valueType, param.span)),
        returnType,
        receiver,
      )
      FunctionInfo(fn.name, params, returnType, fn.body, signature, owner, fn.span, fn.externBinding)

    private def expr(
        node: UntypedExpr,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      node match
        case value: UntypedBlock =>
          blockExpr(value, scope, expected, context)
        case value: UntypedName =>
          nameExpr(value, scope)
        case value: UntypedTypeConstructor =>
          typeConstructorExpr(value)
        case value: UntypedSelect =>
          selectExpr(value, scope, context)
        case value: UntypedVariantConstructor =>
          variantConstructorExpr(value)
        case value: UntypedCall =>
          callExpr(value, scope, expected, context)
        case value: UntypedAssign =>
          assignExpr(value, scope, context)
        case value: UntypedUnary =>
          unaryExpr(value, scope, expected, context)
        case value: UntypedBinary =>
          binaryExpr(value, scope, expected, context)
        case value: UntypedIf =>
          ifExpr(value, scope, expected, context)
        case value: UntypedLoop =>
          val body = expr(value.body, scope.child, Some(SourceType.Unit), context)
          ExprInfo(TypedLoop(body.expr, SourceType.Unit, value.span), mutableBinding = false, mutationAllowed = true)
        case value: UntypedWhile =>
          val cond = expr(value.cond, scope, Some(SourceType.Bool), context)
          requireBool(cond, value.cond.span)
          val body = expr(value.body, scope.child, Some(SourceType.Unit), context)
          ExprInfo(TypedWhile(cond.expr, body.expr, SourceType.Unit, value.span), false, true)
        case value: UntypedFor =>
          forExpr(value, scope, context)
        case value: UntypedMatch =>
          matchExpr(value, scope, expected, context)
        case value: UntypedReturn =>
          returnExpr(value, scope, context)
        case value: UntypedBreak =>
          ExprInfo(TypedBreak(SourceType.Never, value.span), false, false)
        case value: UntypedContinue =>
          ExprInfo(TypedContinue(SourceType.Never, value.span), false, false)
        case value: UntypedBoolLiteral =>
          ExprInfo(TypedBoolLiteral(value.value, SourceType.Bool, value.span), false, false)
        case value: UntypedIntLiteral =>
          val valueType = expected.filter(SourceType.isInteger).getOrElse(SourceType.I32)
          ExprInfo(TypedIntLiteral(value.value, valueType, value.span), false, false)
        case value: UntypedAsciiLiteral =>
          ExprInfo(TypedIntLiteral(asciiLiteralValue(value.value, value.span), SourceType.Byte, value.span), false, false)
        case value: UntypedRuneLiteral =>
          ExprInfo(TypedIntLiteral(runeLiteralValue(value.value, value.span), SourceType.Rune, value.span), false, false)
        case value: UntypedFloatLiteral =>
          val valueType = expected.filter(SourceType.isFloat).getOrElse(SourceType.F64)
          ExprInfo(TypedFloatLiteral(value.value, valueType, value.span), false, false)
        case value: UntypedStringLiteral =>
          ExprInfo(TypedStringLiteral(value.value, SourceType.String, value.span), false, false)
        case value: UntypedUnitLiteral =>
          ExprInfo(TypedUnitLiteral(SourceType.Unit, value.span), false, false)

    private def blockExpr(
        node: UntypedBlock,
        outerScope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      val scope = outerScope.child
      val items = node.items.zipWithIndex.map { case (item, index) =>
        val itemExpected =
          if index == node.items.length - 1 then expected else None
        blockItem(item, scope, itemExpected, context)
      }
      val valueType = items.lastOption match
        case Some(expr: TypedExpr) => expr.valueType
        case _                     => SourceType.Unit
      ExprInfo(TypedBlock(items, valueType, node.span), mutableBinding = false, mutationAllowed = true)

    private def blockItem(
        item: UntypedBlockItem,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): TypedBlockItem =
      item match
        case local: UntypedLocal =>
          val explicitType = local.valueType.map(resolveType(_, context.ownerName))
          val init = local.init.map(expr(_, scope, explicitType, context))
          val valueType =
            explicitType.orElse(init.map(_.expr.valueType)).getOrElse {
              error(
                "cosmo0.type.missing-annotation",
                s"local ${local.name} requires a type annotation or initializer",
                local.span,
              )
              SourceType.Error
            }
          init.foreach(checkAssignable(_, valueType, local.span, "cosmo0.type.assignment-mismatch"))
          scope.define(valueSymbol(local.name, valueType, local.kind, local.span))
          TypedLocal(local.kind, local.name, valueType, init.map(_.expr), local.span)
        case stmt: UntypedExprStmt =>
          val typed = expr(stmt.expr, scope, expected, context)
          TypedExprStmt(typed.expr, stmt.span)
        case expression: UntypedExpr =>
          val typed = expr(expression, scope, expected, context)
          typed.expr

    private def nameExpr(node: UntypedName, scope: Scope): ExprInfo =
      node.path.parts match
        case name :: Nil =>
          scope.resolve(name) match
            case Some(symbol) =>
              ExprInfo(
                TypedName(node.path, symbol.valueType, symbol.mutableBinding, symbol.mutationAllowed, node.span),
                symbol.mutableBinding,
                symbol.mutationAllowed,
              )
            case None =>
              classConstructor(name, node.span).orElse(descriptorConstructor(name, node.span)) match
                case Some(signature) =>
                  val callee =
                    SourceType.dealias(signature.returnType) match
                      case owner: SourceType.User =>
                        TypedTypeConstructorExpr(owner, signature.functionType, node.span)
                      case _ =>
                        descriptorOwner(name) match
                          case Some(owner) if SourceType.same(signature.returnType, owner) =>
                            TypedTypeConstructorExpr(owner, signature.functionType, node.span)
                          case _ =>
                            TypedName(node.path, signature.functionType, false, false, node.span)
                  ExprInfo(callee, false, false)
                case None =>
                  error(
                    "cosmo0.type.unresolved-name",
                    s"unresolved name ${node.path.text}",
                    node.span,
                  )
                  ExprInfo(TypedName(node.path, SourceType.Error, false, false, node.span), false, false)
        case _ =>
          error(
            "cosmo0.type.unresolved-name",
            s"unresolved name ${node.path.text}",
            node.span,
          )
          ExprInfo(TypedName(node.path, SourceType.Error, false, false, node.span), false, false)

    private def typeConstructorExpr(node: UntypedTypeConstructor): ExprInfo =
      val constructedType = resolveType(node.valueType, None)
      val valueType = SourceType.dealias(constructedType) match
        case owner @ SourceType.Standard(name, _) =>
          standardGenerics
            .get(name)
            .flatMap(_.constructor("<init>"))
            .map(_.instantiate(owner, node.span).functionType)
            .getOrElse(SourceType.Error)
        case _ => SourceType.Error
      ExprInfo(TypedTypeConstructorExpr(constructedType, valueType, node.span), false, false)

    private def selectExpr(
        node: UntypedSelect,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      node.receiver match
        case UntypedName(path, _) if path.parts.length == 1 =>
          val ownerName = path.parts.head
          classes.get(ownerName).flatMap(_.variants.get(node.field)) match
            case Some(variant) =>
              val signature = variant.signature(ownerName)
              val valueType =
                if signature.params.isEmpty then signature.returnType else signature.functionType
              return ExprInfo(
                TypedVariantConstructorExpr(SourceType.User(ownerName), node.field, valueType, node.span),
                mutableBinding = false,
                mutationAllowed = false,
              )
            case None =>
        case _ =>

      val receiver = expr(node.receiver, scope, None, context)
      SourceType.dealias(receiver.expr.valueType) match
        case owner if descriptorMethod(owner, node.field, node.span).nonEmpty =>
          val method = descriptorMethod(owner, node.field, node.span).get
          val signature = method.instantiate(normalizeDescriptorOwner(owner), node.span)
          ExprInfo(
            TypedSelect(
              receiver.expr,
              node.field,
              signature.functionType,
              mutableBinding = false,
              mutationAllowed = false,
              node.span,
            ),
            mutableBinding = false,
            mutationAllowed = false,
          )
        case ty =>
          classInfoFor(ty) match
            case Some(cls) =>
              cls.fields.find(_.name == node.field) match
                case Some(field) =>
                  val fieldMutationAllowed = receiver.mutationAllowed && mutationCapability(field.valueType)
                  val mutableBinding =
                    field.kind == UntypedValueKind.Var && receiver.mutationAllowed
                  ExprInfo(
                    TypedSelect(
                      receiver.expr,
                      node.field,
                      field.valueType,
                      mutableBinding,
                      fieldMutationAllowed,
                      node.span,
                    ),
                    mutableBinding,
                    fieldMutationAllowed,
                  )
                case None =>
                  cls.methods.get(node.field) match
                    case Some(method) =>
                      ExprInfo(
                        TypedSelect(
                          receiver.expr,
                          node.field,
                          method.signature.functionType,
                          mutableBinding = false,
                          mutationAllowed = false,
                          node.span,
                        ),
                        false,
                        false,
                      )
                    case None =>
                      invalidField(node.field, receiver.expr.valueType, node.span)
            case None =>
              invalidField(node.field, receiver.expr.valueType, node.span)

    private def invalidField(field: String, receiverType: SourceType, span: SourceSpan): ExprInfo =
      error(
        "cosmo0.type.invalid-field",
        s"type ${receiverType.display} has no field or method $field",
        span,
      )
      ExprInfo(
        TypedSelect(
          TypedUnitLiteral(SourceType.Unit, span),
          field,
          SourceType.Error,
          mutableBinding = false,
          mutationAllowed = false,
          span,
        ),
        mutableBinding = false,
        mutationAllowed = false,
      )

    private def variantConstructorExpr(node: UntypedVariantConstructor): ExprInfo =
      val ownerType = resolveType(node.owner, None)
      constructorSignature(ownerType, node.variant, node.span) match
        case Some(signature) =>
          val valueType =
            if signature.params.isEmpty then signature.returnType else signature.functionType
          ExprInfo(
            TypedVariantConstructorExpr(ownerType, node.variant, valueType, node.span),
            mutableBinding = false,
            mutationAllowed = false,
          )
        case None =>
          error(
            "cosmo0.type.invalid-field",
            s"type ${ownerType.display} has no variant constructor ${node.variant}",
            node.span,
          )
          ExprInfo(
            TypedVariantConstructorExpr(ownerType, node.variant, SourceType.Error, node.span),
            false,
            false,
          )

    private def callExpr(
        node: UntypedCall,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      node.callee match
        case select: UntypedSelect =>
          methodOrVariantCall(select, node.args, node.span, scope, expected, context)
        case constructor: UntypedVariantConstructor =>
          val ownerType = resolveType(constructor.owner, context.ownerName)
          constructorSignature(ownerType, constructor.variant, constructor.span) match
            case Some(signature) =>
              callWithSignature(
                TypedVariantConstructorExpr(ownerType, constructor.variant, signature.functionType, constructor.span),
                signature,
                node.args,
                node.span,
                scope,
                context,
              )
            case None =>
              error(
                "cosmo0.type.invalid-call",
                s"type ${ownerType.display} has no variant constructor ${constructor.variant}",
                constructor.span,
              )
              errorCall(node, scope, context)
        case constructor: UntypedTypeConstructor =>
          val constructedType = resolveType(constructor.valueType, context.ownerName)
          SourceType.dealias(constructedType) match
            case owner @ SourceType.Standard(name, _) =>
              standardGenerics.get(name).flatMap(_.constructor("<init>")) match
                case Some(descriptor) =>
                  val signature = descriptor.instantiate(owner, constructor.span)
                  callWithSignature(
                    TypedTypeConstructorExpr(constructedType, signature.functionType, constructor.span),
                    signature,
                    node.args,
                    node.span,
                    scope,
                    context,
                  )
                case None =>
                  error(
                    "cosmo0.type.invalid-call",
                    s"type ${constructedType.display} is not directly constructible",
                    constructor.span,
                  )
                  errorCall(node, scope, context)
            case _ =>
              error(
                "cosmo0.type.invalid-call",
                s"type ${constructedType.display} is not directly constructible",
                constructor.span,
              )
              errorCall(node, scope, context)
        case name: UntypedName if name.path.parts.length == 1 =>
          val calleeName = name.path.parts.head
          functions.get(calleeName) match
            case Some(info) =>
              callWithSignature(
                TypedName(name.path, info.signature.functionType, false, false, name.span),
                info.signature,
                node.args,
                node.span,
                scope,
                context,
              )
            case None =>
              if isRuntimeFunction(calleeName) then
                runtimeFunctionCall(calleeName, name, node.args, node.span, scope, context)
              else classConstructor(calleeName, name.span).orElse(descriptorConstructor(calleeName, name.span)) match
                case Some(signature) =>
                  val callee =
                    SourceType.dealias(signature.returnType) match
                      case owner: SourceType.User =>
                        TypedTypeConstructorExpr(owner, signature.functionType, name.span)
                      case _ =>
                        descriptorOwner(calleeName) match
                          case Some(owner) if SourceType.same(signature.returnType, owner) =>
                            TypedTypeConstructorExpr(owner, signature.functionType, name.span)
                          case _ =>
                            TypedName(name.path, signature.functionType, false, false, name.span)
                  callWithSignature(
                    callee,
                    signature,
                    node.args,
                    node.span,
                    scope,
                    context,
                  )
                case None =>
                  val callee = expr(node.callee, scope, None, context)
                  callFunctionValue(callee, node.args, node.span, scope, context)
        case _ =>
          val callee = expr(node.callee, scope, None, context)
          callFunctionValue(callee, node.args, node.span, scope, context)

    private def methodOrVariantCall(
        select: UntypedSelect,
        args: List[UntypedExpr],
        span: SourceSpan,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      select.receiver match
        case UntypedName(path, _) if path.parts.length == 1 =>
          val ownerName = path.parts.head
          classes.get(ownerName).flatMap(_.variants.get(select.field)) match
            case Some(variant) =>
              val signature = variant.signature(ownerName)
              return callWithSignature(
                TypedVariantConstructorExpr(SourceType.User(ownerName), select.field, signature.functionType, select.span),
                signature,
                args,
                span,
                scope,
                context,
              )
            case None =>
        case _ =>

      val receiver = expr(select.receiver, scope, None, context)
      SourceType.dealias(receiver.expr.valueType) match
        case owner if descriptorMethod(owner, select.field, select.span).nonEmpty =>
          val method = descriptorMethod(owner, select.field, select.span).get
          val signature = method.instantiate(normalizeDescriptorOwner(owner), select.span)
          checkReceiverMutation(receiver, signature, select.span)
          callWithSignature(
            TypedSelect(receiver.expr, select.field, signature.functionType, false, false, select.span),
            signature,
            args,
            span,
            scope,
            context,
          )
        case ty =>
          classInfoFor(ty) match
            case Some(cls) =>
              cls.methods.get(select.field) match
                case Some(method) =>
                  checkReceiverMutation(receiver, method.signature, select.span)
                  callWithSignature(
                    TypedSelect(receiver.expr, select.field, method.signature.functionType, false, false, select.span),
                    method.signature,
                    args,
                    span,
                    scope,
                    context,
                  )
                case None =>
                  val selected = selectExpr(select, scope, context)
                  callFunctionValue(selected, args, span, scope, context)
            case None =>
              val selected = selectExpr(select, scope, context)
              callFunctionValue(selected, args, span, scope, context)

    private def callFunctionValue(
        callee: ExprInfo,
        args: List[UntypedExpr],
        span: SourceSpan,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      SourceType.dealias(callee.expr.valueType) match
        case SourceType.Function(params, returnType) =>
          val signature = CallableSignature(
            "<function>",
            params.zipWithIndex.map { case (paramType, index) =>
              CallableParam(s"arg$index", paramType, span)
            },
            returnType,
          )
          callWithSignature(callee.expr, signature, args, span, scope, context)
        case other =>
          error(
            "cosmo0.type.invalid-call",
            s"type ${other.display} is not callable",
            span,
          )
          val typedArgs = args.map(expr(_, scope, None, context).expr)
          ExprInfo(
            TypedCall(callee.expr, typedArgs, SourceType.Error, CallableSignature("<error>", Nil, SourceType.Error), span),
            false,
            false,
          )

    private def callWithSignature(
        callee: TypedExpr,
        signature: CallableSignature,
        args: List[UntypedExpr],
        span: SourceSpan,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      if args.length != signature.params.length then
        error(
          "cosmo0.type.wrong-arity",
          s"${signature.name} expects ${signature.params.length} argument(s), got ${args.length}",
          span,
        )
      val typedArgs = args.zipWithIndex.map { case (arg, index) =>
        val expectedType = signature.params.lift(index).map(_.valueType)
        val typed = expr(arg, scope, expectedType, context)
        expectedType.foreach(paramType =>
          if !canPass(typed, paramType) then
            error(
              "cosmo0.type.invalid-call",
              s"argument ${index + 1} has type ${typed.expr.valueType.display}, expected ${paramType.display}",
              arg.span,
            ),
        )
        typed.expr
      }
      ExprInfo(
        TypedCall(callee, typedArgs, signature.returnType, signature, span),
        mutableBinding = false,
        mutationAllowed = mutationCapability(signature.returnType),
      )

    private def errorCall(
        node: UntypedCall,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      val callee = expr(node.callee, scope, None, context)
      val args = node.args.map(expr(_, scope, None, context).expr)
      ExprInfo(
        TypedCall(
          callee.expr,
          args,
          SourceType.Error,
          CallableSignature("<error>", Nil, SourceType.Error),
          node.span,
        ),
        false,
        false,
      )

    private def runtimeFunctionCall(
        calleeName: String,
        name: UntypedName,
        args: List[UntypedExpr],
        span: SourceSpan,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      val signature = TrustedExternAbi.callable(calleeName, name.span).getOrElse(
        CallableSignature(calleeName, Nil, SourceType.Error),
      )
      if args.length != signature.params.length then
        error(
          "cosmo0.type.wrong-arity",
          s"$calleeName expects ${signature.params.length} argument(s), got ${args.length}",
          span,
        )
      val typedArgs = args.zipWithIndex.map { case (arg, index) =>
        expr(arg, scope, signature.params.lift(index).map(_.valueType), context).expr
      }
      typedArgs.zip(signature.params).zipWithIndex.foreach { case ((actual, param), index) =>
        val paramType = param.valueType
        if !SourceType.assignable(actual.valueType, paramType) then
          error(
            "cosmo0.type.invalid-call",
            s"argument ${index + 1} has type ${actual.valueType.display}, expected ${paramType.display}",
            args(index).span,
          )
      }
      ExprInfo(
        TypedCall(
          TypedName(name.path, signature.functionType, false, false, name.span),
          typedArgs,
          signature.returnType,
          signature,
          span,
        ),
        mutableBinding = false,
        mutationAllowed = mutationCapability(signature.returnType),
      )

    private def isRuntimeFunction(name: String): Boolean =
      TrustedExternAbi.isTrustedSourceName(name)

    private def assignExpr(
        node: UntypedAssign,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      val target = expr(node.target, scope, None, context)
      if !target.mutableBinding then
        error(
          "cosmo0.type.invalid-mutability",
          "assignment target is not mutable",
          node.target.span,
        )
      val valueExpected =
        if node.op == "=" then Some(target.expr.valueType) else Some(target.expr.valueType)
      val value = expr(node.value, scope, valueExpected, context)
      if !SourceType.assignable(value.expr.valueType, target.expr.valueType) then
        error(
          "cosmo0.type.assignment-mismatch",
          s"cannot assign ${value.expr.valueType.display} to ${target.expr.valueType.display}",
          node.span,
        )
      if node.op != "=" && !SourceType.isNumeric(target.expr.valueType) then
        error(
          "cosmo0.type.assignment-mismatch",
          s"operator ${node.op} requires a numeric target",
          node.span,
        )
      ExprInfo(TypedAssign(target.expr, value.expr, node.op, SourceType.Unit, node.span), false, true)

    private def unaryExpr(
        node: UntypedUnary,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      node.op match
        case "!" =>
          val value = expr(node.expr, scope, Some(SourceType.Bool), context)
          requireBool(value, node.expr.span)
          ExprInfo(TypedUnary(node.op, value.expr, SourceType.Bool, node.span), false, false)
        case "-" =>
          val value = expr(node.expr, scope, expected.filter(SourceType.isNumeric), context)
          if !SourceType.isNumeric(value.expr.valueType) then
            error(
              "cosmo0.type.invalid-unary",
              s"operator - requires a numeric operand, got ${value.expr.valueType.display}",
              node.span,
            )
          ExprInfo(TypedUnary(node.op, value.expr, value.expr.valueType, node.span), false, false)
        case "&" =>
          val value = expr(node.expr, scope, None, context)
          ExprInfo(
            TypedUnary(node.op, value.expr, SourceType.Ref(value.expr.valueType, mutable = false), node.span),
            false,
            false,
          )
        case "*" =>
          val value = expr(node.expr, scope, None, context)
          SourceType.dealias(value.expr.valueType) match
            case SourceType.Ref(target, mutable) =>
              ExprInfo(TypedUnary(node.op, value.expr, target, node.span), mutable, mutable)
            case other =>
              error(
                "cosmo0.type.invalid-unary",
                s"operator * requires a reference, got ${other.display}",
                node.span,
              )
              ExprInfo(TypedUnary(node.op, value.expr, SourceType.Error, node.span), false, false)
        case other =>
          val value = expr(node.expr, scope, None, context)
          error(
            "cosmo0.type.invalid-unary",
            s"unsupported unary operator $other",
            node.span,
          )
          ExprInfo(TypedUnary(other, value.expr, SourceType.Error, node.span), false, false)

    private def binaryExpr(
        node: UntypedBinary,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      node.op match
        case "and" | "or" | "&&" | "||" =>
          val left = expr(node.left, scope, Some(SourceType.Bool), context)
          val right = expr(node.right, scope, Some(SourceType.Bool), context)
          requireBool(left, node.left.span)
          requireBool(right, node.right.span)
          ExprInfo(TypedBinary(node.op, left.expr, right.expr, SourceType.Bool, node.span), false, false)
        case "==" | "!=" =>
          val left = expr(node.left, scope, None, context)
          val right = expr(node.right, scope, Some(left.expr.valueType), context)
          if !SourceType.same(left.expr.valueType, right.expr.valueType) then
            error(
              "cosmo0.type.invalid-binary",
              s"operator ${node.op} requires comparable operands, got ${left.expr.valueType.display} and ${right.expr.valueType.display}",
              node.span,
            )
          ExprInfo(TypedBinary(node.op, left.expr, right.expr, SourceType.Bool, node.span), false, false)
        case "<" | "<=" | ">" | ">=" =>
          val left = expr(node.left, scope, expected.filter(SourceType.isNumeric), context)
          val right = expr(node.right, scope, Some(left.expr.valueType), context)
          if !SourceType.isNumeric(left.expr.valueType) || !SourceType.same(left.expr.valueType, right.expr.valueType) then
            error(
              "cosmo0.type.invalid-binary",
              s"operator ${node.op} requires matching numeric operands",
              node.span,
            )
          ExprInfo(TypedBinary(node.op, left.expr, right.expr, SourceType.Bool, node.span), false, false)
        case "+" | "-" | "*" | "/" | "%" =>
          val left = expr(node.left, scope, expected.filter(SourceType.isNumeric), context)
          val right = expr(node.right, scope, Some(left.expr.valueType), context)
          if !SourceType.isNumeric(left.expr.valueType) || !SourceType.same(left.expr.valueType, right.expr.valueType) then
            error(
              "cosmo0.type.invalid-binary",
              s"operator ${node.op} requires matching numeric operands",
              node.span,
            )
          ExprInfo(TypedBinary(node.op, left.expr, right.expr, left.expr.valueType, node.span), false, false)
        case other =>
          val left = expr(node.left, scope, None, context)
          val right = expr(node.right, scope, Some(left.expr.valueType), context)
          error(
            "cosmo0.type.invalid-binary",
            s"unsupported binary operator $other",
            node.span,
          )
          ExprInfo(TypedBinary(other, left.expr, right.expr, SourceType.Error, node.span), false, false)

    private def ifExpr(
        node: UntypedIf,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      val cond = expr(node.cond, scope, Some(SourceType.Bool), context)
      requireBool(cond, node.cond.span)
      val thenBranch = expr(node.thenBranch, scope.child, expected, context)
      val elseBranch = node.elseBranch.map(expr(_, scope.child, expected, context))
      val valueType = elseBranch match
        case Some(other) if SourceType.same(thenBranch.expr.valueType, other.expr.valueType) =>
          thenBranch.expr.valueType
        case Some(other) =>
          error(
            "cosmo0.type.branch-mismatch",
            s"if branches have types ${thenBranch.expr.valueType.display} and ${other.expr.valueType.display}",
            node.span,
          )
          SourceType.Error
        case None => SourceType.Unit
      ExprInfo(TypedIf(cond.expr, thenBranch.expr, elseBranch.map(_.expr), valueType, node.span), false, mutationCapability(valueType))

    private def forExpr(
        node: UntypedFor,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      val iter = expr(node.iter, scope, None, context)
      val itemType = SourceType.dealias(iter.expr.valueType) match
        case SourceType.Standard("Vec" | "Set" | "Arena", item :: Nil) => item
        case SourceType.Standard("Map", key :: _ :: Nil)               => key
        case other =>
          error(
            "cosmo0.type.invalid-iterator",
            s"type ${other.display} is not iterable in cosmo0",
            node.iter.span,
          )
          SourceType.Error
      val bodyScope = scope.child
      bodyScope.define(ValueSymbol(node.name, itemType, false, mutationCapability(itemType), node.span))
      val body = expr(node.body, bodyScope, Some(SourceType.Unit), context)
      ExprInfo(TypedFor(node.name, itemType, iter.expr, body.expr, SourceType.Unit, node.span), false, true)

    private def matchExpr(
        node: UntypedMatch,
        scope: Scope,
        expected: Option[SourceType],
        context: FunctionContext,
    ): ExprInfo =
      val scrutinee = expr(node.scrutinee, scope, None, context)
      val arms = node.arms.map { arm =>
        val armScope = scope.child
        val typedPattern = pattern(arm.pattern, scrutinee.expr.valueType, armScope)
        val body = arm.body.map(expr(_, armScope, expected, context))
        TypedMatchArm(typedPattern, body.map(_.expr), arm.span)
      }
      val bodyTypes = arms.flatMap(_.body.map(_.valueType))
      val valueType =
        if bodyTypes.isEmpty then SourceType.Unit
        else
          val first = bodyTypes.head
          if bodyTypes.tail.forall(valueType => SourceType.same(first, valueType)) then first
          else
            error(
              "cosmo0.type.branch-mismatch",
              "match arms must produce the same type",
              node.span,
            )
            SourceType.Error
      ExprInfo(TypedMatch(scrutinee.expr, arms, valueType, node.span), false, mutationCapability(valueType))

    private def returnExpr(
        node: UntypedReturn,
        scope: Scope,
        context: FunctionContext,
    ): ExprInfo =
      context match
        case FunctionContext.Some(returnType, _) =>
          val value = expr(node.value, scope, Some(returnType), context)
          if !SourceType.assignable(value.expr.valueType, returnType) then
            error(
              "cosmo0.type.return-mismatch",
              s"return has type ${value.expr.valueType.display}, expected ${returnType.display}",
              node.span,
            )
          ExprInfo(TypedReturn(value.expr, SourceType.Never, node.span), false, false)
        case FunctionContext.None =>
          val value = expr(node.value, scope, None, context)
          error(
            "cosmo0.type.invalid-return",
            "return can only appear inside a function",
            node.span,
          )
          ExprInfo(TypedReturn(value.expr, SourceType.Never, node.span), false, false)

    private def pattern(
        node: UntypedPattern,
        expectedType: SourceType,
        scope: Scope,
    ): TypedPattern =
      node match
        case value: UntypedWildcardPattern =>
          TypedWildcardPattern(expectedType, value.span)
        case value: UntypedBindingPattern =>
          scope.define(
            ValueSymbol(
              value.name,
              expectedType,
              mutableBinding = false,
              mutationAllowed = mutationCapability(expectedType),
              value.span,
            ),
          )
          TypedBindingPattern(value.name, expectedType, value.span)
        case value: UntypedBoolLiteral =>
          if !SourceType.same(expectedType, SourceType.Bool) then
            invalidPatternType(value.span, expectedType, SourceType.Bool)
          TypedBoolLiteral(value.value, SourceType.Bool, value.span)
        case value: UntypedIntLiteral =>
          val valueType =
            if SourceType.isInteger(expectedType) then expectedType else SourceType.I32
          if !SourceType.isInteger(expectedType) then
            invalidPatternType(value.span, expectedType, valueType)
          TypedIntLiteral(value.value, valueType, value.span)
        case value: UntypedAsciiLiteral =>
          if !SourceType.same(expectedType, SourceType.Byte) then
            invalidPatternType(value.span, expectedType, SourceType.Byte)
          TypedIntLiteral(asciiLiteralValue(value.value, value.span), SourceType.Byte, value.span)
        case value: UntypedRuneLiteral =>
          if !SourceType.same(expectedType, SourceType.Rune) then
            invalidPatternType(value.span, expectedType, SourceType.Rune)
          TypedIntLiteral(runeLiteralValue(value.value, value.span), SourceType.Rune, value.span)
        case value: UntypedFloatLiteral =>
          val valueType =
            if SourceType.isFloat(expectedType) then expectedType else SourceType.F64
          if !SourceType.isFloat(expectedType) then
            invalidPatternType(value.span, expectedType, valueType)
          TypedFloatLiteral(value.value, valueType, value.span)
        case value: UntypedStringLiteral =>
          if !SourceType.same(expectedType, SourceType.String) then
            invalidPatternType(value.span, expectedType, SourceType.String)
          TypedStringLiteral(value.value, SourceType.String, value.span)
        case value: UntypedVariantPattern =>
          variantPattern(value, expectedType, scope)

    private def variantPattern(
        node: UntypedVariantPattern,
        expectedType: SourceType,
        scope: Scope,
    ): TypedPattern =
      val constructor = constructorExprForPattern(node.constructor, expectedType)
      val signature = constructor.flatMap { case (ownerType, variantName, expr) =>
        constructorSignature(ownerType, variantName, expr.span).map(signature => (signature, expr))
      }
      signature match
        case Some((callable, constructorExpr)) =>
          if !SourceType.same(callable.returnType, expectedType) then
            error(
              "cosmo0.type.invalid-match-payload",
              s"pattern constructor returns ${callable.returnType.display}, expected ${expectedType.display}",
              node.span,
            )
          if node.args.length != callable.params.length then
            error(
              "cosmo0.type.wrong-arity",
              s"pattern expects ${callable.params.length} payload(s), got ${node.args.length}",
              node.span,
            )
          val args = node.args.zipWithIndex.map { case (arg, index) =>
            val argType = callable.params.lift(index).map(_.valueType).getOrElse(SourceType.Error)
            pattern(arg, argType, scope)
          }
          TypedVariantPattern(constructorExpr, args, callable.returnType, node.span)
        case None =>
          error(
            "cosmo0.type.invalid-match-payload",
            "invalid variant pattern",
            node.span,
          )
          TypedVariantPattern(
            TypedVariantConstructorExpr(expectedType, "<error>", SourceType.Error, node.span),
            Nil,
            SourceType.Error,
            node.span,
          )

    private def constructorExprForPattern(
        node: UntypedExpr,
        expectedType: SourceType,
    ): Option[(SourceType, String, TypedExpr)] =
      node match
        case UntypedVariantConstructor(owner, variant, span) =>
          val ownerType = resolveType(owner, None)
          Some(
            (
              ownerType,
              variant,
              TypedVariantConstructorExpr(ownerType, variant, SourceType.Function(Nil, ownerType), span),
            ),
          )
        case UntypedSelect(UntypedName(path, _), variant, span) if path.parts.length == 1 =>
          val ownerType = SourceType.User(path.parts.head)
          Some(
            (
              ownerType,
              variant,
              TypedVariantConstructorExpr(ownerType, variant, SourceType.Function(Nil, ownerType), span),
            ),
          )
        case _ =>
          None

    private def constructorSignature(
        ownerType: SourceType,
        variant: String,
        span: SourceSpan,
    ): Option[CallableSignature] =
      SourceType.dealias(ownerType) match
        case owner @ SourceType.Standard(name, _) =>
          standardGenerics.get(name).flatMap(_.constructor(variant)).map(_.instantiate(owner, span))
        case SourceType.User(name) =>
          classes.get(name).flatMap(_.variants.get(variant)).map(_.signature(name))
        case _ => None

    private def descriptorConstructor(
        name: String,
        span: SourceSpan,
    ): Option[CallableSignature] =
      descriptorOwner(name).flatMap(owner =>
        standardGenerics.get(name).flatMap(_.constructor("<init>")).map(_.instantiate(owner, span)),
      )

    private def descriptorMethod(
        ownerType: SourceType,
        methodName: String,
        span: SourceSpan,
    ): Option[DescriptorCallable] =
      val owner = normalizeDescriptorOwner(ownerType)
      descriptorName(owner).flatMap(name =>
        standardGenerics.get(name).filter(_.arity == descriptorArity(owner)).flatMap(_.method(methodName)),
      )

    private def descriptorOwner(name: String): Option[SourceType] =
      standardGenerics.get(name).filter(_.arity == 0).flatMap { _ =>
        SourceType.scalar(name)
      }

    private def normalizeDescriptorOwner(ownerType: SourceType): SourceType =
      SourceType.dealias(ownerType) match
        case SourceType.Ref(target, _) => SourceType.dealias(target)
        case other                     => other

    private def descriptorName(ownerType: SourceType): Option[String] =
      normalizeDescriptorOwner(ownerType) match
        case SourceType.Standard(name, _) => Some(name)
        case SourceType.Builtin(name)     => Some(name)
        case _                            => None

    private def descriptorArity(ownerType: SourceType): Int =
      normalizeDescriptorOwner(ownerType) match
        case SourceType.Standard(_, args) => args.length
        case SourceType.Builtin(_)        => 0
        case _                            => -1

    private def classConstructor(
        name: String,
        span: SourceSpan,
    ): Option[CallableSignature] =
      classes.get(name).map(_.constructorSignature)

    private def classInfoFor(valueType: SourceType): Option[ClassInfo] =
      SourceType.dealias(valueType) match
        case SourceType.User(name)          => classes.get(name)
        case SourceType.Ref(SourceType.User(name), _) => classes.get(name)
        case _                              => None

    private def resolveAlias(name: String): SourceType =
      resolveAlias(name, Set.empty)

    private def resolveAlias(name: String, seen: Set[String]): SourceType =
      val resolved = rawAliases.get(name) match
        case Some(target) if seen.contains(name) =>
          error(
            "cosmo0.type.recursive-alias",
            s"type alias $name is recursive",
            target.span,
          )
          SourceType.Error
        case Some(target) =>
          resolveType(target, None, seen + name)
        case None =>
          SourceType.Error
      aliasTypes.getOrElseUpdate(name, resolved)

    private def resolveType(
        node: UntypedType,
        owner: Option[String],
    ): SourceType =
      resolveType(node, owner, Set.empty)

    private def resolveType(
        node: UntypedType,
        owner: Option[String],
        seenAliases: Set[String],
    ): SourceType =
      node match
        case UntypedNamedType(path, span) =>
          path.parts match
            case name :: Nil if owner.contains(name) || ((name == "self" || name == "Self") && owner.nonEmpty) =>
              SourceType.User(owner.get)
            case name :: Nil =>
              SourceType.scalar(name)
                .orElse(
                  rawAliases.get(name).map(_ =>
                    SourceType.Alias(name, resolveAlias(name, seenAliases)),
                  ),
                )
                .orElse(
                  if classNames.contains(name) then Some(SourceType.User(name)) else None,
                )
                .getOrElse {
                  error(
                    "cosmo0.type.unknown-type",
                    s"unknown type ${path.text}",
                    span,
                  )
                  SourceType.Error
                }
            case _ =>
              error(
                "cosmo0.type.unknown-type",
                s"unknown type ${path.text}",
                span,
              )
              SourceType.Error
        case UntypedAppliedType(base, args, span) =>
          val name = base.parts.lastOption.getOrElse(base.text)
          standardGenerics.get(name) match
            case Some(descriptor) =>
              if descriptor.arity != args.length then
                error(
                  "cosmo0.type.wrong-arity",
                  s"$name expects ${descriptor.arity} type argument(s), got ${args.length}",
                  span,
                )
              val resolvedArgs = args.map(resolveType(_, owner, seenAliases))
              validateStandardTypeApplication(name, resolvedArgs, span)
              SourceType.Standard(name, resolvedArgs)
            case None =>
              error(
                "cosmo0.type.unknown-type",
                s"unknown standard generic type ${base.text}",
                span,
              )
              SourceType.Error
        case UntypedRefType(target, mutable, _) =>
          SourceType.Ref(resolveType(target, owner, seenAliases), mutable)

    private def validateStandardTypeApplication(
        name: String,
        args: List[SourceType],
        span: SourceSpan,
    ): Unit =
      name match
        case "Map" if args.length == 2 =>
          validateMapSetKey(args.head, "Map", span)
        case "Set" if args.length == 1 =>
          validateMapSetKey(args.head, "Set", span)
        case _ =>

    private def validateMapSetKey(
        keyType: SourceType,
        owner: String,
        span: SourceSpan,
    ): Unit =
      if !isSupportedMapSetKey(keyType) then
        error(
          "cosmo0.type.unsupported-map-key",
          s"$owner key type ${keyType.display} is not supported by core0.map-set; use String, a primitive integer, or Id[T]",
          span,
        )

    private def isSupportedMapSetKey(valueType: SourceType): Boolean =
      SourceType.dealias(valueType) match
        case SourceType.String => true
        case SourceType.Builtin("i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "usize") =>
          true
        case SourceType.Standard("Id", _ :: Nil) => true
        case _                                   => false

    private def canPass(actual: ExprInfo, expected: SourceType): Boolean =
      SourceType.dealias(expected) match
        case SourceType.Ref(target, mutable) =>
          SourceType.dealias(actual.expr.valueType) match
            case SourceType.Ref(actualTarget, actualMutable) =>
              SourceType.same(actualTarget, target) && (!mutable || actualMutable)
            case other =>
              SourceType.same(other, target) && (!mutable || actual.mutationAllowed)
        case other =>
          SourceType.assignable(actual.expr.valueType, other)

    private def checkAssignable(
        actual: ExprInfo,
        expected: SourceType,
        span: SourceSpan,
        code: String,
    ): Unit =
      if !SourceType.assignable(actual.expr.valueType, expected) then
        error(
          code,
          s"expected ${expected.display}, got ${actual.expr.valueType.display}",
          span,
        )

    private def checkReceiverMutation(
        receiver: ExprInfo,
        signature: CallableSignature,
        span: SourceSpan,
    ): Unit =
      signature.receiver.foreach { receiverInfo =>
        if receiverInfo.mutable && !receiver.mutationAllowed then
          error(
            "cosmo0.type.invalid-mutability",
            s"${signature.name} requires a mutable receiver",
            span,
          )
      }

    private def requireBool(value: ExprInfo, span: SourceSpan): Unit =
      if !SourceType.same(value.expr.valueType, SourceType.Bool) then
        error(
          "cosmo0.type.expected-bool",
          s"expected Bool, got ${value.expr.valueType.display}",
          span,
        )

    private def invalidPatternType(
        span: SourceSpan,
        expected: SourceType,
        actual: SourceType,
    ): Unit =
      error(
        "cosmo0.type.invalid-match-payload",
        s"pattern has type ${actual.display}, expected ${expected.display}",
        span,
      )

    private def valueSymbol(
        name: String,
        valueType: SourceType,
        kind: UntypedValueKind,
        span: SourceSpan,
    ): ValueSymbol =
      ValueSymbol(
        name,
        valueType,
        mutableBinding = kind == UntypedValueKind.Var,
        mutationAllowed = mutationCapability(valueType),
        span,
      )

    private def mutationCapability(valueType: SourceType): Boolean =
      SourceType.dealias(valueType) match
        case SourceType.Ref(_, mutable) => mutable
        case SourceType.Never | SourceType.Error => false
        case _ => true

    private def asciiLiteralValue(value: String, span: SourceSpan): BigInt =
      singleCodePoint(value, "ascii", span) match
        case Some(codePoint) if codePoint <= 0x7f =>
          BigInt(codePoint)
        case Some(_) =>
          error(
            "cosmo0.type.invalid-ascii-literal",
            "ASCII literals must contain exactly one ASCII code point",
            span,
          )
          BigInt(0)
        case None =>
          BigInt(0)

    private def runeLiteralValue(value: String, span: SourceSpan): BigInt =
      singleCodePoint(value, "rune", span).fold(BigInt(0))(BigInt(_))

    private def singleCodePoint(value: String, name: String, span: SourceSpan): Option[Int] =
      if value.isEmpty || value.codePointCount(0, value.length) != 1 then
        error(
          s"cosmo0.type.invalid-$name-literal",
          s"$name literals must contain exactly one Unicode code point",
          span,
        )
        None
      else
        val codePoint = value.codePointAt(0)
        if value.length == 1 && Character.isSurrogate(value.charAt(0)) then
          error(
            s"cosmo0.type.invalid-$name-literal",
            s"$name literals must contain a valid Unicode scalar value",
            span,
          )
          None
        else Some(codePoint)

    private def error(
        code: String,
        message: String,
        span: SourceSpan,
    ): Unit =
      diagnostics += Diagnostic(
        Phase.Check,
        DiagnosticSeverity.Error,
        code,
        message,
        Some(span),
      )

  private enum FunctionContext:
    case Some(returnType: SourceType, containingOwner: Option[String])
    case None

    def ownerName: Option[String] =
      this match
        case Some(_, containingOwner) => containingOwner
        case None                     => scala.None
