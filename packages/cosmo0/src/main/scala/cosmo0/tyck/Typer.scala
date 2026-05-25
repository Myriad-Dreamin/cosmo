package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Factory for the source-level checker.
  *
  * The no-profile entry point intentionally selects `cosmo0.subset`. MLTT and
  * dependent-pattern profiles are registered in `CheckerProfiles`, but the
  * public cosmo0 source pipeline does not elaborate ordinary source into those
  * artifacts yet.
  */
object SourceTyper:
  def apply(module: UntypedModule): SourceTyper =
    new SourceTyper(
      module,
      StandardGenericDescriptors.all,
      CheckerProfiles.Cosmo0Subset,
    )

  def apply(module: UntypedModule, profile: CheckerProfile): SourceTyper =
    new SourceTyper(module, StandardGenericDescriptors.all, profile)

/** Checks an `UntypedModule` into a `TypedModule` for the default cosmo0 source
  * language subset.
  *
  * Source language examples handled here:
  *
  * {{{
  * val answer = 42
  * var name: String = "cosmo"
  *
  * class Nat {
  *   case Zero
  *   case Succ(Nat)
  * }
  *
  * def inc(value: i32): i32 = { value + 1 }
  *
  * def pred(value: Nat): i32 = {
  *   value match {
  *     case Nat.Zero => 0
  *     case Nat.Succ(rest) => 1
  *   }
  * }
  * }}}
  *
  * The `profile` constructor parameter is metadata for diagnostics and future
  * routing. It is not a second dispatcher inside this class: today this
  * implementation checks the cosmo0 source subset, while public callers reject
  * non-`cosmo0.subset` profiles before reaching `SourceTyper`.
  *
  * The checker is bidirectional in a small, source-oriented sense: `expr(node,
  * expected, context)` infers a type when `expected` is absent, and uses
  * `expected` to guide literals, calls, branch bodies, assignments, returns,
  * and block final expressions.
  *
  * Inference and checking rules:
  *
  *   - Declarations: `val x: T = e` checks `e <= T`; `val x = e` infers `T`
  *     from `e`. Fields and parameters require explicit source types.
  *   - Functions: parameter and return annotations form a `CallableSignature`.
  *     A missing return annotation is `Unit`; the body is checked against the
  *     declared return type.
  *   - Names: lexical values are looked up first; otherwise a single-segment
  *     name may resolve to a function, class constructor, or standard generic
  *     constructor.
  *   - Literals: integer and float literals use an expected numeric type when
  *     one is available, defaulting to `i32` and `f64`; bool, string, unit,
  *     ASCII, and rune literals have fixed types.
  *   - Calls: the callee must have a `CallableSignature` or function type; each
  *     argument is checked against the matching parameter, including mutable
  *     reference passing rules.
  *   - Blocks: only the final item receives the outer expected type; the block
  *     type is the final expression type, or `Unit` for statement-only blocks.
  *   - `if` and `match`: conditions check as `Bool`; branch bodies must agree
  *     on one result type unless no branch body is present.
  *   - Patterns: a match pattern is checked against the scrutinee type. Binding
  *     patterns introduce locals of the expected type, and variant patterns
  *     check payloads against the selected constructor signature.
  *   - Mutation: assignable values require mutable bindings, and mutable
  *     receiver methods require a mutable receiver capability.
  */
final class SourceTyper(
    module: UntypedModule,
    standardGenerics: Map[String, StandardGenericDescriptor] =
      StandardGenericDescriptors.all,
    profile: CheckerProfile = CheckerProfiles.Cosmo0Subset,
):
  private final case class ValueSymbol(
      name: String,
      ty: SourceType,
      mutBinding: Boolean,
      mutAllowed: Boolean,
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
      mutBinding: Boolean,
      mutAllowed: Boolean,
  )

  private final case class FieldInfo(
      name: String,
      kind: UntypedValueKind,
      ty: SourceType,
      init: Option[UntypedExpr],
      span: SourceSpan,
  )

  private final case class VariantInfo(
      name: String,
      fields: List[TypedVariantField],
      span: SourceSpan,
  ):
    def sig(owner: String): CallableSignature =
      CallableSignature(
        name,
        fields.zipWithIndex.map { case (field, index) =>
          CallableParam(
            field.name.getOrElse(s"field$index"),
            field.ty,
            field.span,
          )
        },
        SourceType.User(owner),
      )

  private final case class ParamInfo(
      name: String,
      ty: SourceType,
      default: Option[UntypedExpr],
      span: SourceSpan,
  )

  private final case class FunctionInfo(
      name: String,
      params: List[ParamInfo],
      retTy: SourceType,
      body: Option[UntypedExpr],
      sig: CallableSignature,
      owner: Option[String],
      span: SourceSpan,
      extern: Option[SourceExternBinding],
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
    def ctorSig: CallableSignature =
      val params =
        fields.map(field => CallableParam(field.name, field.ty, field.span))
      CallableSignature(
        name,
        params,
        SourceType.User(name),
      )

  private val diagnostics = ListBuffer.empty[Diagnostic]
  private val classNames =
    module.decls.collect { case decl: UntypedClass => decl.name }.toSet
  private val rawAliases =
    mutable.LinkedHashMap.empty[String, UntypedTypeAlias]
  private val aliasTypes = mutable.LinkedHashMap.empty[String, SourceType]
  private val foreignAliases =
    mutable.LinkedHashMap.empty[String, SourceCppNamespaceImport]
  private val traits = mutable.LinkedHashMap.empty[String, TraitInfo]
  private val classes = mutable.LinkedHashMap.empty[String, ClassInfo]
  private val functions = mutable.LinkedHashMap.empty[String, FunctionInfo]

  def check(): Result[TypedModule] =
    collectForeignNamespaceImports()
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
          info.sig.functionType,
          mutBinding = false,
          mutAllowed = false,
          info.span,
        ),
      ),
    )
    foreignAliases.values.foreach(importValue =>
      globalScope.define(
        ValueSymbol(
          importValue.alias,
          SourceType.ForeignNamespace(importValue),
          mutBinding = false,
          mutAllowed = false,
          importValue.span,
        ),
      ),
    )

    val decls =
      module.decls.flatMap { decl =>
        typedDecl(decl, globalScope)
      }

    val result = TypedModule(
      module.source,
      decls,
      module.span,
      module.cIncludes,
      foreignAliases.values.toList,
    )
    if diagnostics.isEmpty then Result.success(Phase.Check, result)
    else Result.failure(Phase.Check, diagnostics.toList)

  private def collectForeignNamespaceImports(): Unit =
    val ordinaryBindings = mutable.LinkedHashMap.empty[String, SourceSpan]
    module.decls.foreach { declaration =>
      ordinaryBindingName(declaration).foreach { name =>
        if !ordinaryBindings.contains(name) then
          ordinaryBindings.update(name, declaration.span)
      }
    }

    val importValues =
      (module.decls.collect { case importDecl: UntypedCppNamespaceImport =>
        importDecl.value
      } :::
        module.cppImports).distinct

    importValues.foreach { importValue =>
      ordinaryBindings.get(importValue.alias).foreach { _ =>
        error(
          "cosmo1.name.duplicate-definition",
          s"C++ namespace alias ${importValue.alias} conflicts with an existing Cosmo binding",
          importValue.span,
        )
      }

      foreignAliases.get(importValue.alias) match
        case Some(existing) if existing.namespace == importValue.namespace =>
          val mergedHeaders =
            (existing.headers ::: importValue.headers).distinct
          foreignAliases.update(
            importValue.alias,
            existing.copy(headers = mergedHeaders),
          )
        case Some(existing) =>
          error(
            "cosmo1.name.conflicting-cpp-namespace-alias",
            s"C++ namespace alias ${importValue.alias} already targets ${existing.namespace.cppName}, not ${importValue.namespace.cppName}",
            importValue.span,
          )
        case None =>
          foreignAliases.update(importValue.alias, importValue)
    }

  private def ordinaryBindingName(declaration: UntypedDecl): Option[String] =
    declaration match
      case _: UntypedCppNamespaceImport =>
        None
      case importDecl: UntypedImport =>
        importDecl.dest
          .flatMap(_.parts.lastOption)
          .orElse(importDecl.path.parts.lastOption)
      case other =>
        Some(other.name)

  private def collectAliases(): Unit =
    module.decls.foreach {
      case alias: UntypedTypeAlias =>
        rawAliases.update(alias.name, alias)
      case cls: UntypedClass =>
        cls.members.foreach {
          case alias: UntypedTypeAlias =>
            rawAliases.update(alias.name, alias)
          case _ =>
        }
      case _ =>
    }

  private def collectTraits(): Unit =
    module.decls.collect { case trt: UntypedTrait => trt }.foreach { trt =>
      val methods =
        trt.methods.map(method => functionInfo(method, Some(trt.name)))
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
    module.decls.collect { case cls: UntypedClass => cls }.foreach { cls =>
      val fields = cls.members.collect { case field: UntypedValueDecl =>
        FieldInfo(
          field.name,
          field.kind,
          field.ty.map(resolveType(_, Some(cls.name))).getOrElse {
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
        TypedTypeAlias(
          alias.name,
          resolveAlias(alias.name),
          alias.span,
          alias.tyParams,
        )
      }
      val variants = cls.members.collect { case variant: UntypedVariant =>
        val fields = variant.fields.map(field =>
          TypedVariantField(
            field.name,
            resolveType(field.ty, Some(cls.name)),
            field.span,
          ),
        )
        variant.name -> VariantInfo(variant.name, fields, variant.span)
      }.toMap
      val methods = cls.members
        .collect { case fn: UntypedFunction =>
          functionInfo(fn, Some(cls.name))
        }
        .map(info => info.name -> info)
        .toMap

      classes.update(
        cls.name,
        ClassInfo(cls.name, fields, aliases, variants, methods, cls.span),
      )
    }

  private def collectImpls(): Unit =
    module.decls
      .collect { case impl: UntypedImpl => impl }
      .foreach(collectImpl)

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

    val freshMethods =
      methods.filterNot { method =>
        target.methods.contains(method.name)
      }
    val mergedMethods =
      target.methods ++ freshMethods.map(method => method.name -> method)
    classes.update(targetName, target.copy(methods = mergedMethods))

  private def validateTraitImplementation(
      impl: UntypedImpl,
      traitInfo: TraitInfo,
      targetInfo: ClassInfo,
      methods: List[FunctionInfo],
  ): Unit =
    duplicateMethodNames(
      methods,
      s"${impl.traitName.text} impl for ${targetInfo.name}",
      impl.span,
    )

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
          val specialized = specializeSig(
            expected.sig,
            traitInfo.name,
            targetInfo.name,
          )
          if !sameCallableSig(actual.sig, specialized) then
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

  private def specializeSig(
      sig: CallableSignature,
      traitName: String,
      targetName: String,
  ): CallableSignature =
    CallableSignature(
      sig.name,
      sig.params.map(param =>
        CallableParam(
          param.name,
          specializeSelfType(param.valueType, traitName, targetName),
          param.span,
        ),
      ),
      specializeSelfType(sig.returnType, traitName, targetName),
      sig.receiver.map(recv =>
        CallableReceiver(
          specializeSelfType(recv.valueType, traitName, targetName),
          recv.mutable,
        ),
      ),
    )

  private def specializeSelfType(
      ty: SourceType,
      traitName: String,
      targetName: String,
  ): SourceType =
    ty match
      case SourceType.User(name) if name == traitName =>
        SourceType.User(targetName)
      case SourceType.Ref(target, mutable) =>
        SourceType.Ref(
          specializeSelfType(target, traitName, targetName),
          mutable,
        )
      case SourceType.Standard(name, args) =>
        SourceType.Standard(
          name,
          args.map(specializeSelfType(_, traitName, targetName)),
        )
      case SourceType.Function(params, retTy) =>
        SourceType.Function(
          params.map(specializeSelfType(_, traitName, targetName)),
          specializeSelfType(retTy, traitName, targetName),
        )
      case SourceType.Alias(name, target) =>
        SourceType.Alias(
          name,
          specializeSelfType(target, traitName, targetName),
        )
      case other =>
        other

  private def sameCallableSig(
      left: CallableSignature,
      right: CallableSignature,
  ): Boolean =
    left.name == right.name &&
      sameRecv(left.receiver, right.receiver) &&
      left.params.length == right.params.length &&
      left.params.zip(right.params).forall { case (l, r) =>
        SourceType.same(l.valueType, r.valueType)
      } &&
      SourceType.same(left.returnType, right.returnType)

  private def sameRecv(
      left: Option[CallableReceiver],
      right: Option[CallableReceiver],
  ): Boolean =
    (left, right) match
      case (None, None) =>
        true
      case (Some(l), Some(r)) =>
        l.mutable == r.mutable && SourceType.same(l.valueType, r.valueType)
      case _ =>
        false

  private def collectFunctions(): Unit =
    module.decls.collect { case fn: UntypedFunction => fn }.foreach { fn =>
      val info = functionInfo(fn, None)
      functions.update(info.name, info)
    }

  private def typedDecl(decl: UntypedDecl, scope: Scope): Option[TypedDecl] =
    decl match
      case importDecl: UntypedImport =>
        Some(TypedImport(importDecl.path, importDecl.dest, importDecl.span))
      case importDecl: UntypedCppNamespaceImport =>
        Some(TypedCppNamespaceImport(importDecl.value, importDecl.span))
      case alias: UntypedTypeAlias =>
        Some(
          TypedTypeAlias(
            alias.name,
            resolveAlias(alias.name),
            alias.span,
            alias.tyParams,
          ),
        )
      case value: UntypedValueDecl =>
        val typed = typedValueDecl(value, scope)
        typed.foreach(valueDecl =>
          scope.define(
            valueSymbol(
              valueDecl.name,
              valueDecl.ty,
              valueDecl.kind,
              valueDecl.span,
            ),
          ),
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
          method.sig.functionType,
          mutBinding = false,
          mutAllowed = false,
          method.span,
        ),
      ),
    )

    val fields = info.fields.map { field =>
      val init = field.init.map(
        expr(_, classScope, Some(field.ty), FunctionContext.None),
      )
      init.foreach(
        checkAssignable(
          _,
          field.ty,
          field.span,
          "cosmo0.type.assignment-mismatch",
        ),
      )
      TypedValueDecl(
        field.kind,
        field.name,
        field.ty,
        init.map(_.expr),
        field.span,
      )
    }
    val methods = info.methods.values.toList.map(method =>
      typedFunction(method, globalScope),
    )
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

  private def typedFunction(
      info: FunctionInfo,
      outerScope: Scope,
  ): TypedFunction =
    val fnScope = outerScope.child
    info.params.foreach(param =>
      fnScope.define(
        ValueSymbol(
          param.name,
          param.ty,
          mutBinding = false,
          mutAllowed = mutationCapability(param.ty),
          param.span,
        ),
      ),
    )
    val context = FunctionContext.Some(info.retTy, info.owner)
    val defaults = info.params.map { param =>
      param.default
        .map(expr(_, fnScope, Some(param.ty), context))
        .map { typed =>
          checkAssignable(
            typed,
            param.ty,
            param.span,
            "cosmo0.type.assignment-mismatch",
          )
          typed.expr
        }
    }
    val typedParams = info.params.zip(defaults).map { case (param, default) =>
      TypedParam(param.name, param.ty, default, param.span)
    }
    val body = info.body.map(expr(_, fnScope, Some(info.retTy), context))
    body.foreach(
      checkAssignable(
        _,
        info.retTy,
        info.span,
        "cosmo0.type.return-mismatch",
      ),
    )
    TypedFunction(
      info.name,
      typedParams,
      info.retTy,
      body.map(_.expr),
      info.sig,
      info.owner,
      info.span,
      info.extern,
    )

  private def typedValueDecl(
      value: UntypedValueDecl,
      scope: Scope,
  ): Option[TypedValueDecl] =
    val explicitType = value.ty.map(resolveType(_, None))
    val init =
      value.init.map(expr(_, scope, explicitType, FunctionContext.None))
    val ty =
      explicitType.orElse(init.map(_.expr.ty)).getOrElse {
        error(
          "cosmo0.type.missing-annotation",
          s"value ${value.name} requires a type annotation or initializer",
          value.span,
        )
        SourceType.Error
      }
    init.foreach(
      checkAssignable(
        _,
        ty,
        value.span,
        "cosmo0.type.assignment-mismatch",
      ),
    )
    Some(
      TypedValueDecl(
        value.kind,
        value.name,
        ty,
        init.map(_.expr),
        value.span,
      ),
    )

  private def functionInfo(
      fn: UntypedFunction,
      owner: Option[String],
  ): FunctionInfo =
    val params = fn.params.map(param =>
      val ty = param.ty.map(resolveType(_, owner)).getOrElse {
        error(
          "cosmo0.type.missing-annotation",
          s"parameter ${param.name} requires an explicit type",
          param.span,
        )
        SourceType.Error
      }
      ParamInfo(param.name, ty, param.default, param.span),
    )
    val retTy =
      fn.retTy.map(resolveType(_, owner)).getOrElse(SourceType.Unit)
    val recv = owner.flatMap: ownerName =>
      params.headOption
        .filter(_.name == "self")
        .flatMap: self =>
          SourceType.dealias(self.ty) match
            case SourceType.Ref(SourceType.User(name), mutable)
                if name == ownerName =>
              Some(CallableReceiver(SourceType.User(ownerName), mutable))
            case SourceType.User(name) if name == ownerName =>
              Some(
                CallableReceiver(SourceType.User(ownerName), mutable = true),
              )
            case _ =>
              error(
                "cosmo0.type.invalid-self",
                s"method ${fn.name} has a self parameter that does not refer to $ownerName",
                self.span,
              )
              None
    val callableParams =
      if recv.nonEmpty && params.headOption.exists(_.name == "self") then
        params.tail
      else params
    val sig = CallableSignature(
      fn.name,
      callableParams.map(param =>
        CallableParam(param.name, param.ty, param.span),
      ),
      retTy,
      recv,
    )
    FunctionInfo(
      fn.name,
      params,
      retTy,
      fn.body,
      sig,
      owner,
      fn.span,
      fn.extern,
    )

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
        val body =
          expr(value.body, scope.child, Some(SourceType.Unit), context)
        ExprInfo(
          TypedLoop(body.expr, SourceType.Unit, value.span),
          mutBinding = false,
          mutAllowed = true,
        )
      case value: UntypedWhile =>
        val cond = expr(value.cond, scope, Some(SourceType.Bool), context)
        requireBool(cond, value.cond.span)
        val body =
          expr(value.body, scope.child, Some(SourceType.Unit), context)
        ExprInfo(
          TypedWhile(cond.expr, body.expr, SourceType.Unit, value.span),
          false,
          true,
        )
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
        ExprInfo(
          TypedBoolLiteral(value.value, SourceType.Bool, value.span),
          false,
          false,
        )
      case value: UntypedIntLiteral =>
        val ty =
          expected.filter(SourceType.isInteger).getOrElse(SourceType.I32)
        ExprInfo(
          TypedIntLiteral(value.value, ty, value.span),
          false,
          false,
        )
      case value: UntypedAsciiLiteral =>
        ExprInfo(
          TypedIntLiteral(
            asciiLiteralValue(value.value, value.span),
            SourceType.Byte,
            value.span,
          ),
          false,
          false,
        )
      case value: UntypedRuneLiteral =>
        ExprInfo(
          TypedIntLiteral(
            runeLiteralValue(value.value, value.span),
            SourceType.Rune,
            value.span,
          ),
          false,
          false,
        )
      case value: UntypedFloatLiteral =>
        val ty =
          expected.filter(SourceType.isFloat).getOrElse(SourceType.F64)
        ExprInfo(
          TypedFloatLiteral(value.value, ty, value.span),
          false,
          false,
        )
      case value: UntypedStringLiteral =>
        ExprInfo(
          TypedStringLiteral(value.value, SourceType.String, value.span),
          false,
          false,
        )
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
    val ty = items.lastOption match
      case Some(expr: TypedExpr) => expr.ty
      case _                     => SourceType.Unit
    ExprInfo(
      TypedBlock(items, ty, node.span),
      mutBinding = false,
      mutAllowed = true,
    )

  private def blockItem(
      item: UntypedBlockItem,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): TypedBlockItem =
    item match
      case local: UntypedLocal =>
        val explicitType =
          local.ty.map(resolveType(_, context.ownerName))
        val init = local.init.map(expr(_, scope, explicitType, context))
        val ty =
          explicitType.orElse(init.map(_.expr.ty)).getOrElse {
            error(
              "cosmo0.type.missing-annotation",
              s"local ${local.name} requires a type annotation or initializer",
              local.span,
            )
            SourceType.Error
          }
        init.foreach(
          checkAssignable(
            _,
            ty,
            local.span,
            "cosmo0.type.assignment-mismatch",
          ),
        )
        scope.define(
          valueSymbol(local.name, ty, local.kind, local.span),
        )
        TypedLocal(
          local.kind,
          local.name,
          ty,
          init.map(_.expr),
          local.span,
        )
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
              TypedName(
                node.path,
                symbol.ty,
                symbol.mutBinding,
                symbol.mutAllowed,
                node.span,
              ),
              symbol.mutBinding,
              symbol.mutAllowed,
            )
          case None =>
            classCtor(name, node.span).orElse(
              descriptorCtor(name, node.span),
            ) match
              case Some(sig) =>
                val callee =
                  SourceType.dealias(sig.returnType) match
                    case owner: SourceType.User =>
                      TypedTypeConstructorExpr(
                        owner,
                        sig.functionType,
                        node.span,
                      )
                    case _ =>
                      descriptorOwner(name) match
                        case Some(owner)
                            if SourceType.same(sig.returnType, owner) =>
                          TypedTypeConstructorExpr(
                            owner,
                            sig.functionType,
                            node.span,
                          )
                        case _ =>
                          TypedName(
                            node.path,
                            sig.functionType,
                            false,
                            false,
                            node.span,
                          )
                ExprInfo(callee, false, false)
              case None =>
                error(
                  "cosmo0.type.unresolved-name",
                  s"unresolved name ${node.path.text}",
                  node.span,
                )
                ExprInfo(
                  TypedName(
                    node.path,
                    SourceType.Error,
                    false,
                    false,
                    node.span,
                  ),
                  false,
                  false,
                )
      case _ =>
        error(
          "cosmo0.type.unresolved-name",
          s"unresolved name ${node.path.text}",
          node.span,
        )
        ExprInfo(
          TypedName(node.path, SourceType.Error, false, false, node.span),
          false,
          false,
        )

  private def typeConstructorExpr(node: UntypedTypeConstructor): ExprInfo =
    val constructedTy = resolveType(node.ty, None)
    val ty = SourceType.dealias(constructedTy) match
      case owner @ SourceType.Standard(name, _) =>
        standardGenerics
          .get(name)
          .flatMap(_.constructor("<init>"))
          .map(_.instantiate(owner, node.span).functionType)
          .getOrElse(SourceType.Error)
      case _ => SourceType.Error
    ExprInfo(
      TypedTypeConstructorExpr(constructedTy, ty, node.span),
      false,
      false,
    )

  private def selectExpr(
      node: UntypedSelect,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    node.recv match
      case UntypedName(path, _) if path.parts.length == 1 =>
        val ownerName = path.parts.head
        classes.get(ownerName).flatMap(_.variants.get(node.field)) match
          case Some(variant) =>
            val sig = variant.sig(ownerName)
            val ty =
              if sig.params.isEmpty then sig.returnType
              else sig.functionType
            return ExprInfo(
              TypedVariantConstructorExpr(
                SourceType.User(ownerName),
                node.field,
                ty,
                node.span,
              ),
              mutBinding = false,
              mutAllowed = false,
            )
          case None =>
      case _ =>

    val recv = expr(node.recv, scope, None, context)
    SourceType.dealias(recv.expr.ty) match
      case owner if foreignMethod(owner, node.field, node.span).nonEmpty =>
        val sig = foreignMethod(owner, node.field, node.span).get
        ExprInfo(
          TypedSelect(
            recv.expr,
            node.field,
            sig.functionType,
            mutBinding = false,
            mutAllowed = false,
            node.span,
          ),
          mutBinding = false,
          mutAllowed = false,
        )
      case owner if descriptorMethod(owner, node.field, node.span).nonEmpty =>
        val method = descriptorMethod(owner, node.field, node.span).get
        val sig =
          method.instantiate(normalizeDescriptorOwner(owner), node.span)
        ExprInfo(
          TypedSelect(
            recv.expr,
            node.field,
            sig.functionType,
            mutBinding = false,
            mutAllowed = false,
            node.span,
          ),
          mutBinding = false,
          mutAllowed = false,
        )
      case ty =>
        classInfoFor(ty) match
          case Some(cls) =>
            cls.fields.find(_.name == node.field) match
              case Some(field) =>
                val fieldMutAllowed =
                  recv.mutAllowed && mutationCapability(
                    field.ty,
                  )
                val mutBinding =
                  field.kind == UntypedValueKind.Var && recv.mutAllowed
                ExprInfo(
                  TypedSelect(
                    recv.expr,
                    node.field,
                    field.ty,
                    mutBinding,
                    fieldMutAllowed,
                    node.span,
                  ),
                  mutBinding,
                  fieldMutAllowed,
                )
              case None =>
                cls.methods.get(node.field) match
                  case Some(method) =>
                    ExprInfo(
                      TypedSelect(
                        recv.expr,
                        node.field,
                        method.sig.functionType,
                        mutBinding = false,
                        mutAllowed = false,
                        node.span,
                      ),
                      false,
                      false,
                    )
                  case None =>
                    invalidField(
                      node.field,
                      recv.expr.ty,
                      node.span,
                    )
          case None =>
            invalidField(node.field, recv.expr.ty, node.span)

  private def invalidField(
      field: String,
      receiverType: SourceType,
      span: SourceSpan,
  ): ExprInfo =
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
        mutBinding = false,
        mutAllowed = false,
        span,
      ),
      mutBinding = false,
      mutAllowed = false,
    )

  private def variantConstructorExpr(
      node: UntypedVariantConstructor,
  ): ExprInfo =
    val foreignName =
      foreignQualifiedName(node.owner, node.variant, node.span)
    if foreignName.nonEmpty then
      return ExprInfo(
        foreignName.get,
        mutBinding = false,
        mutAllowed = false,
      )

    val ownerType = resolveType(node.owner, None)
    ctorSig(ownerType, node.variant, node.span) match
      case Some(sig) =>
        val ty =
          if sig.params.isEmpty then sig.returnType
          else sig.functionType
        ExprInfo(
          TypedVariantConstructorExpr(
            ownerType,
            node.variant,
            ty,
            node.span,
          ),
          mutBinding = false,
          mutAllowed = false,
        )
      case None =>
        error(
          "cosmo0.type.invalid-field",
          s"type ${ownerType.display} has no variant constructor ${node.variant}",
          node.span,
        )
        ExprInfo(
          TypedVariantConstructorExpr(
            ownerType,
            node.variant,
            SourceType.Error,
            node.span,
          ),
          false,
          false,
        )

  private def foreignQualifiedName(
      owner: UntypedType,
      finalSegment: String,
      span: SourceSpan,
  ): Option[TypedForeignQualifiedName] =
    owner match
      case UntypedNamedType(path, _) =>
        path.parts match
          case root :: rest =>
            foreignAliases.get(root).map { importValue =>
              val suffix = rest :+ finalSegment
              TypedForeignQualifiedName(
                importValue,
                suffix,
                SourceType
                  .ForeignSymbol(foreignCanonicalName(importValue, suffix)),
                span,
              )
            }
          case Nil =>
            None
      case _ =>
        None

  private def foreignCanonicalName(
      importValue: SourceCppNamespaceImport,
      suffix: List[String],
  ): String =
    val suffixName = suffix.mkString("::")
    if suffixName.isEmpty then importValue.namespace.cppName
    else s"${importValue.namespace.cppName}::$suffixName"

  private def callExpr(
      node: UntypedCall,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    node.callee match
      case select: UntypedSelect =>
        methodOrVariantCall(
          select,
          node.args,
          node.span,
          scope,
          expected,
          context,
        )
      case ctor: UntypedVariantConstructor =>
        val ownerType = resolveType(ctor.owner, context.ownerName)
        ctorSig(
          ownerType,
          ctor.variant,
          ctor.span,
        ) match
          case Some(sig) =>
            callWithSig(
              TypedVariantConstructorExpr(
                ownerType,
                ctor.variant,
                sig.functionType,
                ctor.span,
              ),
              sig,
              node.args,
              node.span,
              scope,
              context,
            )
          case None =>
            error(
              "cosmo0.type.invalid-call",
              s"type ${ownerType.display} has no variant constructor ${ctor.variant}",
              ctor.span,
            )
            errorCall(node, scope, context)
      case ctor: UntypedTypeConstructor =>
        val constructedTy =
          resolveType(ctor.ty, context.ownerName)
        SourceType.dealias(constructedTy) match
          case owner @ SourceType.Standard(name, _) =>
            standardGenerics.get(name).flatMap(_.constructor("<init>")) match
              case Some(descriptor) =>
                val sig =
                  descriptor.instantiate(owner, ctor.span)
                callWithSig(
                  TypedTypeConstructorExpr(
                    constructedTy,
                    sig.functionType,
                    ctor.span,
                  ),
                  sig,
                  node.args,
                  node.span,
                  scope,
                  context,
                )
              case None =>
                error(
                  "cosmo0.type.invalid-call",
                  s"type ${constructedTy.display} is not directly constructible",
                  ctor.span,
                )
                errorCall(node, scope, context)
          case owner @ SourceType.ForeignApplied(_, _) =>
            val sig = CallableSignature("<init>", Nil, owner)
            callWithSig(
              TypedTypeConstructorExpr(
                constructedTy,
                sig.functionType,
                ctor.span,
              ),
              sig,
              node.args,
              node.span,
              scope,
              context,
            )
          case _ =>
            error(
              "cosmo0.type.invalid-call",
              s"type ${constructedTy.display} is not directly constructible",
              ctor.span,
            )
            errorCall(node, scope, context)
      case name: UntypedName if name.path.parts.length == 1 =>
        val calleeName = name.path.parts.head
        functions.get(calleeName) match
          case Some(info) =>
            callWithSig(
              TypedName(
                name.path,
                info.sig.functionType,
                false,
                false,
                name.span,
              ),
              info.sig,
              node.args,
              node.span,
              scope,
              context,
            )
          case None =>
            if isRuntimeFunction(calleeName) then
              runtimeFunctionCall(
                calleeName,
                name,
                node.args,
                node.span,
                scope,
                context,
              )
            else
              classCtor(calleeName, name.span).orElse(
                descriptorCtor(calleeName, name.span),
              ) match
                case Some(sig) =>
                  val callee =
                    SourceType.dealias(sig.returnType) match
                      case owner: SourceType.User =>
                        TypedTypeConstructorExpr(
                          owner,
                          sig.functionType,
                          name.span,
                        )
                      case _ =>
                        descriptorOwner(calleeName) match
                          case Some(owner)
                              if SourceType.same(
                                sig.returnType,
                                owner,
                              ) =>
                            TypedTypeConstructorExpr(
                              owner,
                              sig.functionType,
                              name.span,
                            )
                          case _ =>
                            TypedName(
                              name.path,
                              sig.functionType,
                              false,
                              false,
                              name.span,
                            )
                  callWithSig(
                    callee,
                    sig,
                    node.args,
                    node.span,
                    scope,
                    context,
                  )
                case None =>
                  val callee = expr(node.callee, scope, None, context)
                  callFunctionValue(
                    callee,
                    node.args,
                    node.span,
                    scope,
                    context,
                  )
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
    select.recv match
      case UntypedName(path, _) if path.parts.length == 1 =>
        val ownerName = path.parts.head
        classes.get(ownerName).flatMap(_.variants.get(select.field)) match
          case Some(variant) =>
            val sig = variant.sig(ownerName)
            return callWithSig(
              TypedVariantConstructorExpr(
                SourceType.User(ownerName),
                select.field,
                sig.functionType,
                select.span,
              ),
              sig,
              args,
              span,
              scope,
              context,
            )
          case None =>
      case _ =>

    val recv = expr(select.recv, scope, None, context)
    SourceType.dealias(recv.expr.ty) match
      case owner if foreignMethod(owner, select.field, select.span).nonEmpty =>
        val sig = foreignMethod(owner, select.field, select.span).get
        checkRecvMutation(recv, sig, select.span)
        callWithSig(
          TypedSelect(
            recv.expr,
            select.field,
            sig.functionType,
            false,
            false,
            select.span,
          ),
          sig,
          args,
          span,
          scope,
          context,
        )
      case owner
          if descriptorMethod(owner, select.field, select.span).nonEmpty =>
        val method = descriptorMethod(owner, select.field, select.span).get
        val sig =
          method.instantiate(normalizeDescriptorOwner(owner), select.span)
        checkRecvMutation(recv, sig, select.span)
        callWithSig(
          TypedSelect(
            recv.expr,
            select.field,
            sig.functionType,
            false,
            false,
            select.span,
          ),
          sig,
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
                checkRecvMutation(recv, method.sig, select.span)
                callWithSig(
                  TypedSelect(
                    recv.expr,
                    select.field,
                    method.sig.functionType,
                    false,
                    false,
                    select.span,
                  ),
                  method.sig,
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
    SourceType.dealias(callee.expr.ty) match
      case SourceType.Function(params, retTy) =>
        val sig = CallableSignature(
          "<function>",
          params.zipWithIndex.map { case (paramType, index) =>
            CallableParam(s"arg$index", paramType, span)
          },
          retTy,
        )
        callWithSig(callee.expr, sig, args, span, scope, context)
      case other =>
        error(
          "cosmo0.type.invalid-call",
          s"type ${other.display} is not callable",
          span,
        )
        val typedArgs = args.map(expr(_, scope, None, context).expr)
        ExprInfo(
          TypedCall(
            callee.expr,
            typedArgs,
            SourceType.Error,
            CallableSignature("<error>", Nil, SourceType.Error),
            span,
          ),
          false,
          false,
        )

  private def callWithSig(
      callee: TypedExpr,
      sig: CallableSignature,
      args: List[UntypedExpr],
      span: SourceSpan,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    if args.length != sig.params.length then
      error(
        "cosmo0.type.wrong-arity",
        s"${sig.name} expects ${sig.params.length} argument(s), got ${args.length}",
        span,
      )
    val typedArgs = args.zipWithIndex.map { case (arg, index) =>
      val expectedTy = sig.params.lift(index).map(_.valueType)
      val typed = expr(arg, scope, expectedTy, context)
      expectedTy.foreach(paramType =>
        if !canPass(typed, paramType) then
          error(
            "cosmo0.type.invalid-call",
            s"argument ${index + 1} has type ${typed.expr.ty.display}, expected ${paramType.display}",
            arg.span,
          ),
      )
      typed.expr
    }
    ExprInfo(
      TypedCall(callee, typedArgs, sig.returnType, sig, span),
      mutBinding = false,
      mutAllowed = mutationCapability(sig.returnType),
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
    val sig = TrustedExternAbi
      .callable(calleeName, name.span)
      .getOrElse(
        CallableSignature(calleeName, Nil, SourceType.Error),
      )
    if args.length != sig.params.length then
      error(
        "cosmo0.type.wrong-arity",
        s"$calleeName expects ${sig.params.length} argument(s), got ${args.length}",
        span,
      )
    val typedArgs = args.zipWithIndex.map { case (arg, index) =>
      expr(
        arg,
        scope,
        sig.params.lift(index).map(_.valueType),
        context,
      ).expr
    }
    typedArgs.zip(sig.params).zipWithIndex.foreach {
      case ((actual, param), index) =>
        val paramType = param.valueType
        if !SourceType.assignable(actual.ty, paramType) then
          error(
            "cosmo0.type.invalid-call",
            s"argument ${index + 1} has type ${actual.ty.display}, expected ${paramType.display}",
            args(index).span,
          )
    }
    ExprInfo(
      TypedCall(
        TypedName(name.path, sig.functionType, false, false, name.span),
        typedArgs,
        sig.returnType,
        sig,
        span,
      ),
      mutBinding = false,
      mutAllowed = mutationCapability(sig.returnType),
    )

  private def isRuntimeFunction(name: String): Boolean =
    TrustedExternAbi.isTrustedSourceName(name)

  private def assignExpr(
      node: UntypedAssign,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    val target = expr(node.target, scope, None, context)
    if !target.mutBinding then
      error(
        "cosmo0.type.invalid-mutability",
        "assignment target is not mutable",
        node.target.span,
      )
    val valueExpected =
      if node.op == "=" then Some(target.expr.ty)
      else Some(target.expr.ty)
    val value = expr(node.value, scope, valueExpected, context)
    if !SourceType.assignable(value.expr.ty, target.expr.ty)
    then
      error(
        "cosmo0.type.assignment-mismatch",
        s"cannot assign ${value.expr.ty.display} to ${target.expr.ty.display}",
        node.span,
      )
    if node.op != "=" && !SourceType.isNumeric(target.expr.ty) then
      error(
        "cosmo0.type.assignment-mismatch",
        s"operator ${node.op} requires a numeric target",
        node.span,
      )
    ExprInfo(
      TypedAssign(
        target.expr,
        value.expr,
        node.op,
        SourceType.Unit,
        node.span,
      ),
      false,
      true,
    )

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
        ExprInfo(
          TypedUnary(node.op, value.expr, SourceType.Bool, node.span),
          false,
          false,
        )
      case "-" =>
        val value = expr(
          node.expr,
          scope,
          expected.filter(SourceType.isNumeric),
          context,
        )
        if !SourceType.isNumeric(value.expr.ty) then
          error(
            "cosmo0.type.invalid-unary",
            s"operator - requires a numeric operand, got ${value.expr.ty.display}",
            node.span,
          )
        ExprInfo(
          TypedUnary(node.op, value.expr, value.expr.ty, node.span),
          false,
          false,
        )
      case "&" =>
        val value = expr(node.expr, scope, None, context)
        ExprInfo(
          TypedUnary(
            node.op,
            value.expr,
            SourceType.Ref(value.expr.ty, mutable = false),
            node.span,
          ),
          false,
          false,
        )
      case "*" =>
        val value = expr(node.expr, scope, None, context)
        SourceType.dealias(value.expr.ty) match
          case SourceType.Ref(target, mutable) =>
            ExprInfo(
              TypedUnary(node.op, value.expr, target, node.span),
              mutable,
              mutable,
            )
          case other =>
            error(
              "cosmo0.type.invalid-unary",
              s"operator * requires a reference, got ${other.display}",
              node.span,
            )
            ExprInfo(
              TypedUnary(node.op, value.expr, SourceType.Error, node.span),
              false,
              false,
            )
      case other =>
        val value = expr(node.expr, scope, None, context)
        error(
          "cosmo0.type.invalid-unary",
          s"unsupported unary operator $other",
          node.span,
        )
        ExprInfo(
          TypedUnary(other, value.expr, SourceType.Error, node.span),
          false,
          false,
        )

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
        ExprInfo(
          TypedBinary(
            node.op,
            left.expr,
            right.expr,
            SourceType.Bool,
            node.span,
          ),
          false,
          false,
        )
      case "==" | "!=" =>
        val left = expr(node.left, scope, None, context)
        val right =
          expr(node.right, scope, Some(left.expr.ty), context)
        if !SourceType.same(left.expr.ty, right.expr.ty) then
          error(
            "cosmo0.type.invalid-binary",
            s"operator ${node.op} requires comparable operands, got ${left.expr.ty.display} and ${right.expr.ty.display}",
            node.span,
          )
        ExprInfo(
          TypedBinary(
            node.op,
            left.expr,
            right.expr,
            SourceType.Bool,
            node.span,
          ),
          false,
          false,
        )
      case "<" | "<=" | ">" | ">=" =>
        val left = expr(
          node.left,
          scope,
          expected.filter(SourceType.isNumeric),
          context,
        )
        val right =
          expr(node.right, scope, Some(left.expr.ty), context)
        if !SourceType.isNumeric(left.expr.ty) || !SourceType.same(
            left.expr.ty,
            right.expr.ty,
          )
        then
          error(
            "cosmo0.type.invalid-binary",
            s"operator ${node.op} requires matching numeric operands",
            node.span,
          )
        ExprInfo(
          TypedBinary(
            node.op,
            left.expr,
            right.expr,
            SourceType.Bool,
            node.span,
          ),
          false,
          false,
        )
      case "+" | "-" | "*" | "/" | "%" =>
        val left = expr(
          node.left,
          scope,
          expected.filter(SourceType.isNumeric),
          context,
        )
        val right =
          expr(node.right, scope, Some(left.expr.ty), context)
        if !SourceType.isNumeric(left.expr.ty) || !SourceType.same(
            left.expr.ty,
            right.expr.ty,
          )
        then
          error(
            "cosmo0.type.invalid-binary",
            s"operator ${node.op} requires matching numeric operands",
            node.span,
          )
        ExprInfo(
          TypedBinary(
            node.op,
            left.expr,
            right.expr,
            left.expr.ty,
            node.span,
          ),
          false,
          false,
        )
      case other =>
        val left = expr(node.left, scope, None, context)
        val right =
          expr(node.right, scope, Some(left.expr.ty), context)
        error(
          "cosmo0.type.invalid-binary",
          s"unsupported binary operator $other",
          node.span,
        )
        ExprInfo(
          TypedBinary(
            other,
            left.expr,
            right.expr,
            SourceType.Error,
            node.span,
          ),
          false,
          false,
        )

  private def ifExpr(
      node: UntypedIf,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    val cond = expr(node.cond, scope, Some(SourceType.Bool), context)
    requireBool(cond, node.cond.span)
    val thenExp = expr(node.thenExp, scope.child, expected, context)
    val elseExp =
      node.elseExp.map(expr(_, scope.child, expected, context))
    val ty = elseExp match
      case Some(other)
          if SourceType.same(
            thenExp.expr.ty,
            other.expr.ty,
          ) =>
        thenExp.expr.ty
      case Some(other) =>
        error(
          "cosmo0.type.branch-mismatch",
          s"if branches have types ${thenExp.expr.ty.display} and ${other.expr.ty.display}",
          node.span,
        )
        SourceType.Error
      case None => SourceType.Unit
    ExprInfo(
      TypedIf(
        cond.expr,
        thenExp.expr,
        elseExp.map(_.expr),
        ty,
        node.span,
      ),
      false,
      mutationCapability(ty),
    )

  private def forExpr(
      node: UntypedFor,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    val iter = expr(node.iter, scope, None, context)
    val itemTy = SourceType.dealias(iter.expr.ty) match
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
    bodyScope.define(
      ValueSymbol(
        node.name,
        itemTy,
        false,
        mutationCapability(itemTy),
        node.span,
      ),
    )
    val body = expr(node.body, bodyScope, Some(SourceType.Unit), context)
    ExprInfo(
      TypedFor(
        node.name,
        itemTy,
        iter.expr,
        body.expr,
        SourceType.Unit,
        node.span,
      ),
      false,
      true,
    )

  private def matchExpr(
      node: UntypedMatch,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    // Ordinary cosmo0 match checking is SourceType-based: it checks variant
    // payloads and branch result agreement, but it does not elaborate indexed
    // families into `DependentPatterns` case trees.
    val scrut = expr(node.scrut, scope, None, context)
    val arms = node.arms.map { arm =>
      val armScope = scope.child
      val typedPattern =
        pattern(arm.pat, scrut.expr.ty, armScope)
      val body = arm.body.map(expr(_, armScope, expected, context))
      TypedMatchArm(typedPattern, body.map(_.expr), arm.span)
    }
    val bodyTypes = arms.flatMap(_.body.map(_.ty))
    val ty =
      if bodyTypes.isEmpty then SourceType.Unit
      else
        val first = bodyTypes.head
        if bodyTypes.tail.forall(ty => SourceType.same(first, ty))
        then first
        else
          error(
            "cosmo0.type.branch-mismatch",
            "match arms must produce the same type",
            node.span,
          )
          SourceType.Error
    ExprInfo(
      TypedMatch(scrut.expr, arms, ty, node.span),
      false,
      mutationCapability(ty),
    )

  private def returnExpr(
      node: UntypedReturn,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    context match
      case FunctionContext.Some(retTy, _) =>
        val value = expr(node.value, scope, Some(retTy), context)
        if !SourceType.assignable(value.expr.ty, retTy) then
          error(
            "cosmo0.type.return-mismatch",
            s"return has type ${value.expr.ty.display}, expected ${retTy.display}",
            node.span,
          )
        ExprInfo(
          TypedReturn(value.expr, SourceType.Never, node.span),
          false,
          false,
        )
      case FunctionContext.None =>
        val value = expr(node.value, scope, None, context)
        error(
          "cosmo0.type.invalid-return",
          "return can only appear inside a function",
          node.span,
        )
        ExprInfo(
          TypedReturn(value.expr, SourceType.Never, node.span),
          false,
          false,
        )

  private def pattern(
      node: UntypedPattern,
      expectedTy: SourceType,
      scope: Scope,
  ): TypedPattern =
    node match
      case value: UntypedWildcardPattern =>
        TypedWildcardPattern(expectedTy, value.span)
      case value: UntypedBindingPattern =>
        scope.define(
          ValueSymbol(
            value.name,
            expectedTy,
            mutBinding = false,
            mutAllowed = mutationCapability(expectedTy),
            value.span,
          ),
        )
        TypedBindingPattern(value.name, expectedTy, value.span)
      case value: UntypedBoolLiteral =>
        if !SourceType.same(expectedTy, SourceType.Bool) then
          invalidPatternType(value.span, expectedTy, SourceType.Bool)
        TypedBoolLiteral(value.value, SourceType.Bool, value.span)
      case value: UntypedIntLiteral =>
        val ty =
          if SourceType.isInteger(expectedTy) then expectedTy
          else SourceType.I32
        if !SourceType.isInteger(expectedTy) then
          invalidPatternType(value.span, expectedTy, ty)
        TypedIntLiteral(value.value, ty, value.span)
      case value: UntypedAsciiLiteral =>
        if !SourceType.same(expectedTy, SourceType.Byte) then
          invalidPatternType(value.span, expectedTy, SourceType.Byte)
        TypedIntLiteral(
          asciiLiteralValue(value.value, value.span),
          SourceType.Byte,
          value.span,
        )
      case value: UntypedRuneLiteral =>
        if !SourceType.same(expectedTy, SourceType.Rune) then
          invalidPatternType(value.span, expectedTy, SourceType.Rune)
        TypedIntLiteral(
          runeLiteralValue(value.value, value.span),
          SourceType.Rune,
          value.span,
        )
      case value: UntypedFloatLiteral =>
        val ty =
          if SourceType.isFloat(expectedTy) then expectedTy
          else SourceType.F64
        if !SourceType.isFloat(expectedTy) then
          invalidPatternType(value.span, expectedTy, ty)
        TypedFloatLiteral(value.value, ty, value.span)
      case value: UntypedStringLiteral =>
        if !SourceType.same(expectedTy, SourceType.String) then
          invalidPatternType(value.span, expectedTy, SourceType.String)
        TypedStringLiteral(value.value, SourceType.String, value.span)
      case value: UntypedVariantPattern =>
        variantPattern(value, expectedTy, scope)

  private def variantPattern(
      node: UntypedVariantPattern,
      expectedTy: SourceType,
      scope: Scope,
  ): TypedPattern =
    val ctor =
      ctorExprForPattern(node.ctor, expectedTy)
    val sig = ctor.flatMap { case (ownerType, variantName, expr) =>
      ctorSig(ownerType, variantName, expr.span).map(sig => (sig, expr))
    }
    sig match
      case Some((callable, constructorExpr)) =>
        if !SourceType.same(callable.returnType, expectedTy) then
          error(
            "cosmo0.type.invalid-match-payload",
            s"pattern constructor returns ${callable.returnType.display}, expected ${expectedTy.display}",
            node.span,
          )
        if node.args.length != callable.params.length then
          error(
            "cosmo0.type.wrong-arity",
            s"pattern expects ${callable.params.length} payload(s), got ${node.args.length}",
            node.span,
          )
        val args = node.args.zipWithIndex.map { case (arg, index) =>
          val argType = callable.params
            .lift(index)
            .map(_.valueType)
            .getOrElse(SourceType.Error)
          pattern(arg, argType, scope)
        }
        TypedVariantPattern(
          constructorExpr,
          args,
          callable.returnType,
          node.span,
        )
      case None =>
        error(
          "cosmo0.type.invalid-match-payload",
          "invalid variant pattern",
          node.span,
        )
        TypedVariantPattern(
          TypedVariantConstructorExpr(
            expectedTy,
            "<error>",
            SourceType.Error,
            node.span,
          ),
          Nil,
          SourceType.Error,
          node.span,
        )

  private def ctorExprForPattern(
      node: UntypedExpr,
      expectedTy: SourceType,
  ): Option[(SourceType, String, TypedExpr)] =
    node match
      case UntypedVariantConstructor(owner, variant, span) =>
        val ownerType = resolveType(owner, None)
        Some(
          (
            ownerType,
            variant,
            TypedVariantConstructorExpr(
              ownerType,
              variant,
              SourceType.Function(Nil, ownerType),
              span,
            ),
          ),
        )
      case UntypedSelect(UntypedName(path, _), variant, span)
          if path.parts.length == 1 =>
        val ownerType = SourceType.User(path.parts.head)
        Some(
          (
            ownerType,
            variant,
            TypedVariantConstructorExpr(
              ownerType,
              variant,
              SourceType.Function(Nil, ownerType),
              span,
            ),
          ),
        )
      case _ =>
        None

  private def ctorSig(
      ownerType: SourceType,
      variant: String,
      span: SourceSpan,
  ): Option[CallableSignature] =
    SourceType.dealias(ownerType) match
      case owner @ SourceType.Standard(name, _) =>
        standardGenerics
          .get(name)
          .flatMap(_.constructor(variant))
          .map(_.instantiate(owner, span))
      case SourceType.User(name) =>
        classes
          .get(name)
          .flatMap(_.variants.get(variant))
          .map(_.sig(name))
      case _ => None

  private def descriptorCtor(
      name: String,
      span: SourceSpan,
  ): Option[CallableSignature] =
    descriptorOwner(name).flatMap(owner =>
      standardGenerics
        .get(name)
        .flatMap(_.constructor("<init>"))
        .map(_.instantiate(owner, span)),
    )

  private def descriptorMethod(
      ownerType: SourceType,
      methodName: String,
      span: SourceSpan,
  ): Option[DescriptorCallable] =
    val owner = normalizeDescriptorOwner(ownerType)
    descriptorName(owner).flatMap(name =>
      standardGenerics
        .get(name)
        .filter(_.arity == descriptorArity(owner))
        .flatMap(_.method(methodName)),
    )

  private def foreignMethod(
      ownerType: SourceType,
      methodName: String,
      span: SourceSpan,
  ): Option[CallableSignature] =
    normalizeDescriptorOwner(ownerType) match
      case owner @ SourceType.ForeignApplied("::std::vector", item :: Nil) =>
        foreignVectorMethod(owner, item, methodName, span)
      case _ =>
        None

  private def foreignVectorMethod(
      owner: SourceType,
      item: SourceType,
      methodName: String,
      span: SourceSpan,
  ): Option[CallableSignature] =
    methodName match
      case "push_back" =>
        Some(
          CallableSignature(
            methodName,
            List(CallableParam("value", item, span)),
            SourceType.Unit,
            Some(CallableReceiver(owner, mutable = true)),
          ),
        )
      case "size" | "len" =>
        Some(
          CallableSignature(
            methodName,
            Nil,
            SourceType.Usize,
            Some(CallableReceiver(owner, mutable = false)),
          ),
        )
      case _ =>
        None

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

  private def classCtor(
      name: String,
      span: SourceSpan,
  ): Option[CallableSignature] =
    classes.get(name).map(_.ctorSig)

  private def classInfoFor(ty: SourceType): Option[ClassInfo] =
    SourceType.dealias(ty) match
      case SourceType.User(name)                    => classes.get(name)
      case SourceType.Ref(SourceType.User(name), _) => classes.get(name)
      case _                                        => None

  private def resolveAlias(name: String): SourceType =
    resolveAlias(name, Set.empty)

  private def resolveAlias(name: String, seen: Set[String]): SourceType =
    val resolved = rawAliases.get(name) match
      case Some(alias) if seen.contains(name) =>
        error(
          "cosmo0.type.recursive-alias",
          s"type alias $name is recursive",
          alias.span,
        )
        SourceType.Error
      case Some(alias) =>
        resolveType(alias.target, None, seen + name, alias.tyParams.toSet)
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
      tyParams: Set[String] = Set.empty,
  ): SourceType =
    node match
      case UntypedNamedType(path, span) =>
        path.parts match
          case name :: Nil if tyParams.contains(name) =>
            SourceType.TypeParam(name)
          case name :: Nil
              if owner.contains(
                name,
              ) || ((name == "self" || name == "Self") && owner.nonEmpty) =>
            SourceType.User(owner.get)
          case name :: Nil if foreignAliases.contains(name) =>
            SourceType.ForeignNamespace(foreignAliases(name))
          case name :: Nil =>
            SourceType
              .scalar(name)
              .orElse(
                rawAliases
                  .get(name)
                  .map(_ =>
                    SourceType.Alias(name, resolveAlias(name, seenAliases)),
                  ),
              )
              .orElse(
                if classNames.contains(name) then Some(SourceType.User(name))
                else None,
              )
              .getOrElse {
                error(
                  "cosmo0.type.unknown-type",
                  s"unknown type ${path.text}",
                  span,
                )
                SourceType.Error
              }
          case root :: suffix if foreignAliases.contains(root) =>
            SourceType.ForeignSymbol(
              foreignCanonicalName(foreignAliases(root), suffix),
            )
          case _ =>
            error(
              "cosmo0.type.unknown-type",
              s"unknown type ${path.text}",
              span,
            )
            SourceType.Error
      case UntypedAppliedType(base, args, span) =>
        val name = base.parts.lastOption.getOrElse(base.text)
        base.parts match
          case root :: suffix
              if suffix.nonEmpty && foreignAliases.contains(root) =>
            val resolvedArgs =
              args.map(resolveType(_, owner, seenAliases, tyParams))
            SourceType.ForeignApplied(
              foreignCanonicalName(foreignAliases(root), suffix),
              resolvedArgs,
            )
          case aliasName :: Nil
              if rawAliases.get(aliasName).exists(_.tyParams.nonEmpty) =>
            val alias = rawAliases(aliasName)
            if alias.tyParams.length != args.length then
              error(
                "cosmo0.type.wrong-arity",
                s"$aliasName expects ${alias.tyParams.length} type argument(s), got ${args.length}",
                span,
              )
            val resolvedArgs =
              args.map(resolveType(_, owner, seenAliases, tyParams))
            substituteTypeParams(
              resolveAlias(aliasName, seenAliases),
              alias.tyParams.zip(resolvedArgs).toMap,
            )
          case _ =>
            standardGenerics.get(name) match
              case Some(descriptor) =>
                if descriptor.arity != args.length then
                  error(
                    "cosmo0.type.wrong-arity",
                    s"$name expects ${descriptor.arity} type argument(s), got ${args.length}",
                    span,
                  )
                val resolvedArgs =
                  args.map(resolveType(_, owner, seenAliases, tyParams))
                validateStandardTypeApplication(name, resolvedArgs, span)
                SourceType.Standard(name, resolvedArgs)
              case None =>
                error(
                  "cosmo0.type.unknown-type",
                  s"unknown standard generic type ${base.text}",
                  span,
                )
                SourceType.Error
      case UntypedRefType(target, mut, _) =>
        SourceType.Ref(
          resolveType(target, owner, seenAliases, tyParams),
          mut,
        )

  private def substituteTypeParams(
      ty: SourceType,
      values: Map[String, SourceType],
  ): SourceType =
    ty match
      case SourceType.TypeParam(name) =>
        values.getOrElse(name, ty)
      case SourceType.Alias(name, target) =>
        SourceType.Alias(name, substituteTypeParams(target, values))
      case SourceType.ForeignApplied(canonicalName, args) =>
        SourceType.ForeignApplied(
          canonicalName,
          args.map(substituteTypeParams(_, values)),
        )
      case SourceType.Ref(target, mutable) =>
        SourceType.Ref(substituteTypeParams(target, values), mutable)
      case SourceType.Standard(name, args) =>
        SourceType.Standard(name, args.map(substituteTypeParams(_, values)))
      case SourceType.Function(params, retTy) =>
        SourceType.Function(
          params.map(substituteTypeParams(_, values)),
          substituteTypeParams(retTy, values),
        )
      case other =>
        other

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

  private def isSupportedMapSetKey(ty: SourceType): Boolean =
    SourceType.dealias(ty) match
      case SourceType.String => true
      case SourceType.Builtin(
            "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
            "usize",
          ) =>
        true
      case SourceType.Standard("Id", _ :: Nil) => true
      case _                                   => false

  private def canPass(actual: ExprInfo, expected: SourceType): Boolean =
    SourceType.dealias(expected) match
      case SourceType.Ref(target, mutable) =>
        SourceType.dealias(actual.expr.ty) match
          case SourceType.Ref(actualTarget, actualMutable) =>
            SourceType.same(
              actualTarget,
              target,
            ) && (!mutable || actualMutable)
          case other =>
            SourceType.same(
              other,
              target,
            ) && (!mutable || actual.mutAllowed)
      case other =>
        SourceType.assignable(actual.expr.ty, other)

  private def checkAssignable(
      actual: ExprInfo,
      expected: SourceType,
      span: SourceSpan,
      code: String,
  ): Unit =
    if !SourceType.assignable(actual.expr.ty, expected) then
      error(
        code,
        s"expected ${expected.display}, got ${actual.expr.ty.display}",
        span,
      )

  private def checkRecvMutation(
      recv: ExprInfo,
      sig: CallableSignature,
      span: SourceSpan,
  ): Unit =
    sig.receiver.foreach { recvInfo =>
      if recvInfo.mutable && !recv.mutAllowed then
        error(
          "cosmo0.type.invalid-mutability",
          s"${sig.name} requires a mutable receiver",
          span,
        )
    }

  private def requireBool(value: ExprInfo, span: SourceSpan): Unit =
    if !SourceType.same(value.expr.ty, SourceType.Bool) then
      error(
        "cosmo0.type.expected-bool",
        s"expected Bool, got ${value.expr.ty.display}",
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
      ty: SourceType,
      kind: UntypedValueKind,
      span: SourceSpan,
  ): ValueSymbol =
    ValueSymbol(
      name,
      ty,
      mutBinding = kind == UntypedValueKind.Var,
      mutAllowed = mutationCapability(ty),
      span,
    )

  private def mutationCapability(ty: SourceType): Boolean =
    SourceType.dealias(ty) match
      case SourceType.Ref(_, mutable) => mutable
      case SourceType.ForeignNamespace(_) | SourceType.ForeignSymbol(_) =>
        false
      case SourceType.Never | SourceType.Error => false
      case _                                   => true

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

  private def singleCodePoint(
      value: String,
      name: String,
      span: SourceSpan,
  ): Option[Int] =
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
    case Some(retTy: SourceType, containingOwner: Option[String])
    case None

    def ownerName: Option[String] =
      this match
        case Some(_, containingOwner) => containingOwner
        case None                     => scala.None
