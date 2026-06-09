package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Factory for the MLTT-backed source checker.
  *
  * The no-profile entry point selects `mltt.dependent-patterns`: ordinary
  * cosmo0 source still needs source-level declaration and expression
  * elaboration, but its type relations are delegated to `MlttTypeChecker`.
  */
object MlttTyper:
  def apply(module: UntypedModule): MlttTyper =
    new MlttTyper(
      module,
      StandardGenericDescriptors.all,
      CheckerProfiles.MlttDependentPatterns,
    )

  def apply(module: UntypedModule, profile: CheckerProfile): MlttTyper =
    new MlttTyper(module, StandardGenericDescriptors.all, profile)

/** Checks an `UntypedModule` into a `TypedModule` through the MLTT-backed
  * source frontend.
  *
  * Program examples:
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
  * The `profile` constructor parameter controls MLTT feature validation. The
  * frontend is responsible for source declaration resolution and typed-tree
  * construction; type equality, assignability, and dependent-pattern hooks are
  * delegated to `MlttTypeChecker`.
  *
  * The checker is bidirectional in a small, source-oriented sense: `expr(node,
  * expected, context)` infers a type when `expected` is absent, and uses
  * `expected` to guide literals, calls, branch bodies, assignments, returns,
  * and block final expressions.
  *
  * Rules:
  *
  * {{{
  * Gamma |- e => T
  * Gamma |- e <= T
  *
  * Gamma |- init <= T
  *   --------------------------------------------------- ValueCheck
  *   Gamma |- val x : T = init checks
  *
  * Gamma |- init => T
  *   --------------------------------------------------- ValueInfer
  *   Gamma |- val x = init defines x : T
  *
  * Gamma, params : ParamTypes |- body <= Return
  *   --------------------------------------------------- FunctionCheck
  *   Gamma |- def f(params): Return = body checks
  *
  * Gamma(x) = T
  *   --------------------------------------------------- Name
  *   Gamma |- x => T
  *
  * Gamma |- callee => (P1, ..., Pn) -> R
  * Gamma |- arg1 <= P1 ... Gamma |- argn <= Pn
  *   --------------------------------------------------- Call
  *   Gamma |- callee(arg1, ..., argn) => R
  *
  * Gamma |- recv => Owner; method(Owner, name) = (P*) -> R
  * Gamma |- args <= P*
  *   --------------------------------------------------- MethodCall
  *   Gamma |- recv.name(args) => R
  *
  * Gamma |- target => T mutable; Gamma |- value <= T
  *   --------------------------------------------------- Assign
  *   Gamma |- target = value => Unit
  *
  * Gamma |- cond <= Bool; Gamma |- then <= T; Gamma |- else <= T
  *   --------------------------------------------------- If
  *   Gamma |- if cond then else => T
  *
  * Gamma |- scrutinee => T
  * Gamma |- pattern_i <= T; Gamma + binds(pattern_i) |- body_i <= R
  *   --------------------------------------------------- Match
  *   Gamma |- match scrutinee { pattern_i => body_i } => R
  *
  * Gamma |- pattern <= Expected
  *   --------------------------------------------------- Pattern
  *   pattern binds locals and checks constructor payloads
  *
  * Gamma |- final <= Expected
  *   --------------------------------------------------- BlockCheck
  *   Gamma |- { items; final } <= Expected
  *
  * Gamma |- left => Numeric; Gamma |- right <= Numeric
  *   --------------------------------------------------- BinaryNumeric
  *   Gamma |- left op right => Numeric
  *
  * Gamma |- operand <= Bool
  *   --------------------------------------------------- UnaryNot
  *   Gamma |- !operand => Bool
  * }}}
  *
  * Explanation:
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
final class MlttTyper(
    module: UntypedModule,
    standardGenerics: Map[String, StandardGenericDescriptor] =
      StandardGenericDescriptors.all,
    profile: CheckerProfile = CheckerProfiles.MlttDependentPatterns,
    expressionMacroProviders: MacroExpressionProviderRegistry =
      MacroExpressionProviderRegistry.default,
    deriveMacroProviders: MacroDeriveProviderRegistry =
      MacroDeriveProviderRegistry.default,
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
    private val compileTimeInts = mutable.LinkedHashMap.empty[String, BigInt]

    def define(symbol: ValueSymbol): Unit =
      values.update(symbol.name, symbol)

    def defineCompileTimeInt(name: String, value: BigInt): Unit =
      compileTimeInts.update(name, value)

    def resolve(name: String): Option[ValueSymbol] =
      values.get(name).orElse(parent.flatMap(_.resolve(name)))

    def resolveCompileTimeInt(name: String): Option[BigInt] =
      compileTimeInts
        .get(name)
        .orElse(
          parent.flatMap(_.resolveCompileTimeInt(name)),
        )

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
      attributes: List[UntypedMacroAttribute],
      visibility: UntypedVisibility,
      span: SourceSpan,
  )

  private final case class VariantInfo(
      name: String,
      fields: List[TypedVariantField],
      attributes: List[UntypedMacroAttribute],
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
      attributes: List[UntypedMacroAttribute],
      visibility: UntypedVisibility,
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
      attributes: List[UntypedMacroAttribute],
      visibility: UntypedVisibility,
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

  private final case class ImplementationFact(
      traitName: String,
      targetName: String,
  )

  private enum ImplOrigin:
    case Source(span: SourceSpan)
    case Derive(providerPath: String, attribute: UntypedMacroAttribute)

  private val diagnostics = ListBuffer.empty[Diagnostic]
  private val macroInvocations = ListBuffer.empty[String]
  private val macroGenerated = ListBuffer.empty[String]
  private val macroConsumedAttributes = ListBuffer.empty[String]
  private val macroExpansionStack = ListBuffer.empty[String]
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
  private val implementationFacts =
    mutable.LinkedHashSet.empty[ImplementationFact]

  private def sameType(left: SourceType, right: SourceType): Boolean =
    MlttTypeChecker.sourceTypesSame(left, right)

  private def assignableType(from: SourceType, to: SourceType): Boolean =
    MlttTypeChecker.sourceTypeAssignable(from, to)

  /** Runs declaration collection before expression inference.
    *
    * Program examples:
    *
    * {{{
    * class Counter { var value: i32 }
    * def bump(value: i32): i32 = { value + 1 }
    * val next = bump(41)
    * }}}
    *
    * The checker first collects class/function signatures so later expression
    * inference can resolve `Counter` constructors and `bump` calls without
    * depending on declaration order.
    */
  def check(): Result[TypedModule] =
    diagnostics ++= expressionMacroProviders.diagnostics
    diagnostics ++= deriveMacroProviders.diagnostics
    collectForeignNamespaceImports()
    collectAliases()
    rawAliases.keys.foreach(resolveAlias)
    collectTraits()
    collectClasses()
    collectImpls()
    expandDeriveMacros()
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
      MacroExpansionSummary(
        macroInvocations.toList,
        macroGenerated.toList,
        macroConsumedAttributes.toList.distinct.sorted,
      ),
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
          field.macroAttributes,
          field.vis,
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
        variant.name ->
          VariantInfo(
            variant.name,
            fields,
            variant.macroAttributes,
            variant.span,
          )
      }.toMap
      val methods = cls.members
        .collect { case fn: UntypedFunction =>
          functionInfo(fn, Some(cls.name))
        }
        .map(info => info.name -> info)
        .toMap

      classes.update(
        cls.name,
        ClassInfo(
          cls.name,
          fields,
          aliases,
          variants,
          methods,
          cls.macroAttributes,
          cls.vis,
          cls.span,
        ),
      )
    }

  private def collectImpls(): Unit =
    module.decls
      .collect { case impl: UntypedImpl => impl }
      .foreach(impl => collectImpl(impl, ImplOrigin.Source(impl.span)))

  private def collectImpl(impl: UntypedImpl, origin: ImplOrigin): Unit =
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
    val fact = ImplementationFact(traitInfo.get.name, target.name)
    if implementationFacts.contains(fact) then
      duplicateImplementationDiagnostic(origin, traitInfo.get.name, target.name)
      return

    val methods = impl.members.collect { case fn: UntypedFunction =>
      functionInfo(fn, Some(targetName))
    }
    val beforeValidation = diagnostics.length
    validateTraitImplementation(impl, traitInfo.get, target, methods)
    if diagnostics.length != beforeValidation then return

    implementationFacts += fact

    val freshMethods =
      methods.filterNot { method =>
        target.methods.contains(method.name)
      }
    val mergedMethods =
      target.methods ++ freshMethods.map(method => method.name -> method)
    classes.update(targetName, target.copy(methods = mergedMethods))
    recordImplementationFacts(origin, fact, methods)

  private def duplicateImplementationDiagnostic(
      origin: ImplOrigin,
      traitName: String,
      targetName: String,
  ): Unit =
    origin match
      case ImplOrigin.Source(span) =>
        error(
          "cosmo0.type.duplicate-impl",
          s"impl $traitName for $targetName is already defined",
          span,
        )
      case ImplOrigin.Derive(providerPath, attribute) =>
        error(
          "cosmo0.macro.duplicate-derive-impl",
          s"derive macro $providerPath generated duplicate impl $traitName for $targetName",
          attribute.span,
        )

  private def recordImplementationFacts(
      origin: ImplOrigin,
      fact: ImplementationFact,
      methods: List[FunctionInfo],
  ): Unit =
    origin match
      case ImplOrigin.Derive(_, _) =>
        macroGenerated +=
          s"impl-fact:${fact.traitName} for ${fact.targetName}"
        methods.map(_.name).distinct.sorted.foreach { methodName =>
          macroGenerated += s"method-set:${fact.targetName}.$methodName"
        }
      case ImplOrigin.Source(_) =>

  private def expandDeriveMacros(): Unit =
    module.decls.foreach {
      case cls: UntypedClass =>
        cls.macroAttributes
          .filter(isDeriveAttribute)
          .foreach(attribute => expandClassDerive(cls, attribute))
        cls.members.foreach(reportUnsupportedMemberDerive)
      case decl =>
        macroAttributes(decl)
          .filter(isDeriveAttribute)
          .foreach(attribute =>
            reportUnsupportedDeriveTarget(
              attribute,
              s"declaration ${decl.name}",
            ),
          )
    }

  private def reportUnsupportedMemberDerive(member: UntypedClassMember): Unit =
    val ownerName = member match
      case value: UntypedValueDecl => s"field ${value.name}"
      case fn: UntypedFunction     => s"method ${fn.name}"
      case alias: UntypedTypeAlias => s"type alias ${alias.name}"
      case variant: UntypedVariant => s"variant ${variant.name}"
    memberMacroAttributes(member)
      .filter(isDeriveAttribute)
      .foreach(attribute => reportUnsupportedDeriveTarget(attribute, ownerName))

  private def expandClassDerive(
      cls: UntypedClass,
      attribute: UntypedMacroAttribute,
  ): Unit =
    if !profile.supports(CheckerProfiles.DeriveMacrosFeature) then
      diagnostics += CheckerProfiles.unsupportedDiagnostic(
        profile,
        CheckerProfiles.DeriveMacrosFeature,
        Some(attribute.span),
      )
      return

    val providerPath = deriveProviderPath(attribute) match
      case Some(path) => path
      case None       => return
    val providerKey = MacroStableDisplay.path(providerPath)
    deriveMacroProviders.resolve(providerKey) match
      case MacroDeriveProviderLookup.Found(provider) =>
        val selectedTrait = selectedDeriveTrait(providerPath, attribute)
        val classInfo = classes.get(cls.name)
        if selectedTrait.isEmpty || classInfo.isEmpty then return
        invokeDeriveMacro(
          provider,
          providerKey,
          attribute,
          classInfo.get,
          selectedTrait.get,
        )
      case MacroDeriveProviderLookup.Disabled(providerIdentity) =>
        error(
          "cosmo0.macro.disabled-provider",
          s"derive macro provider $providerIdentity is disabled",
          attribute.span,
        )
      case MacroDeriveProviderLookup.Missing =>
        error(
          "cosmo0.macro.unresolved-provider",
          s"derive macro provider $providerKey is not registered",
          attribute.span,
        )

  private def invokeDeriveMacro(
      provider: CompilerHostedDeriveProvider,
      providerPath: String,
      attribute: UntypedMacroAttribute,
      classInfo: ClassInfo,
      traitInfo: TraitInfo,
  ): Unit =
    val input =
      deriveFunctionInput(
        provider,
        providerPath,
        attribute,
        classInfo,
        traitInfo,
      )
    val output = CompilerHostedMacroEvaluator.evaluateDerive(provider, input)
    macroInvocations += input.stableDisplay
    macroGenerated ++= output.generatedSourceSummary
    macroConsumedAttributes ++=
      output.consumedAttributes.map(_.stableDisplay)
    diagnostics ++= output.diagnostics
    if output.diagnostics.nonEmpty then return

    val impls =
      validateDeriveOutput(
        providerPath,
        attribute,
        classInfo,
        traitInfo,
        output,
      )
    if impls.isEmpty then return

    val beforeUnconsumed = diagnostics.length
    reportUnconsumedDeriveAttributes(
      providerPath,
      allDeriveInputAttributes(classInfo),
      output.consumedAttributes,
    )
    if diagnostics.length != beforeUnconsumed then return

    impls.foreach(impl =>
      collectImpl(impl, ImplOrigin.Derive(providerPath, attribute)),
    )

  private def deriveFunctionInput(
      provider: CompilerHostedDeriveProvider,
      providerPath: String,
      attribute: UntypedMacroAttribute,
      classInfo: ClassInfo,
      traitInfo: TraitInfo,
  ): MacroFunctionInput =
    MacroFunctionInput(
      providerIdentity = provider.id,
      sourcePackageIdentity = module.source.name,
      invocationIdentity =
        s"${module.source.name}:${attribute.span.start.offset}:${classInfo.name}:$providerPath",
      target = MacroReflectionTarget(
        kind = MacroReflectionTargetKind.Class,
        name = classInfo.name,
        modulePath = macroModulePath,
        visibility = classInfo.visibility,
        fields = classInfo.fields.map(macroReflectionField),
        variants = classInfo.variants.values.toList
          .sortBy(_.name)
          .map(macroReflectionVariant),
        functions = classInfo.methods.values.toList
          .sortBy(_.name)
          .map(macroReflectionFunction),
        attributes = classInfo.attributes,
        span = classInfo.span,
      ),
      cxxContext = MacroCxxExecutionContext(),
      span = attribute.span,
      selectedTrait = Some(macroSelectedTrait(traitInfo, attribute.span)),
    )

  private def macroReflectionField(field: FieldInfo): MacroReflectionField =
    MacroReflectionField(
      field.name,
      field.ty.display,
      field.init.map(MacroExpr.UntypedSource.apply),
      field.attributes,
      field.visibility,
      field.span,
    )

  private def macroReflectionVariant(
      variant: VariantInfo,
  ): MacroReflectionVariant =
    MacroReflectionVariant(
      variant.name,
      variant.fields.map(field =>
        MacroReflectionVariantField(
          field.name,
          field.ty.display,
          field.span,
        ),
      ),
      variant.attributes,
      variant.span,
    )

  private def macroReflectionFunction(
      fn: FunctionInfo,
  ): MacroReflectionFunction =
    MacroReflectionFunction(
      fn.name,
      fn.sig.params.map(_.valueType.display),
      Some(fn.retTy.display),
      fn.attributes,
      fn.visibility,
      fn.span,
    )

  private def macroSelectedTrait(
      traitInfo: TraitInfo,
      span: SourceSpan,
  ): MacroSelectedTrait =
    MacroSelectedTrait(
      traitInfo.name,
      UntypedPath(List(traitInfo.name), span),
      traitInfo.methods.values.toList.sortBy(_.name).map { method =>
        MacroTraitRequirement(
          method.name,
          method.sig.params.map(_.valueType.display),
          method.retTy.display,
          method.sig.receiver.map(receiver =>
            if receiver.mutable then s"&mut ${receiver.valueType.display}"
            else s"&${receiver.valueType.display}",
          ),
          method.span,
        )
      },
      traitInfo.span,
    )

  private def macroModulePath: List[String] =
    val name = module.source.name
    if name.isEmpty || name == "<memory>" then Nil
    else
      name
        .replace('\\', '/')
        .split('/')
        .toList
        .filter(_.nonEmpty)

  private def validateDeriveOutput(
      providerPath: String,
      attribute: UntypedMacroAttribute,
      classInfo: ClassInfo,
      traitInfo: TraitInfo,
      output: MacroFunctionOutput,
  ): List[UntypedImpl] =
    if output.generatedExpr.nonEmpty then
      output.generatedExpr.foreach {
        case _: MacroExpr.TypedArtifact =>
          invalidDeriveOutput(
            providerPath,
            "attempted typed expression injection",
            attribute.span,
          )
        case other =>
          invalidDeriveOutput(
            providerPath,
            s"returned expression output ${other.stableDisplay}",
            attribute.span,
          )
      }
      return Nil

    if output.generatedDeclarations.isEmpty then
      invalidDeriveOutput(
        providerPath,
        "did not return a trait implementation attachment",
        attribute.span,
      )
      return Nil

    val impls = ListBuffer.empty[UntypedImpl]
    output.generatedDeclarations.foreach {
      case GeneratedDeclaration.TraitImplementationAttachment(impl, origin) =>
        if validateDeriveAttachment(
            providerPath,
            classInfo.name,
            traitInfo.name,
            impl,
            origin,
          )
        then impls += impl
    }
    impls.toList

  private def validateDeriveAttachment(
      providerPath: String,
      targetName: String,
      traitName: String,
      impl: UntypedImpl,
      origin: SourceSpan,
  ): Boolean =
    var ok = true
    val actualTarget =
      impl.target.parts.lastOption.getOrElse(impl.target.text)
    val actualTrait =
      impl.traitName.parts.lastOption.getOrElse(impl.traitName.text)

    if actualTarget != targetName then
      invalidDeriveOutput(
        providerPath,
        s"generated impl target $actualTarget instead of $targetName",
        impl.target.span,
      )
      ok = false
    if actualTrait != traitName then
      invalidDeriveOutput(
        providerPath,
        s"generated impl trait $actualTrait instead of $traitName",
        impl.traitName.span,
      )
      ok = false

    impl.members.foreach {
      case _: UntypedFunction =>
      case other =>
        invalidDeriveOutput(
          providerPath,
          s"generated unsupported impl member ${other.getClass.getSimpleName.stripSuffix("$")}",
          origin,
        )
        ok = false
    }
    ok

  private def invalidDeriveOutput(
      providerPath: String,
      message: String,
      span: SourceSpan,
  ): Unit =
    error(
      "cosmo0.macro.invalid-output",
      s"derive macro $providerPath $message",
      span,
    )

  private def reportUnconsumedDeriveAttributes(
      providerPath: String,
      attributes: List[UntypedMacroAttribute],
      consumed: List[UntypedMacroAttribute],
  ): Unit =
    val consumedSet = consumed.toSet
    attributes.filterNot(consumedSet.contains).foreach { attribute =>
      error(
        "cosmo0.macro.unconsumed-attribute",
        s"derive macro $providerPath did not consume ${attribute.stableDisplay}",
        attribute.span,
      )
    }

  private def selectedDeriveTrait(
      providerPath: UntypedPath,
      attribute: UntypedMacroAttribute,
  ): Option[TraitInfo] =
    val traitName = providerPath.parts.lastOption.getOrElse(providerPath.text)
    traits.get(traitName) match
      case Some(traitInfo) => Some(traitInfo)
      case None =>
        error(
          "cosmo0.macro.unsupported-trait",
          s"derive trait $traitName is not a known trait",
          attribute.span,
        )
        None

  private def deriveProviderPath(
      attribute: UntypedMacroAttribute,
  ): Option[UntypedPath] =
    attribute.args match
      case List(
            UntypedMacroAttributeArg(
              None,
              UntypedMacroAttributeValue.PathValue(path),
              _,
            ),
          ) =>
        Some(path)
      case _ =>
        error(
          "cosmo0.macro.invalid-provider",
          "@derive(...) expects exactly one provider path argument",
          attribute.span,
        )
        None

  private def allDeriveInputAttributes(
      classInfo: ClassInfo,
  ): List[UntypedMacroAttribute] =
    classInfo.attributes :::
      classInfo.fields.flatMap(_.attributes) :::
      classInfo.variants.values.toList.flatMap(_.attributes) :::
      classInfo.methods.values.toList.flatMap(_.attributes)

  private def macroAttributes(decl: UntypedDecl): List[UntypedMacroAttribute] =
    decl match
      case cls: UntypedClass       => cls.macroAttributes
      case trt: UntypedTrait       => trt.macroAttributes
      case fn: UntypedFunction     => fn.macroAttributes
      case value: UntypedValueDecl => value.macroAttributes
      case _                       => Nil

  private def memberMacroAttributes(
      member: UntypedClassMember,
  ): List[UntypedMacroAttribute] =
    member match
      case fn: UntypedFunction     => fn.macroAttributes
      case value: UntypedValueDecl => value.macroAttributes
      case variant: UntypedVariant => variant.macroAttributes
      case _                       => Nil

  private def isDeriveAttribute(attribute: UntypedMacroAttribute): Boolean =
    attribute.path.parts == List("derive")

  private def reportUnsupportedDeriveTarget(
      attribute: UntypedMacroAttribute,
      targetDescription: String,
  ): Unit =
    error(
      "cosmo0.macro.unsupported-target",
      s"@derive(...) cannot target $targetDescription",
      attribute.span,
    )

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
        sameType(l.valueType, r.valueType)
      } &&
      sameType(left.returnType, right.returnType)

  private def sameRecv(
      left: Option[CallableReceiver],
      right: Option[CallableReceiver],
  ): Boolean =
    (left, right) match
      case (None, None) =>
        true
      case (Some(l), Some(r)) =>
        l.mutable == r.mutable && sameType(l.valueType, r.valueType)
      case _ =>
        false

  private def collectFunctions(): Unit =
    module.decls.collect { case fn: UntypedFunction => fn }.foreach { fn =>
      val info = functionInfo(fn, None)
      functions.update(info.name, info)
    }

  /** Converts one top-level declaration into its typed form.
    *
    * Program examples:
    *
    * {{{
    * val answer = 42
    * def id(value: i32): i32 = { value }
    * class Box { val value: i32 }
    * }}}
    *
    * Value declarations define globals in the shared scope after their
    * initializer has been inferred or checked. Functions and classes use the
    * pre-collected signatures from the collection pass.
    */
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

  /** Checks class fields, variants, aliases, and methods.
    *
    * Program examples:
    *
    * {{{
    * class Point {
    *   val x: i32 = 0
    *   val y: i32
    *   def sum(&self): i32 = { self.x + self.y }
    * }
    * }}}
    *
    * Field initializers are checked against explicit field types. Method bodies
    * are checked with the global scope so method lookup remains owner-based
    * rather than accidentally capturing sibling methods as locals.
    */
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

  /** Checks a function or method body against its declared signature.
    *
    * Program examples:
    *
    * {{{
    * def clamp(value: i32, min: i32 = 0): i32 = {
    *   if value < min { min } else { value }
    * }
    * }}}
    *
    * Parameter annotations seed the local scope. Defaults and the body receive
    * expected types from the resolved signature, so literals and branches can
    * infer in context.
    */
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

  /** Infers or checks a top-level value declaration.
    *
    * Program examples:
    *
    * {{{
    * val inferred = 42      // inferred: i32
    * val checked: u8 = 42   // literal checked as u8
    * }}}
    *
    * An explicit annotation supplies the expected initializer type. Without an
    * annotation, the initializer type becomes the declaration type.
    */
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

  /** Resolves a function signature before body checking.
    *
    * Program examples:
    *
    * {{{
    * class Counter {
    *   def add(&mut self, amount: i32): Unit = { self.value += amount }
    * }
    * }}}
    *
    * Parameters require explicit types. A leading `self` parameter is
    * normalized into receiver metadata, which later method-call inference uses
    * for receiver mutability checks.
    */
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
      fn.macroAttributes,
      fn.vis,
    )

  /** Infers an expression type, optionally under an expected type.
    *
    * Program examples:
    *
    * {{{
    * val byte: u8 = 65
    * val total = byte.to_i32() + 1
    * }}}
    *
    * The `expected` type flows into literals, calls, branches, returns, and the
    * final expression of a block. When no expected type is available, the
    * expression chooses its default inference rule.
    */
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
      case value: UntypedBlockCall =>
        blockMacroCallExpr(value, scope, expected, context)
      case value: UntypedAssign =>
        assignExpr(value, scope, context)
      case value: UntypedUnary =>
        unaryExpr(value, scope, expected, context)
      case value: UntypedBinary =>
        binaryExpr(value, scope, expected, context)
      case value: UntypedIf =>
        ifExpr(value, scope, expected, context)
      case value: UntypedLoop =>
        loopExpr(value, scope, context)
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
      case value: UntypedTemplate =>
        templateMacroExpr(value, scope, expected, context)
      case value: UntypedUnitLiteral =>
        ExprInfo(TypedUnitLiteral(SourceType.Unit, value.span), false, false)

  /** Checks a block left-to-right and infers its result from the final item.
    *
    * Program examples:
    *
    * {{{
    * val result = {
    *   val base = 40
    *   base + 2
    * }
    * }}}
    *
    * Only the final item receives the caller's expected type; earlier items are
    * checked without it so local inference remains statement-local.
    */
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

  /** Checks one item inside a block and updates block scope.
    *
    * Program examples:
    *
    * {{{
    * {
    *   val x = 1
    *   var y: i32 = x + 1
    *   y
    * }
    * }}}
    *
    * Local declarations follow the same annotation-or-initializer rule as
    * top-level values, then define a local symbol for later items.
    */
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
      case alias: UntypedCompileTimeIntAlias =>
        compileTimeIntExpr(alias.value, scope).foreach(value =>
          scope.defineCompileTimeInt(alias.name, value),
        )
        TypedExprStmt(
          TypedUnitLiteral(SourceType.Unit, alias.span),
          alias.span,
        )
      case stmt: UntypedExprStmt =>
        val typed = expr(stmt.expr, scope, expected, context)
        TypedExprStmt(typed.expr, stmt.span)
      case expression: UntypedExpr =>
        val typed = expr(expression, scope, expected, context)
        typed.expr

  /** Infers the type of a name.
    *
    * Program examples:
    *
    * {{{
    * val value = 1
    * val copied = value
    * val empty = Vec[i32]()
    * }}}
    *
    * Lexical bindings win first. If no value binding exists, a single-segment
    * name may infer as a class constructor or standard generic constructor.
    */
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
            scope.resolveCompileTimeInt(name) match
              case Some(value) =>
                ExprInfo(
                  TypedIntLiteral(value, SourceType.I32, node.span),
                  mutBinding = false,
                  mutAllowed = false,
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
                                if sameType(sig.returnType, owner) =>
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

  private def compileTimeIntExpr(
      node: UntypedExpr,
      scope: Scope,
  ): Option[BigInt] =
    node match
      case value: UntypedIntLiteral =>
        Some(value.value)
      case UntypedName(path, span) if path.parts.length == 1 =>
        val name = path.parts.head
        scope.resolveCompileTimeInt(name) match
          case Some(value) => Some(value)
          case None =>
            error(
              "cosmo0.cte.local-int-alias.unresolved-name",
              s"unresolved compile-time integer alias $name",
              span,
            )
            None
      case UntypedUnary("+", expr, _) =>
        compileTimeIntExpr(expr, scope)
      case UntypedUnary("-", expr, _) =>
        compileTimeIntExpr(expr, scope).map(-_)
      case UntypedBinary(op, left, right, span)
          if Set("+", "-", "*", "/", "%").contains(op) =>
        val leftValue = compileTimeIntExpr(left, scope)
        val rightValue = compileTimeIntExpr(right, scope)
        leftValue.zip(rightValue).flatMap { case (lhs, rhs) =>
          op match
            case "+" => Some(lhs + rhs)
            case "-" => Some(lhs - rhs)
            case "*" => Some(lhs * rhs)
            case "/" | "%" if rhs == 0 =>
              error(
                "cosmo0.cte.local-int-alias.divide-by-zero",
                "compile-time integer alias division by zero",
                span,
              )
              None
            case "/" => Some(lhs / rhs)
            case "%" => Some(lhs % rhs)
            case _   => None
        }
      case other =>
        error(
          "cosmo0.cte.local-int-alias.unsupported-expression",
          "local compile-time integer aliases currently support integer literals, alias names, unary +/- and binary + - * / %",
          other.span,
        )
        None

  /** Infers a direct type-constructor expression.
    *
    * Program examples:
    *
    * {{{
    * val make_vec = Vec[i32]
    * val values = Vec[i32]()
    * }}}
    *
    * Only constructible standard or foreign applied types produce callable
    * constructor types here.
    */
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

  /** Infers a field, method, foreign method, or variant constructor selection.
    *
    * Program examples:
    *
    * {{{
    * class Nat { case Zero; case Succ(Nat) }
    * val zero = Nat.Zero
    * val size = values.len()
    * val field = point.x
    * }}}
    *
    * The receiver type determines whether selection resolves to variant
    * metadata, field type, callable method signature, descriptor method, or
    * foreign method surface.
    */
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

  /** Builds the typed error expression for failed field or method inference.
    *
    * Program examples:
    *
    * {{{
    * value.missing
    * }}}
    *
    * The surrounding inference rule can keep producing a typed tree after the
    * diagnostic, which avoids cascading failures in later expressions.
    */
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

  /** Infers a fully qualified variant constructor expression.
    *
    * Program examples:
    *
    * {{{
    * val ok = Result[i32, String]::Ok
    * val one = Result[i32, String]::Ok(1)
    * }}}
    *
    * Nullary constructors infer directly as their result type. Constructors
    * with payloads infer as function types until a call supplies arguments.
    */
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

  /** Resolves an imported foreign qualified name during expression inference.
    *
    * Program examples:
    *
    * {{{
    * import cpp namespace std as std
    * val symbol = std::vector
    * }}}
    *
    * If the root segment is a foreign namespace alias, the source path infers
    * as a foreign symbol rather than a user-defined variant constructor.
    */
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

  /** Builds the canonical C++ spelling for a foreign symbol.
    *
    * Program examples:
    *
    * {{{
    * std + vector + push_back => ::std::vector::push_back
    * }}}
    */
  private def foreignCanonicalName(
      importValue: SourceCppNamespaceImport,
      suffix: List[String],
  ): String =
    val suffixName = suffix.mkString("::")
    if suffixName.isEmpty then importValue.namespace.cppName
    else s"${importValue.namespace.cppName}::$suffixName"

  /** Infers a call expression from callee shape and expected argument types.
    *
    * Program examples:
    *
    * {{{
    * def add(left: i32, right: i32): i32 = { left + right }
    * val total = add(1, 2)
    * val next = counter.bump(1)
    * val some = Option[i32]::Some(1)
    * }}}
    *
    * Named functions, constructors, variant constructors, selected methods, and
    * function-valued expressions all converge on `callWithSig` once a callable
    * signature is known.
    */
  private def callExpr(
      node: UntypedCall,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    expandExpressionMacroCall(node, scope, expected, context) match
      case Some(expanded) => return expanded
      case None           => ()

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
                              if sameType(
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

  /** Expands expression macros at the expression checking site.
    *
    * Provider output is never typed directly. A successful provider must return
    * `Expr[Untyped]`; the checker then recursively checks that untyped
    * expression with the caller's current expected type.
    */
  private def expandExpressionMacroCall(
      node: UntypedCall,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): Option[ExprInfo] =
    macroProviderPathFromCallee(node.callee, scope) match
      case None =>
        None
      case Some(path) =>
        val providerPath = MacroStableDisplay.path(path)
        expressionMacroProviders.resolveFree(providerPath) match
          case MacroProviderLookup.Found(provider) =>
            Some(
              invokeExpressionMacro(
                provider,
                providerPath,
                node.span,
                macroArgsPayload(None, node.args, node.span),
                scope,
                expected,
                context,
              ),
            )
          case MacroProviderLookup.Disabled(providerIdentity) =>
            Some(disabledMacroProvider(providerIdentity, node.callee.span))
          case MacroProviderLookup.Missing
              if expressionMacroProviders.hasFreeCandidate(providerPath) =>
            Some(unresolvedMacroProvider(providerPath, node.callee.span))
          case MacroProviderLookup.Missing =>
            None

  private def blockMacroCallExpr(
      node: UntypedBlockCall,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    macroProviderPathFromCallee(node.callee, scope) match
      case None =>
        unsupportedMacroPayload(
          "block-attached expression macro target does not resolve to a provider path",
          node.span,
        )
      case Some(path) =>
        val providerPath = MacroStableDisplay.path(path)
        expressionMacroProviders.resolveBlock(providerPath) match
          case MacroProviderLookup.Found(provider) =>
            invokeExpressionMacro(
              provider,
              providerPath,
              node.span,
              MacroExpr.Block(node.block),
              scope,
              expected,
              context,
            )
          case MacroProviderLookup.Disabled(providerIdentity) =>
            disabledMacroProvider(providerIdentity, node.callee.span)
          case MacroProviderLookup.Missing
              if expressionMacroProviders.hasBlockCandidate(providerPath) =>
            unresolvedMacroProvider(providerPath, node.callee.span)
          case MacroProviderLookup.Missing =>
            unsupportedMacroPayload(
              s"block-attached expression macro target $providerPath does not resolve to a registered provider",
              node.callee.span,
            )

  private def templateMacroExpr(
      node: UntypedTemplate,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    val providerPath = MacroStableDisplay.path(node.tag)
    expressionMacroProviders.resolveTemplate(providerPath) match
      case MacroProviderLookup.Found(provider) =>
        invokeExpressionMacro(
          provider,
          providerPath,
          node.span,
          MacroExpr.Template(node.tag, node.parts, node.span),
          scope,
          expected,
          context,
        )
      case MacroProviderLookup.Disabled(providerIdentity) =>
        disabledMacroProvider(providerIdentity, node.tag.span)
      case MacroProviderLookup.Missing
          if expressionMacroProviders.hasTemplateCandidate(providerPath) =>
        unresolvedMacroProvider(providerPath, node.tag.span)
      case MacroProviderLookup.Missing =>
        unsupportedMacroPayload(
          s"template literal tag $providerPath does not resolve to a registered expression macro provider",
          node.tag.span,
        )

  private def macroProviderPathFromCallee(
      callee: UntypedExpr,
      scope: Scope,
  ): Option[UntypedPath] =
    macroPathFromCallee(callee).filterNot(pathRootResolvesAsValue(_, scope))

  private def macroPathFromCallee(
      callee: UntypedExpr,
  ): Option[UntypedPath] =
    callee match
      case UntypedName(path, _) =>
        Some(path)
      case UntypedSelect(recv, field, span) =>
        macroPathFromCallee(recv).map(path =>
          UntypedPath(path.parts :+ field, span),
        )
      case _ =>
        None

  private def pathRootResolvesAsValue(
      path: UntypedPath,
      scope: Scope,
  ): Boolean =
    path.parts.headOption.exists { root =>
      scope.resolve(root).nonEmpty ||
      functions.contains(root) ||
      classes.contains(root) ||
      foreignAliases.contains(root)
    }

  private def macroArgsPayload(
      receiver: Option[UntypedExpr],
      args: List[UntypedCallArg],
      span: SourceSpan,
  ): MacroExpr.Args =
    val positional = ListBuffer.empty[UntypedExpr]
    val named = ListBuffer.empty[MacroExpr.NamedArg]
    args.foreach {
      case UntypedCallArg.Positional(value, _) =>
        positional += value
      case UntypedCallArg.Named(name, value, argSpan) =>
        named += MacroExpr.NamedArg(name, value, argSpan)
    }
    MacroExpr.Args(receiver, positional.toList, named.toList, span)

  private def invokeExpressionMacro(
      provider: CompilerHostedExpressionProvider,
      providerPath: String,
      span: SourceSpan,
      payload: MacroExpr,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    if !profile.supports(CheckerProfiles.MacrosFeature) then
      diagnostics += CheckerProfiles.unsupportedDiagnostic(
        profile,
        CheckerProfiles.MacrosFeature,
        Some(span),
      )
      return macroErrorExpr(span)

    if macroExpansionStack.contains(provider.id) then
      error(
        "cosmo0.macro.expansion-cycle",
        s"expression macro expansion cycle includes ${provider.id}",
        span,
      )
      return macroErrorExpr(span)

    val input = macroFunctionInput(provider, providerPath, span, payload)
    macroExpansionStack += provider.id
    val output =
      CompilerHostedMacroEvaluator.evaluateExpression(provider, input)
    macroInvocations += input.stableDisplay
    macroGenerated ++= output.generatedSourceSummary
    macroConsumedAttributes ++=
      output.consumedAttributes.map(_.stableDisplay)
    diagnostics ++= output.diagnostics

    val result =
      if output.diagnostics.nonEmpty then macroErrorExpr(span)
      else
        validateExpressionMacroOutput(
          providerPath,
          output,
          span,
          scope,
          expected,
          context,
        )
    macroExpansionStack.remove(macroExpansionStack.length - 1)
    result

  private def macroFunctionInput(
      provider: CompilerHostedExpressionProvider,
      providerPath: String,
      span: SourceSpan,
      payload: MacroExpr,
  ): MacroFunctionInput =
    MacroFunctionInput(
      providerIdentity = provider.id,
      sourcePackageIdentity = module.source.name,
      invocationIdentity = s"${module.source.name}:${span.start.offset}",
      target = MacroReflectionTarget(
        kind = MacroReflectionTargetKind.Expression,
        name = providerPath,
        modulePath = Nil,
        visibility = UntypedVisibility.Private,
        fields = Nil,
        variants = Nil,
        functions = Nil,
        attributes = Nil,
        span = span,
      ),
      cxxContext = MacroCxxExecutionContext(),
      span = span,
      payload = Some(payload),
    )

  private def validateExpressionMacroOutput(
      providerPath: String,
      output: MacroFunctionOutput,
      span: SourceSpan,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    if output.generatedDeclarations.nonEmpty then
      error(
        "cosmo0.macro.invalid-output",
        s"expression macro $providerPath returned declaration output",
        span,
      )
      return macroErrorExpr(span)

    output.generatedExpr match
      case Some(MacroExpr.UntypedSource(expanded)) =>
        expr(expanded, scope, expected, context)
      case Some(_: MacroExpr.TypedArtifact) =>
        error(
          "cosmo0.macro.invalid-output",
          s"expression macro $providerPath attempted typed expression injection",
          span,
        )
        macroErrorExpr(span)
      case Some(other) =>
        error(
          "cosmo0.macro.invalid-output",
          s"expression macro $providerPath returned unsupported output ${other.stableDisplay}",
          span,
        )
        macroErrorExpr(span)
      case None =>
        error(
          "cosmo0.macro.invalid-output",
          s"expression macro $providerPath did not return Expr[Untyped]",
          span,
        )
        macroErrorExpr(span)

  private def unresolvedMacroProvider(
      providerPath: String,
      span: SourceSpan,
  ): ExprInfo =
    error(
      "cosmo0.macro.unresolved-provider",
      s"expression macro provider $providerPath is not registered",
      span,
    )
    macroErrorExpr(span)

  private def disabledMacroProvider(
      providerIdentity: String,
      span: SourceSpan,
  ): ExprInfo =
    error(
      "cosmo0.macro.disabled-provider",
      s"expression macro provider $providerIdentity is disabled",
      span,
    )
    macroErrorExpr(span)

  private def unsupportedMacroPayload(
      message: String,
      span: SourceSpan,
  ): ExprInfo =
    error("cosmo0.macro.unsupported-payload", message, span)
    macroErrorExpr(span)

  private def macroErrorExpr(span: SourceSpan): ExprInfo =
    ExprInfo(
      TypedUnitLiteral(SourceType.Error, span),
      mutBinding = false,
      mutAllowed = false,
    )

  /** Resolves selected calls before generic function-value call inference.
    *
    * Program examples:
    *
    * {{{
    * val n = Nat.Succ(Nat.Zero)
    * values.push(1)
    * builder.finish()
    * }}}
    *
    * This rule handles `Owner.Variant(...)` specially, then checks descriptor,
    * foreign, and class methods with receiver mutability before validating
    * arguments.
    */
  private def methodOrVariantCall(
      select: UntypedSelect,
      args: List[UntypedCallArg],
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
                if cls.fields.exists(_.name == select.field) then
                  val selected = selectExpr(select, scope, context)
                  callFunctionValue(selected, args, span, scope, context)
                else
                  expandMethodExpressionMacroCall(
                    select,
                    args,
                    recv,
                    scope,
                    expected,
                    context,
                  ).getOrElse {
                    val selected = selectExpr(select, scope, context)
                    callFunctionValue(selected, args, span, scope, context)
                  }
          case None =>
            expandMethodExpressionMacroCall(
              select,
              args,
              recv,
              scope,
              expected,
              context,
            ).getOrElse {
              val selected = selectExpr(select, scope, context)
              callFunctionValue(selected, args, span, scope, context)
            }

  private def expandMethodExpressionMacroCall(
      select: UntypedSelect,
      args: List[UntypedCallArg],
      recv: ExprInfo,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): Option[ExprInfo] =
    macroReceiverTypeName(recv.expr.ty) match
      case None =>
        None
      case Some(receiverType) =>
        expressionMacroProviders.resolveMethod(receiverType, select.field) match
          case MacroProviderLookup.Found(provider) =>
            Some(
              invokeExpressionMacro(
                provider,
                provider.id,
                select.span,
                macroArgsPayload(Some(select.recv), args, select.span),
                scope,
                expected,
                context,
              ),
            )
          case MacroProviderLookup.Disabled(providerIdentity) =>
            Some(disabledMacroProvider(providerIdentity, select.span))
          case MacroProviderLookup.Missing =>
            None

  private def macroReceiverTypeName(ty: SourceType): Option[String] =
    SourceType.dealias(ty) match
      case SourceType.User(name) => Some(name)
      case _                     => None

  /** Checks a call whose callee already inferred as an expression.
    *
    * Program examples:
    *
    * {{{
    * val apply: (i32) -> i32 = inc
    * val next = apply(41)
    * }}}
    *
    * Only `SourceType.Function` is callable here; all named and method calls
    * should already have been resolved to a `CallableSignature`.
    */
  private def callFunctionValue(
      callee: ExprInfo,
      args: List[UntypedCallArg],
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
        rejectNamedCallArgs(args)
        val typedArgs =
          args.map(arg => expr(arg.value, scope, None, context).expr)
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

  /** Checks arguments against a resolved callable signature and returns result
    * type.
    *
    * Program examples:
    *
    * {{{
    * def take(value: u8): Unit = {}
    * take(65) // integer literal is checked with expected u8
    * }}}
    *
    * Each argument is inferred with its parameter type as `expected`, then
    * `canPass` applies assignment and reference-passing rules.
    */
  private def callWithSig(
      callee: TypedExpr,
      sig: CallableSignature,
      args: List[UntypedCallArg],
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
    rejectNamedCallArgs(args)
    val typedArgs = args.zipWithIndex.map { case (arg, index) =>
      val expectedTy = sig.params.lift(index).map(_.valueType)
      val typed = expr(arg.value, scope, expectedTy, context)
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

  private def rejectNamedCallArgs(args: List[UntypedCallArg]): Unit =
    args.foreach {
      case UntypedCallArg.Named(name, _, span) =>
        error(
          "cosmo0.type.unsupported-named-argument",
          s"ordinary cosmo0 calls do not support named argument $name",
          span,
        )
      case _: UntypedCallArg.Positional =>
    }

  /** Builds a typed placeholder for a call whose callee rule already failed.
    *
    * Program examples:
    *
    * {{{
    * missing(1)
    * }}}
    *
    * Arguments are still inferred so their diagnostics are preserved, but the
    * call itself receives an error signature.
    */
  private def errorCall(
      node: UntypedCall,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    val callee = expr(node.callee, scope, None, context)
    rejectNamedCallArgs(node.args)
    val args = node.args.map(arg => expr(arg.value, scope, None, context).expr)
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

  /** Checks calls to trusted runtime functions.
    *
    * Program examples:
    *
    * {{{
    * println("hello")
    * print("prefix")
    * }}}
    *
    * Runtime calls use `TrustedExternAbi` signatures instead of ordinary source
    * declarations, but argument inference still uses the parameter types as
    * expected types.
    */
  private def runtimeFunctionCall(
      calleeName: String,
      name: UntypedName,
      args: List[UntypedCallArg],
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
    rejectNamedCallArgs(args)
    val typedArgs = args.zipWithIndex.map { case (arg, index) =>
      expr(
        arg.value,
        scope,
        sig.params.lift(index).map(_.valueType),
        context,
      ).expr
    }
    typedArgs.zip(sig.params).zipWithIndex.foreach {
      case ((actual, param), index) =>
        val paramType = param.valueType
        if !assignableType(actual.ty, paramType) then
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

  /** Detects source names handled by the trusted runtime call rule.
    *
    * Program examples:
    *
    * {{{
    * println("hello")
    * }}}
    */
  private def isRuntimeFunction(name: String): Boolean =
    TrustedExternAbi.isTrustedSourceName(name)

  /** Checks assignment and compound numeric assignment.
    *
    * Program examples:
    *
    * {{{
    * var count: i32 = 0
    * count += 1
    * count = 10
    * }}}
    *
    * The target must be a mutable binding. The right side is checked against
    * the target type, and compound assignments additionally require a numeric
    * target.
    */
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
    if !assignableType(value.expr.ty, target.expr.ty)
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

  /** Infers unary operator expressions.
    *
    * Program examples:
    *
    * {{{
    * val no = !flag
    * val neg = -amount
    * val ref = &value
    * val value = *ref
    * }}}
    *
    * Boolean negation checks `Bool`; numeric negation preserves the operand
    * numeric type; reference and dereference rules construct or inspect
    * `SourceType.Ref` while carrying mutability.
    */
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

  /** Infers binary operator expressions.
    *
    * Program examples:
    *
    * {{{
    * val ok = left < right and enabled
    * val sum: i64 = 1 + 2
    * val same = name == other
    * }}}
    *
    * Boolean operators check both operands as `Bool`; comparisons return
    * `Bool`; arithmetic preserves the common numeric operand type and passes an
    * expected numeric type to the left operand when available.
    */
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
        if !sameType(left.expr.ty, right.expr.ty) then
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
        if !SourceType.isNumeric(left.expr.ty) || !sameType(
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
        if !SourceType.isNumeric(left.expr.ty) || !sameType(
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

  /** Checks conditionals and infers their common branch type.
    *
    * Program examples:
    *
    * {{{
    * val result: i32 = if flag { 1 } else { 2 }
    * if ready { println("ready") }
    * }}}
    *
    * The condition checks as `Bool`. Both branches receive the outer expected
    * type, then their inferred types must match; an `if` without `else` is
    * `Unit`.
    */
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
          if sameType(
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

  /** Checks canonical loops and binds for-each loop items when present.
    *
    * Program examples:
    *
    * {{{
    * loop { break }
    * while (keep_running()) { tick() }
    * for item in values { println(item.to_string()) }
    * }}}
    */
  private def loopExpr(
      node: UntypedLoop,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    val prologue =
      node.prologue.map(expr(_, scope, Some(SourceType.Unit), context).expr)
    val (condition, bodyScope) =
      typedLoopCondition(node.condition, scope, context)
    val body =
      expr(node.body, bodyScope, Some(SourceType.Unit), context)
    val epilogue =
      node.epilogue.map(expr(_, bodyScope, Some(SourceType.Unit), context).expr)
    ExprInfo(
      TypedLoop(
        prologue,
        condition,
        body.expr,
        epilogue,
        SourceType.Unit,
        node.span,
      ),
      false,
      true,
    )

  private def typedLoopCondition(
      condition: UntypedLoopCondition,
      scope: Scope,
      context: FunctionContext,
  ): (TypedLoopCondition, Scope) =
    condition match
      case UntypedLoopCondition.Always(span) =>
        TypedLoopCondition.Always(span) -> scope.child
      case UntypedLoopCondition.SourceCondition(value) =>
        val cond = expr(value, scope, Some(SourceType.Bool), context)
        requireBool(cond, value.span)
        TypedLoopCondition.SourceCondition(cond.expr) -> scope.child
      case value: UntypedLoopCondition.ForEach =>
        typedForEachLoopCondition(value, scope, context)

  /** Checks supported collection iteration and binds the loop item type.
    *
    * Program examples:
    *
    * {{{
    * for item in values { println(item.to_string()) }
    * for key in table { println(key) }
    * }}}
    *
    * Vec/Set/Arena iterate their element type. Map iteration currently binds
    * the key type. The loop body is expected to type as `Unit`.
    */
  private def typedForEachLoopCondition(
      node: UntypedLoopCondition.ForEach,
      scope: Scope,
      context: FunctionContext,
  ): (TypedLoopCondition, Scope) =
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
    TypedLoopCondition
      .ForEach(
        node.name,
        itemTy,
        iter.expr,
        node.span,
      ) -> bodyScope

  /** Checks ordinary cosmo0 match expressions.
    *
    * Program examples:
    *
    * {{{
    * value match {
    *   case Option[i32]::Some(item) => item
    *   case Option[i32]::None => 0
    * }
    * }}}
    *
    * The scrutinee type drives pattern checking. Arm bodies receive the outer
    * expected type and must infer a common result type when present.
    */
  private def matchExpr(
      node: UntypedMatch,
      scope: Scope,
      expected: Option[SourceType],
      context: FunctionContext,
  ): ExprInfo =
    val scrut = expr(node.scrut, scope, None, context)
    val arms = node.arms.map { arm =>
      val armScope = scope.child
      val typedPattern =
        pattern(arm.pat, scrut.expr.ty, armScope)
      val body = arm.body.map(expr(_, armScope, expected, context))
      TypedMatchArm(typedPattern, body.map(_.expr), arm.span)
    }
    diagnostics ++= MlttTypeChecker.validateDependentPatternMatch(
      profile,
      scrut.expr.ty,
      node.arms,
    )
    val bodyTypes = arms.flatMap(_.body.map(_.ty))
    val ty =
      if bodyTypes.isEmpty then SourceType.Unit
      else
        val first = bodyTypes.head
        if bodyTypes.tail.forall(ty => sameType(first, ty))
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

  /** Checks a return expression against the enclosing function result type.
    *
    * Program examples:
    *
    * {{{
    * def answer(): i32 = {
    *   return 42
    * }
    * }}}
    *
    * The returned value receives the function return type as its expected type.
    * The return expression itself has type `Never` after checking.
    */
  private def returnExpr(
      node: UntypedReturn,
      scope: Scope,
      context: FunctionContext,
  ): ExprInfo =
    context match
      case FunctionContext.Some(retTy, _) =>
        val value = expr(node.value, scope, Some(retTy), context)
        if !assignableType(value.expr.ty, retTy) then
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

  /** Checks a source match pattern against the expected scrutinee/payload type.
    *
    * Program examples:
    *
    * {{{
    * value match {
    *   case 0 => "zero"
    *   case Option[i32]::Some(item) => item.to_string()
    *   case _ => "other"
    * }
    * }}}
    *
    * Patterns do not infer independently. Literals check against the expected
    * type, bindings define locals of that type, and variant patterns delegate
    * payload checking to the constructor signature.
    */
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
        if !sameType(expectedTy, SourceType.Bool) then
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
        if !sameType(expectedTy, SourceType.Byte) then
          invalidPatternType(value.span, expectedTy, SourceType.Byte)
        TypedIntLiteral(
          asciiLiteralValue(value.value, value.span),
          SourceType.Byte,
          value.span,
        )
      case value: UntypedRuneLiteral =>
        if !sameType(expectedTy, SourceType.Rune) then
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
        if !sameType(expectedTy, SourceType.String) then
          invalidPatternType(value.span, expectedTy, SourceType.String)
        TypedStringLiteral(value.value, SourceType.String, value.span)
      case value: UntypedVariantPattern =>
        variantPattern(value, expectedTy, scope)

  /** Checks a variant pattern and its payload patterns.
    *
    * Program examples:
    *
    * {{{
    * result match {
    *   case Result[i32, String]::Ok(value) => value
    *   case Result[i32, String]::Err(message) => 0
    * }
    * }}}
    *
    * The selected constructor must return the expected scrutinee type. Each
    * nested pattern is checked against the matching constructor payload type.
    */
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
        if !sameType(callable.returnType, expectedTy) then
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

  /** Resolves constructor syntax used in a pattern.
    *
    * Program examples:
    *
    * {{{
    * case Nat.Succ(rest) => rest
    * case Result[i32, String]::Ok(value) => value
    * }}}
    *
    * Pattern constructors accept the same owner-qualified forms that expression
    * constructors use, but they are resolved under the scrutinee's expected
    * type.
    */
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

  /** Looks up the callable signature of a variant constructor.
    *
    * Program examples:
    *
    * {{{
    * Nat.Succ(Nat.Zero)
    * Option[i32]::Some(1)
    * }}}
    *
    * Standard generic constructors come from descriptor metadata. User variants
    * come from collected class metadata.
    */
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

  /** Resolves a nullary standard descriptor constructor by source name.
    *
    * Program examples:
    *
    * {{{
    * val text = String()
    * val values = Vec[i32]()
    * }}}
    *
    * Descriptor constructors let builtin and standard generic families behave
    * like source constructors during call inference.
    */
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

  /** Resolves a method from standard descriptor metadata.
    *
    * Program examples:
    *
    * {{{
    * val length = values.len()
    * val text = number.to_string()
    * }}}
    *
    * The receiver type is normalized before descriptor lookup so references use
    * the same method surface as their targets.
    */
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

  /** Resolves a supported foreign method surface.
    *
    * Program examples:
    *
    * {{{
    * cxx_vector.push_back(1)
    * val size = cxx_vector.size()
    * }}}
    *
    * Foreign methods are intentionally explicit descriptor shims; only known
    * imported runtime surfaces infer callable signatures here.
    */
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

  /** Resolves the supported `std::vector` method signatures.
    *
    * Program examples:
    *
    * {{{
    * cxx_vector.push_back(value)
    * val n = cxx_vector.size()
    * }}}
    *
    * These signatures enter the same method-call inference rule as source
    * methods, including receiver mutability checks.
    */
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

  /** Finds the nullary descriptor owner used by constructor-call inference.
    *
    * Program examples:
    *
    * {{{
    * String()
    * }}}
    */
  private def descriptorOwner(name: String): Option[SourceType] =
    standardGenerics.get(name).filter(_.arity == 0).flatMap { _ =>
      SourceType.scalar(name)
    }

  /** Removes references and aliases before descriptor method lookup.
    *
    * Program examples:
    *
    * {{{
    * (&String).len()  // lookup is performed on String
    * }}}
    */
  private def normalizeDescriptorOwner(ownerType: SourceType): SourceType =
    SourceType.dealias(ownerType) match
      case SourceType.Ref(target, _) => SourceType.dealias(target)
      case other                     => other

  /** Extracts the descriptor family name from a normalized owner type.
    *
    * Program examples:
    *
    * {{{
    * Vec[i32].len()  // descriptor name Vec
    * }}}
    */
  private def descriptorName(ownerType: SourceType): Option[String] =
    normalizeDescriptorOwner(ownerType) match
      case SourceType.Standard(name, _) => Some(name)
      case SourceType.Builtin(name)     => Some(name)
      case _                            => None

  /** Extracts descriptor arity for generic-method inference.
    *
    * Program examples:
    *
    * {{{
    * Vec[i32] => 1
    * String   => 0
    * }}}
    */
  private def descriptorArity(ownerType: SourceType): Int =
    normalizeDescriptorOwner(ownerType) match
      case SourceType.Standard(_, args) => args.length
      case SourceType.Builtin(_)        => 0
      case _                            => -1

  /** Resolves a source class constructor signature by class name.
    *
    * Program examples:
    *
    * {{{
    * Point(1, 2)
    * }}}
    */
  private def classCtor(
      name: String,
      span: SourceSpan,
  ): Option[CallableSignature] =
    classes.get(name).map(_.ctorSig)

  /** Finds class metadata for field, method, and constructor inference.
    *
    * Program examples:
    *
    * {{{
    * point.x
    * point.translate(1, 2)
    * }}}
    */
  private def classInfoFor(ty: SourceType): Option[ClassInfo] =
    SourceType.dealias(ty) match
      case SourceType.User(name)                    => classes.get(name)
      case SourceType.Ref(SourceType.User(name), _) => classes.get(name)
      case _                                        => None

  /** Resolves a type alias target and detects recursive aliases.
    *
    * Program examples:
    *
    * {{{
    * type Count = i32
    * val total: Count = 0
    * }}}
    *
    * Alias resolution preserves the alias wrapper for diagnostics elsewhere,
    * while this helper computes the target used by equality and assignment
    * checks.
    */
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

  /** Resolves source type syntax into `SourceType`.
    *
    * Program examples:
    *
    * {{{
    * val bytes: Vec[u8] = Vec[u8]()
    * def update(value: &mut String): Unit = {}
    * type Names = Map[String, i32]
    * }}}
    *
    * This is the type-level counterpart of expression inference: it resolves
    * builtins, `Self`, user classes, aliases, standard generics, references,
    * and imported foreign symbols, then validates arity and descriptor
    * constraints.
    */
  private def resolveType(
      node: UntypedType,
      owner: Option[String],
  ): SourceType =
    resolveType(node, owner, Set.empty)

  /** Worker for `resolveType` that tracks alias recursion and in-scope generic
    * type parameters.
    *
    * Program examples:
    *
    * {{{
    * type Pair[T] = Vec[T]
    * class Box { type Item = i32 }
    * }}}
    *
    * `seenAliases` prevents infinite expansion, while `tyParams` lets alias
    * bodies resolve generic parameters before falling back to named types.
    */
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

  /** Substitutes generic alias parameters after type-argument checking.
    *
    * Program examples:
    *
    * {{{
    * type Boxed[T] = Option[T]
    * val value: Boxed[i32] = Option[i32]::Some(1)
    * }}}
    *
    * The alias body may contain references, functions, standard generics, and
    * foreign applied types; substitution preserves that structure.
    */
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

  /** Applies descriptor-specific constraints after standard type inference.
    *
    * Program examples:
    *
    * {{{
    * Map[String, i32]
    * Set[Id[User]]
    * }}}
    *
    * The core checker accepts the type constructor shape first, then verifies
    * the extra key-type premise required by Map and Set.
    */
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

  /** Checks the Map/Set key-type rule.
    *
    * Program examples:
    *
    * {{{
    * Map[String, i32]  // accepted
    * Map[Vec[i32], i32] // rejected
    * }}}
    */
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

  /** Decides whether a type is a supported Map or Set key.
    *
    * Program examples:
    *
    * {{{
    * String
    * u64
    * Id[User]
    * }}}
    */
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

  /** Applies argument-passing compatibility after argument inference.
    *
    * Program examples:
    *
    * {{{
    * def read(value: &String): Unit = {}
    * def write(value: &mut String): Unit = {}
    * var text = "hello"
    * read(text)   // allowed
    * write(text)  // allowed because `text` is mutation-capable
    * }}}
    *
    * Non-reference parameters use assignment compatibility. Reference
    * parameters additionally check that mutable references are supplied only by
    * mutable references or mutation-capable l-values.
    */
  private def canPass(actual: ExprInfo, expected: SourceType): Boolean =
    SourceType.dealias(expected) match
      case SourceType.Ref(target, mutable) =>
        SourceType.dealias(actual.expr.ty) match
          case SourceType.Ref(actualTarget, actualMutable) =>
            sameType(
              actualTarget,
              target,
            ) && (!mutable || actualMutable)
          case other =>
            sameType(
              other,
              target,
            ) && (!mutable || actual.mutAllowed)
      case other =>
        assignableType(actual.expr.ty, other)

  /** Emits an assignment-style mismatch when an inferred type cannot flow to an
    * expected type.
    *
    * Program examples:
    *
    * {{{
    * val value: i32 = true
    * }}}
    *
    * This helper centralizes declaration initialization, assignment, and return
    * checks so all use the MLTT-backed assignability relation.
    */
  private def checkAssignable(
      actual: ExprInfo,
      expected: SourceType,
      span: SourceSpan,
      code: String,
  ): Unit =
    if !assignableType(actual.expr.ty, expected) then
      error(
        code,
        s"expected ${expected.display}, got ${actual.expr.ty.display}",
        span,
      )

  /** Checks receiver mutability for method calls.
    *
    * Program examples:
    *
    * {{{
    * class Buffer { def push(&mut self, value: u8): Unit = {} }
    * val frozen = Buffer()
    * frozen.push(1) // rejected
    * }}}
    *
    * Method lookup infers the receiver expression first; this check verifies
    * that a mutable receiver signature only accepts a mutation-capable
    * receiver.
    */
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

  /** Checks a condition-like expression as `Bool`.
    *
    * Program examples:
    *
    * {{{
    * while index < limit { index += 1 }
    * if ready { println("ready") }
    * }}}
    *
    * The expression is already inferred; this helper reports the standard
    * expected-bool diagnostic when the inferred type is not `Bool`.
    */
  private def requireBool(value: ExprInfo, span: SourceSpan): Unit =
    if !sameType(value.expr.ty, SourceType.Bool) then
      error(
        "cosmo0.type.expected-bool",
        s"expected Bool, got ${value.expr.ty.display}",
        span,
      )

  /** Reports a literal-pattern type mismatch.
    *
    * Program examples:
    *
    * {{{
    * value: String match {
    *   case 1 => "bad"
    * }
    * }}}
    *
    * Literal patterns have fixed or expected numeric types, so this diagnostic
    * keeps pattern checking tied to the scrutinee type.
    */
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

  /** Creates the scope symbol used by later name-expression inference.
    *
    * Program examples:
    *
    * {{{
    * var count: i32 = 0
    * count
    * }}}
    *
    * The `var`/`val` kind determines whether assignment inference can later use
    * the name as a mutable binding.
    */
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

  /** Computes whether a value of a type can be used as a mutable l-value or
    * mutable receiver.
    *
    * Program examples:
    *
    * {{{
    * var value: i32 = 1
    * val ref: &mut i32 = &value
    * }}}
    *
    * Mutable references carry their own capability; foreign namespaces/symbols,
    * `Never`, and `Error` are never mutable values.
    */
  private def mutationCapability(ty: SourceType): Boolean =
    SourceType.dealias(ty) match
      case SourceType.Ref(_, mutable) => mutable
      case SourceType.ForeignNamespace(_) | SourceType.ForeignSymbol(_) =>
        false
      case SourceType.Never | SourceType.Error => false
      case _                                   => true

  /** Checks an ASCII literal and returns its integer code point.
    *
    * Program examples:
    *
    * {{{
    * val open: u8 = a"("
    * }}}
    *
    * ASCII literals type as `u8` and must contain exactly one ASCII code point.
    */
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

  /** Checks a rune literal and returns its Unicode scalar value.
    *
    * Program examples:
    *
    * {{{
    * val letter: u32 = c"A"
    * }}}
    *
    * Rune literals type as `u32` and must contain exactly one valid Unicode
    * scalar value.
    */
  private def runeLiteralValue(value: String, span: SourceSpan): BigInt =
    singleCodePoint(value, "rune", span).fold(BigInt(0))(BigInt(_))

  /** Extracts the single code point required by rune and ASCII literals.
    *
    * Program examples:
    *
    * {{{
    * c"A"  // one code point
    * a"A"  // one ASCII code point
    * }}}
    */
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
