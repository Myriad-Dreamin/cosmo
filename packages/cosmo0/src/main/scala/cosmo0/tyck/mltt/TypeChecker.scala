package cosmo0

import scala.collection.mutable.ListBuffer

/** Core Martin-Lof type theory term language used by the experimental MLTT
  * checker mirror.
  *
  * Term examples:
  *
  * {{{
  * Type0                         // Universe(0)
  * (x: A) -> A                   // Pi("x", A, A)
  * fun x: A => x                 // Lambda("x", A, Var("x"))
  * f(a)                          // Apply(f, a)
  * Sigma(x: A). B                // Sigma("x", A, B)
  * (a, b), fst(p), snd(p)        // Pair and projections
  * Eq(A, x, y), Refl(x)          // identity type and reflexivity witness
  * Nat, Z, S(n)                  // builtin Nat fixture
  * Vec(A, n), Nil, Cons(k,h,t)   // builtin Vec fixture
  * }}}
  *
  * Terms are stored by integer ids in `MlttTermStore`. The checker expects an
  * already elaborated core term; ordinary cosmo0 source is not translated into
  * this representation by the default compiler pipeline.
  */
enum MlttTerm:
  case Var(name: String)
  case Universe(level: Int)
  case Pi(name: String, domain: Int, body: Int)
  case Sigma(name: String, first: Int, second: Int)
  case Lambda(name: String, annotation: Int, body: Int)
  case Apply(function: Int, argument: Int)
  case Pair(first: Int, second: Int)
  case Fst(pair: Int)
  case Snd(pair: Int)
  case Let(name: String, value: Int, body: Int)
  case Inductive(name: String, params: List[Int], indices: List[Int])
  case Constructor(name: String, args: List[Int])
  case Eq(valueType: Int, left: Int, right: Int)
  case Refl(value: Int)
  case Neutral(head: String, spine: List[Int])
  case Meta(index: Int)
  case Error

final class MlttTermStore:
  private val terms = ListBuffer.empty[MlttTerm]

  def alloc(value: MlttTerm): Int =
    val id = terms.length
    terms += value
    id

  def termValue(id: Int): MlttTerm =
    terms(id)

  def allocVar(name: String): Int =
    alloc(MlttTerm.Var(name))

  def allocUniverse(level: Int): Int =
    alloc(MlttTerm.Universe(level))

  def allocPi(name: String, domain: Int, body: Int): Int =
    alloc(MlttTerm.Pi(name, domain, body))

  def allocSigma(name: String, first: Int, second: Int): Int =
    alloc(MlttTerm.Sigma(name, first, second))

  def allocLambda(name: String, annotation: Int, body: Int): Int =
    alloc(MlttTerm.Lambda(name, annotation, body))

  def allocApply(function: Int, argument: Int): Int =
    alloc(MlttTerm.Apply(function, argument))

  def allocPair(first: Int, second: Int): Int =
    alloc(MlttTerm.Pair(first, second))

  def allocFst(pair: Int): Int =
    alloc(MlttTerm.Fst(pair))

  def allocSnd(pair: Int): Int =
    alloc(MlttTerm.Snd(pair))

  def allocLet(name: String, value: Int, body: Int): Int =
    alloc(MlttTerm.Let(name, value, body))

  def allocInductive(
      name: String,
      params: List[Int] = Nil,
      indices: List[Int] = Nil,
  ): Int =
    alloc(MlttTerm.Inductive(name, params, indices))

  def allocConstructor(name: String, args: List[Int] = Nil): Int =
    alloc(MlttTerm.Constructor(name, args))

  def allocEq(valueType: Int, left: Int, right: Int): Int =
    alloc(MlttTerm.Eq(valueType, left, right))

  def allocRefl(value: Int): Int =
    alloc(MlttTerm.Refl(value))

  def allocNeutral(head: String, spine: List[Int] = Nil): Int =
    alloc(MlttTerm.Neutral(head, spine))

  def allocMeta(index: Int): Int =
    alloc(MlttTerm.Meta(index))

  def allocError(): Int =
    alloc(MlttTerm.Error)

  def size: Int =
    terms.length

final case class MlttContextEntry(
    name: String,
    valueType: Int,
    value: Option[Int] = None,
    transparent: Boolean = false,
    pure: Boolean = true,
    universeLevel: Int = 0,
)

final case class MlttContext(entries: List[MlttContextEntry] = Nil):
  def lookup(name: String): Option[MlttContextEntry] =
    entries.reverse.find(_.name == name)

  def extendLocal(name: String, valueType: Int): MlttContext =
    copy(entries = entries :+ MlttContextEntry(name, valueType))

  def extendDefinition(
      name: String,
      valueType: Int,
      value: Int,
      transparent: Boolean,
      pure: Boolean,
  ): MlttContext =
    copy(entries =
      entries :+ MlttContextEntry(
        name,
        valueType,
        Some(value),
        transparent,
        pure,
      ),
    )

  def summary: String =
    if entries.isEmpty then "<empty>" else entries.map(_.name).mkString(", ")

final case class MlttBinder(name: String, valueType: Int)

final case class MlttDefinitionDecl(
    name: String,
    valueType: Int,
    value: Int,
    transparent: Boolean,
    pure: Boolean,
)

final case class MlttConstructorDecl(
    name: String,
    telescope: List[MlttBinder],
    result: Int,
)

final case class MlttInductiveDecl(
    name: String,
    parameters: List[MlttBinder],
    indices: List[MlttBinder],
    constructors: List[MlttConstructorDecl],
)

final class MlttDeclarationEnv:
  private val definitions = ListBuffer.empty[MlttDefinitionDecl]
  private val inductives = ListBuffer.empty[MlttInductiveDecl]

  def addDefinition(decl: MlttDefinitionDecl): Unit =
    definitions += decl

  def addInductive(decl: MlttInductiveDecl): Unit =
    inductives += decl

  def findDefinition(name: String): Option[MlttDefinitionDecl] =
    definitions.find(_.name == name)

  def findInductive(name: String): Option[MlttInductiveDecl] =
    inductives.find(_.name == name)

  def findConstructor(name: String): Option[MlttConstructorDecl] =
    inductives.iterator
      .flatMap(_.constructors.iterator)
      .find(_.name == name)

final case class MlttMetaVar(
    id: Int,
    contextSummary: String,
    expectedType: Int,
    solution: Option[Int] = None,
)

final class MlttMetaStore:
  private val metas = ListBuffer.empty[MlttMetaVar]

  def newMeta(context: MlttContext, expectedType: Int): Int =
    val id = metas.length
    metas += MlttMetaVar(id, context.summary, expectedType)
    id

  def solveExact(id: Int, value: Int): Boolean =
    if id < 0 || id >= metas.length then return false
    val current = metas(id)
    metas.update(id, current.copy(solution = Some(value)))
    true

  def hasUnsolvedPublicMeta: Boolean =
    metas.exists(_.solution.isEmpty)

  def all: List[MlttMetaVar] =
    metas.toList

final case class MlttDiagnostic(
    code: String,
    message: String,
    span: Option[SourceSpan],
    profileId: String,
    contextSummary: String,
    expected: String,
    actual: String,
)

final case class MlttConversionResult(
    ok: Boolean,
    strategy: String,
    diagnostics: List[MlttDiagnostic],
):
  def isOk: Boolean =
    ok && diagnostics.isEmpty

  def firstCode: String =
    diagnostics.headOption.map(_.code).getOrElse("")

final case class MlttInferResult(
    profileId: String,
    artifactKind: String,
    term: Int,
    valueType: Int,
    diagnostics: List[MlttDiagnostic],
    normalizationStrategy: String,
):
  def isOk: Boolean =
    diagnostics.isEmpty

  def firstCode: String =
    diagnostics.headOption.map(_.code).getOrElse("")

final case class MlttCheckResult(
    profileId: String,
    artifactKind: String,
    term: Int,
    valueType: Int,
    diagnostics: List[MlttDiagnostic],
    metas: MlttMetaStore,
    status: String,
    normalizationStrategy: String,
):
  def isOk: Boolean =
    diagnostics.isEmpty

  def firstCode: String =
    diagnostics.headOption.map(_.code).getOrElse("")

  def artifactSummary: String =
    s"$profileId|$artifactKind|$status|$normalizationStrategy"

/** Bidirectional checker for the experimental `mltt.core` profile.
  *
  * Program examples:
  *
  * {{{
  * A : Type0
  * y : A
  * (fun x: A => x)(y)
  * (x, y)
  * fst((x, y))
  * Refl(x)
  * Cons(k, head, tail)
  * }}}
  *
  * Direct harness example:
  *
  * {{{
  * val store = MlttTypeChecker.termStore()
  * val type0 = store.allocUniverse(0)
  * val a = store.allocVar("A")
  * val id = store.allocLambda("x", a, store.allocVar("x"))
  * val idType = store.allocPi("x", a, a)
  * val context = MlttTypeChecker.context().extendLocal("A", type0)
  *
  * MlttTypeChecker.check(store, context, id, idType)
  * }}}
  *
  * Rules:
  *
  * {{{
  * Gamma |- Type n => Type (n + 1)
  * Gamma, x : A |- x => A
  * Gamma |- A => Type i; Gamma, x : A |- B => Type j
  *   --------------------------------------------------- Pi
  *   Gamma |- (x: A) -> B => Type(max(i, j))
  * Gamma |- A => Type i; Gamma, x : A |- B => Type j
  *   --------------------------------------------------- Sigma
  *   Gamma |- Sigma(x: A). B => Type(max(i, j))
  * Gamma |- A => Type i; Gamma, x : A |- body => B
  *   --------------------------------------------------- LambdaInfer
  *   Gamma |- fun x: A => body => (x: A) -> B
  * Gamma, x : A |- body <= B
  *   --------------------------------------------------- LambdaCheck
  *   Gamma |- fun x: A => body <= (x: A) -> B
  * Gamma |- f => (x: A) -> B; Gamma |- arg <= A
  *   --------------------------------------------------- Apply
  *   Gamma |- f(arg) => B[arg / x]
  * Gamma |- fst(p) => A
  *   when Gamma |- p => Sigma(x: A). B
  * Gamma |- snd(p) => B[fst(p) / x]
  *   when Gamma |- p => Sigma(x: A). B
  * Gamma |- first <= A; Gamma |- second <= B[first / x]
  *   --------------------------------------------------- PairCheck
  *   Gamma |- (first, second) <= Sigma(x: A). B
  * Gamma |- A => Type i; Gamma |- left <= A; Gamma |- right <= A
  *   --------------------------------------------------- Eq
  *   Gamma |- Eq(A, left, right) => Type i
  * Gamma |- value == left; Gamma |- value == right
  *   --------------------------------------------------- ReflCheck
  *   Gamma |- Refl(value) <= Eq(A, left, right)
  * Gamma |- Z <= Nat
  * Gamma |- n <= Nat
  *   --------------------------------------------------- Succ
  *   Gamma |- S(n) <= Nat
  * Gamma |- Nil <= Vec(A, Z)
  * Gamma |- k <= Nat; Gamma |- head <= A; Gamma |- tail <= Vec(A, k)
  *   --------------------------------------------------- Cons
  *   Gamma |- Cons(k, head, tail) <= Vec(A, S(k))
  * Gamma |- left --> lwhnf; Gamma |- right --> rwhnf; lwhnf ~= rwhnf
  *   --------------------------------------------------- Conversion
  *   Gamma |- left == right
  * }}}
  *
  * Explanation:
  *
  * The checker implements these rules over an already elaborated MLTT core term
  * store. `infer` covers synthesis rules, `check` covers introduction forms
  * that need an expected type, and `convert` supplies definitional equality
  * through WHNF beta, let, transparent pure definitions, and Sigma projections.
  * Opaque or effectful definitions are intentionally not reduced.
  *
  * Pipeline note: direct tests can call this object, and `mltt.core` profile
  * source fixtures reach it through `checkSource` and `checkSources`.
  */
object MlttTypeChecker:
  val WhnfConversionStrategy = "mltt.whnf-conversion"
  val UnknownVariableCode = "cosmo.type.mltt.unknown-variable"
  val NonFunctionApplicationCode = "cosmo.type.mltt.non-function-application"
  val UniverseMismatchCode = "cosmo.type.mltt.universe-mismatch"
  val TypeMismatchCode = "cosmo.type.mltt.type-mismatch"
  val UnsolvedConstraintCode = "cosmo.type.mltt.unsolved-constraint"
  val UnsupportedNormalizationProfileCode =
    "cosmo.type.mltt.unsupported-normalization-profile"
  val EffectfulConversionCode = "cosmo.type.mltt.effectful-conversion"
  val UnsupportedInferenceCode = "cosmo.type.mltt.unsupported-inference"
  val HigherOrderUnificationCode = "cosmo.type.mltt.higher-order-unification"
  val NoSourceAssertionsCode = "cosmo.type.mltt.no-profile-assertions"
  val UnknownSourceAssertionCode =
    "cosmo.type.mltt.unknown-profile-assertion"
  val SourceAssertionFailedCode =
    "cosmo.type.mltt.profile-assertion-failed"

  private val SourceDirectivePrefix = "mltt:"

  def termStore(): MlttTermStore =
    MlttTermStore()

  def context(): MlttContext =
    MlttContext()

  def declarationEnv(): MlttDeclarationEnv =
    MlttDeclarationEnv()

  def metaStore(): MlttMetaStore =
    MlttMetaStore()

  def supportsProfile(profile: CheckerProfile): Boolean =
    profile.id == CheckerProfiles.MlttCore.id ||
      profile.id == CheckerProfiles.MlttDependentPatterns.id

  /** Checks a cosmo0 source file selected by an MLTT-backed checker profile.
    *
    * Source examples:
    *
    * {{{
    * mltt: lambda-checks-pi
    * mltt: application-infers-through-pi
    * mltt: vec-constructors-check
    * }}}
    *
    * The current cosmo0 frontend still does not elaborate arbitrary source
    * declarations into MLTT core terms. Instead, this entry point accepts
    * profile fixture directives that name concrete MLTT obligations, constructs
    * those core terms, and checks them through this object. The
    * `mltt.dependent-patterns` profile is handled here as an MLTT extension and
    * invokes `DependentPatterns` for case-tree elaboration obligations.
    */
  def checkSource(source: SourceFile): Result[TypedModule] =
    checkSource(source, CheckerProfiles.MlttCore)

  def checkSource(
      source: SourceFile,
      profile: CheckerProfile,
  ): Result[TypedModule] =
    checkSources(List(source), source.name, profile)

  /** Checks package-level MLTT source fixtures.
    *
    * Each directive maps to a concrete MLTT core term or conversion problem, so
    * package checking exercises the same bidirectional rules as direct calls to
    * `infer`, `check`, and `convert`. The dependent-pattern profile extends
    * this path by also running dependent-pattern assertion directives.
    */
  def checkSources(
      sources: List[SourceFile],
      moduleName: String,
  ): Result[TypedModule] =
    checkSources(sources, moduleName, CheckerProfiles.MlttCore)

  def checkSources(
      sources: List[SourceFile],
      moduleName: String,
      profile: CheckerProfile,
  ): Result[TypedModule] =
    if profile.id == CheckerProfiles.MlttDependentPatterns.id then
      return checkDependentPatternSources(sources, moduleName)

    if profile.id != CheckerProfiles.MlttCore.id then
      return Result.unsupported(
        Phase.Check,
        CheckerProfiles.unsupportedDiagnostic(
          profile,
          CheckerProfiles.firstUnsupportedFeatureForUnavailableProfile(profile),
          Some(wholeSourceSpan(sources, "<mltt-core>")),
        ),
      )

    checkCoreSources(sources, moduleName)

  private def checkCoreSources(
      sources: List[SourceFile],
      moduleName: String,
  ): Result[TypedModule] =
    val directives =
      ProfileDirectiveParser.extract(sources, SourceDirectivePrefix)
    if directives.isEmpty then
      return Result.failure(
        Phase.Check,
        List(
          sourceDiagnostic(
            NoSourceAssertionsCode,
            "mltt.core source did not declare any MLTT assertions",
            wholeSourceSpan(sources),
          ),
        ),
      )

    val diagnostics = directives.flatMap(runSourceAssertion)
    if diagnostics.nonEmpty then return Result.failure(Phase.Check, diagnostics)

    Result.success(
      Phase.Check,
      syntheticModule(moduleName, directives, "mltt_core"),
    )

  private def checkDependentPatternSources(
      sources: List[SourceFile],
      moduleName: String,
  ): Result[TypedModule] =
    val hasCoreAssertions = hasCoreSourceAssertions(sources)
    val hasDependentAssertions = DependentPatterns.hasSourceAssertions(sources)

    if !hasCoreAssertions && !hasDependentAssertions then
      return Result.failure(
        Phase.Check,
        List(
          sourceDiagnostic(
            NoSourceAssertionsCode,
            "mltt.dependent-patterns source did not declare any MLTT or dependent-pattern assertions",
            wholeSourceSpan(sources, "<mltt-dependent-patterns>"),
          ),
        ),
      )

    val modules = ListBuffer.empty[TypedModule]

    if hasCoreAssertions then
      checkCoreSources(sources, moduleName) match
        case checked if checked.isSuccess =>
          modules += checked.value.get
        case checked if checked.isUnsupported =>
          return Result(
            Phase.Check,
            PhaseStatus.Unsupported,
            None,
            checked.diagnostics,
          )
        case checked =>
          return Result.failure(Phase.Check, checked.diagnostics)

    if hasDependentAssertions then
      DependentPatterns.checkSourceAssertions(sources, moduleName) match
        case checked if checked.isSuccess =>
          modules += checked.value.get
        case checked if checked.isUnsupported =>
          return Result(
            Phase.Check,
            PhaseStatus.Unsupported,
            None,
            checked.diagnostics,
          )
        case checked =>
          return Result.failure(Phase.Check, checked.diagnostics)

    Result.success(
      Phase.Check,
      combinedSyntheticModule(moduleName, modules.toList),
    )

  private def hasCoreSourceAssertions(sources: List[SourceFile]): Boolean =
    ProfileDirectiveParser.extract(sources, SourceDirectivePrefix).nonEmpty

  /** Dispatches a source directive to the corresponding MLTT rule.
    *
    * Profile source examples:
    *
    * {{{
    * mltt: conversion-beta-let
    * }}}
    *
    * The directive above checks definitional equality for beta and let
    * reductions through `convert`.
    */
  private def runSourceAssertion(
      directive: ProfileDirective,
  ): List[Diagnostic] =
    directive.name match
      case "lambda-checks-pi" =>
        lambdaChecksPiAssertion(directive.span)
      case "application-infers-through-pi" =>
        applicationInfersThroughPiAssertion(directive.span)
      case "sigma-pair-checks" =>
        sigmaPairChecksAssertion(directive.span)
      case "equality-refl-checks" =>
        equalityReflChecksAssertion(directive.span)
      case "conversion-beta-let" =>
        conversionBetaLetAssertion(directive.span)
      case "vec-constructors-check" =>
        vecConstructorsCheckAssertion(directive.span)
      case other =>
        List(
          sourceDiagnostic(
            UnknownSourceAssertionCode,
            s"unknown mltt.core assertion $other",
            directive.span,
          ),
        )

  /** Checks the Pi introduction rule for annotated lambdas.
    *
    * Rules:
    *
    * {{{
    * A : Type0
    * fun x: A => x <= (x: A) -> A
    * }}}
    */
  private def lambdaChecksPiAssertion(span: SourceSpan): List[Diagnostic] =
    val store = termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val body = store.allocVar("x")
    val lambda = store.allocLambda("x", a, body)
    val expected = store.allocPi("x", a, a)
    val contextValue = context().extendLocal("A", type0)
    val checked = check(store, contextValue, lambda, expected)
    val expectedSummary =
      "mltt.core|mltt-core-term|accepted|mltt.whnf-conversion"

    sourceDiagnostics(checked.diagnostics, span) :::
      assertSourceCondition(
        checked.isOk && checked.artifactSummary == expectedSummary,
        "lambda did not check against the expected Pi type",
        expectedSummary,
        checked.artifactSummary,
        span,
      )

  /** Checks the Pi elimination rule used by application inference.
    *
    * Rules:
    *
    * {{{
    * A : Type0
    * y : A
    * (fun x: A => x)(y)  // infers A
    * }}}
    */
  private def applicationInfersThroughPiAssertion(
      span: SourceSpan,
  ): List[Diagnostic] =
    val store = termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    var contextValue = context().extendLocal("A", type0)
    contextValue = contextValue.extendLocal("y", a)

    val body = store.allocVar("x")
    val lambda = store.allocLambda("x", a, body)
    val y = store.allocVar("y")
    val apply = store.allocApply(lambda, y)
    val inferred = infer(store, contextValue, apply)
    val actual = display(store, inferred.valueType)

    sourceDiagnostics(inferred.diagnostics, span) :::
      assertSourceCondition(
        inferred.isOk && actual == "A",
        "application did not infer through Pi elimination",
        "A",
        actual,
        span,
      )

  /** Checks the Sigma introduction rule for pairs.
    *
    * Rules:
    *
    * {{{
    * x : A
    * y : A
    * (x, y) <= Sigma(first: A). A
    * }}}
    */
  private def sigmaPairChecksAssertion(span: SourceSpan): List[Diagnostic] =
    val store = termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val x = store.allocVar("x")
    val y = store.allocVar("y")
    var contextValue = context().extendLocal("A", type0)
    contextValue = contextValue.extendLocal("x", a).extendLocal("y", a)

    val pair = store.allocPair(x, y)
    val sigma = store.allocSigma("first", a, a)
    val checked = check(store, contextValue, pair, sigma)

    sourceDiagnostics(checked.diagnostics, span) :::
      assertSourceCondition(
        checked.isOk,
        "pair did not check against the expected Sigma type",
        "accepted",
        checked.status,
        span,
      )

  /** Checks equality introduction for reflexivity.
    *
    * Rules:
    *
    * {{{
    * x : A
    * Refl(x) <= Eq(A, x, x)
    * }}}
    */
  private def equalityReflChecksAssertion(span: SourceSpan): List[Diagnostic] =
    val store = termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val x = store.allocVar("x")
    val contextValue = context()
      .extendLocal("A", type0)
      .extendLocal("x", a)

    val eq = store.allocEq(a, x, x)
    val refl = store.allocRefl(x)
    val checked = check(store, contextValue, refl, eq)

    sourceDiagnostics(checked.diagnostics, span) :::
      assertSourceCondition(
        checked.isOk,
        "Refl did not check against the expected equality type",
        "accepted",
        checked.status,
        span,
      )

  /** Checks WHNF conversion for beta and let reduction.
    *
    * Rules:
    *
    * {{{
    * (fun x: A => x)(y) == y
    * let z = y; z == y
    * }}}
    */
  private def conversionBetaLetAssertion(span: SourceSpan): List[Diagnostic] =
    val store = termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val y = store.allocVar("y")
    val contextValue = context()
      .extendLocal("A", type0)
      .extendLocal("y", a)

    val lambda = store.allocLambda("x", a, store.allocVar("x"))
    val beta = store.allocApply(lambda, y)
    val letTerm = store.allocLet("z", y, store.allocVar("z"))
    val betaResult = convert(store, contextValue, beta, y)
    val letResult = convert(store, contextValue, letTerm, y)

    sourceDiagnostics(betaResult.diagnostics, span) :::
      sourceDiagnostics(letResult.diagnostics, span) :::
      assertSourceCondition(
        betaResult.isOk && letResult.isOk,
        "WHNF conversion did not accept beta and let reductions",
        "both conversions accepted",
        s"beta=${betaResult.isOk}, let=${letResult.isOk}",
        span,
      )

  /** Checks indexed constructor introduction for the `Vec` fixture.
    *
    * Rules:
    *
    * {{{
    * Nil <= Vec(A, Z)
    * Cons(k, head, tail) <= Vec(A, S(k))
    * }}}
    */
  private def vecConstructorsCheckAssertion(
      span: SourceSpan,
  ): List[Diagnostic] =
    val store = termStore()
    val type0 = store.allocUniverse(0)
    val nat = store.allocInductive("Nat")
    val a = store.allocVar("A")
    val k = store.allocVar("k")
    val z = store.allocConstructor("Z")
    val sK = store.allocConstructor("S", List(k))
    val vecAK = store.allocInductive("Vec", List(a), List(k))
    val vecAZ = store.allocInductive("Vec", List(a), List(z))
    val vecASK = store.allocInductive("Vec", List(a), List(sK))
    val head = store.allocVar("head")
    val tail = store.allocVar("tail")
    val nil = store.allocConstructor("Nil")
    val cons = store.allocConstructor("Cons", List(k, head, tail))
    var contextValue = context().extendLocal("A", type0)
    contextValue = contextValue
      .extendLocal("k", nat)
      .extendLocal("head", a)
      .extendLocal("tail", vecAK)

    val nilChecked = check(store, contextValue, nil, vecAZ)
    val consChecked = check(store, contextValue, cons, vecASK)

    sourceDiagnostics(nilChecked.diagnostics, span) :::
      sourceDiagnostics(consChecked.diagnostics, span) :::
      assertSourceCondition(
        nilChecked.isOk && consChecked.isOk,
        "Vec constructors did not check against indexed Vec families",
        "Nil and Cons accepted",
        s"Nil=${nilChecked.isOk}, Cons=${consChecked.isOk}",
        span,
      )

  private def sourceDiagnostics(
      diagnostics: List[MlttDiagnostic],
      span: SourceSpan,
  ): List[Diagnostic] =
    diagnostics.map { diagnosticValue =>
      sourceDiagnostic(
        diagnosticValue.code,
        s"${diagnosticValue.message}; expected ${diagnosticValue.expected}, got ${diagnosticValue.actual}",
        diagnosticValue.span.getOrElse(span),
      )
    }

  private def assertSourceCondition(
      condition: Boolean,
      message: String,
      expected: String,
      actual: String,
      span: SourceSpan,
  ): List[Diagnostic] =
    if condition then Nil
    else
      List(
        sourceDiagnostic(
          SourceAssertionFailedCode,
          s"$message; expected $expected, got $actual",
          span,
        ),
      )

  private def syntheticModule(
      moduleName: String,
      directives: List[ProfileDirective],
      prefix: String,
  ): TypedModule =
    val source = SourceFile(moduleName, "")
    val span = source.span(0, 0)
    val declarations = directives.map { directive =>
      val name = s"${prefix}_${sanitizeName(directive.name)}"
      TypedValueDecl(
        UntypedValueKind.Val,
        name,
        SourceType.Bool,
        Some(TypedBoolLiteral(true, SourceType.Bool, directive.span)),
        directive.span,
      )
    }
    TypedModule(source, declarations, span)

  private def combinedSyntheticModule(
      moduleName: String,
      modules: List[TypedModule],
  ): TypedModule =
    val source = SourceFile(moduleName, "")
    val span = source.span(0, 0)
    TypedModule(source, modules.flatMap(_.decls), span)

  private def sanitizeName(value: String): String =
    value.map {
      case char if char.isLetterOrDigit => char
      case _                            => '_'
    }

  private def wholeSourceSpan(
      sources: List[SourceFile],
      emptyName: String = "<mltt-core>",
  ): SourceSpan =
    sources.headOption match
      case Some(source) => source.span(0, source.text.length)
      case None         => SourceFile(emptyName, "").span(0, 0)

  private def sourceDiagnostic(
      code: String,
      message: String,
      span: SourceSpan,
  ): Diagnostic =
    Diagnostic(Phase.Check, DiagnosticSeverity.Error, code, message, Some(span))

  /** Infers the type of an MLTT core term.
    *
    * Rules:
    *
    * {{{
    * Gamma |- term => T
    * }}}
    *
    * Program examples:
    *
    * {{{
    * (fun x: A => x)(y)
    * }}}
    *
    * Explanation:
    *
    * This entry point is synthesis-only: terms such as pairs and `Refl` that
    * need an expected type report unsupported inference diagnostics.
    */
  def infer(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
  ): MlttInferResult =
    val diagnostics = ListBuffer.empty[MlttDiagnostic]
    val valueType = inferTerm(store, context, term, diagnostics)
    MlttInferResult(
      CheckerProfiles.MlttCore.id,
      CheckerProfiles.MlttCore.artifactKind,
      term,
      valueType,
      diagnostics.toList,
      WhnfConversionStrategy,
    )

  /** Checks an MLTT core term against an expected type.
    *
    * Rules:
    *
    * {{{
    * Gamma |- term <= Expected
    * }}}
    *
    * Program examples:
    *
    * {{{
    * fun x: A => x
    * (x, y)
    * Refl(x)
    * }}}
    *
    * Explanation:
    *
    * Checking handles introduction forms directly, then falls back to inference
    * plus conversion when the term is not an introduction form for the expected
    * type.
    */
  def check(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      expected: Int,
  ): MlttCheckResult =
    val diagnostics = ListBuffer.empty[MlttDiagnostic]
    val metas = metaStore()
    val valueType = checkTerm(store, context, term, expected, diagnostics)
    reportUnsolvedMetas(context, metas, diagnostics)
    MlttCheckResult(
      CheckerProfiles.MlttCore.id,
      CheckerProfiles.MlttCore.artifactKind,
      term,
      valueType,
      diagnostics.toList,
      metas,
      if diagnostics.isEmpty then "accepted" else "rejected",
      WhnfConversionStrategy,
    )

  /** Checks definitional equality under the selected normalization strategy.
    *
    * Rules:
    *
    * {{{
    * Gamma |- left --> lwhnf
    * Gamma |- right --> rwhnf
    * lwhnf ~= rwhnf
    *   --------------------------------------------------- Convert
    * Gamma |- left == right
    * }}}
    *
    * Program examples:
    *
    * {{{
    * (fun x: A => x)(y)
    * let z = y; z
    * fst((x, y))
    * }}}
    *
    * Explanation:
    *
    * The current strategy is WHNF conversion. It reduces beta, let, transparent
    * pure definitions, and Sigma projections, then compares normalized heads
    * structurally.
    */
  def convert(
      store: MlttTermStore,
      context: MlttContext,
      left: Int,
      right: Int,
      strategy: String = WhnfConversionStrategy,
  ): MlttConversionResult =
    val diagnostics = ListBuffer.empty[MlttDiagnostic]
    if strategy != WhnfConversionStrategy then
      diagnostics += diagnostic(
        UnsupportedNormalizationProfileCode,
        s"checker profile mltt.core does not implement normalization profile $strategy",
        context,
        WhnfConversionStrategy,
        strategy,
      )
      return MlttConversionResult(ok = false, strategy, diagnostics.toList)

    val ok = convertTerms(store, context, left, right, diagnostics)
    if !ok && diagnostics.isEmpty then
      diagnostics += diagnostic(
        TypeMismatchCode,
        "types are not definitionally equal after allowed normalization",
        context,
        display(store, left),
        display(store, right),
      )
    MlttConversionResult(ok, strategy, diagnostics.toList)

  /** Produces the diagnostic for constraints outside the MLTT profile.
    *
    * Rules:
    *
    * {{{
    * ?f(x) = y
    * }}}
    *
    * The profile only accepts the first-order pattern constraints represented
    * by explicit metas in this file. Arbitrary higher-order unification is
    * reported as a rejected rule rather than approximated silently.
    */
  def higherOrderUnificationDiagnostic(context: MlttContext): MlttDiagnostic =
    diagnostic(
      HigherOrderUnificationCode,
      "arbitrary higher-order unification is outside mltt.core",
      context,
      "first-order pattern constraint",
      "higher-order constraint",
    )

  /** Installs the Nat fixture used by MLTT inference and conversion examples.
    *
    * Rules:
    *
    * {{{
    * Nat : Type0
    * Z : Nat
    * S(pred: Nat) : Nat
    * }}}
    */
  def addNatFixture(store: MlttTermStore, env: MlttDeclarationEnv): Unit =
    val type0 = store.allocUniverse(0)
    val nat = store.allocInductive("Nat")
    val z = MlttConstructorDecl("Z", Nil, nat)
    val s = MlttConstructorDecl("S", List(MlttBinder("pred", nat)), nat)
    env.addInductive(MlttInductiveDecl("Nat", Nil, Nil, List(z, s)))
    env.addDefinition(
      MlttDefinitionDecl("Nat", type0, nat, transparent = true, pure = true),
    )

  /** Installs the indexed Vec fixture used by constructor checking examples.
    *
    * Rules:
    *
    * {{{
    * Vec(A: Type0, n: Nat) : Type0
    * Nil : Vec(A, Z)
    * Cons(k: Nat, head: A, tail: Vec(A, k)) : Vec(A, S(k))
    * }}}
    */
  def addVecFixture(store: MlttTermStore, env: MlttDeclarationEnv): Unit =
    val type0 = store.allocUniverse(0)
    val nat = store.allocInductive("Nat")
    val a = store.allocVar("A")
    val n = store.allocVar("n")
    val k = store.allocVar("k")
    val z = store.allocConstructor("Z")
    val sK = store.allocConstructor("S", List(k))
    val nilResult = store.allocInductive("Vec", List(a), List(z))
    val tailType = store.allocInductive("Vec", List(a), List(k))
    val consResult = store.allocInductive("Vec", List(a), List(sK))
    val nil = MlttConstructorDecl("Nil", Nil, nilResult)
    val cons =
      MlttConstructorDecl(
        "Cons",
        List(
          MlttBinder("k", nat),
          MlttBinder("head", a),
          MlttBinder("tail", tailType),
        ),
        consResult,
      )
    env.addInductive(
      MlttInductiveDecl(
        "Vec",
        List(MlttBinder("A", type0)),
        List(MlttBinder("n", nat)),
        List(nil, cons),
      ),
    )
    env.addDefinition(
      MlttDefinitionDecl(
        "Vec",
        type0,
        store.allocInductive("Vec", List(a), List(n)),
        transparent = true,
        pure = true,
      ),
    )

  /** Renders core MLTT terms in the same notation used by diagnostics.
    *
    * Rules:
    *
    * {{{
    * (x: A) -> A
    * Vec(A, S(k))
    * Eq(A, x, x)
    * }}}
    *
    * Infer/check rules store terms by id; display converts those ids back into
    * stable program text for expected/actual diagnostics and profile summaries.
    */
  def display(store: MlttTermStore, id: Int): String =
    store.termValue(id) match
      case MlttTerm.Var(name)       => name
      case MlttTerm.Universe(level) => s"Type$level"
      case MlttTerm.Pi(name, domain, body) =>
        s"($name: ${display(store, domain)}) -> ${display(store, body)}"
      case MlttTerm.Sigma(name, first, second) =>
        s"Sigma($name: ${display(store, first)}). ${display(store, second)}"
      case MlttTerm.Lambda(name, annotation, body) =>
        s"fun $name: ${display(store, annotation)} => ${display(store, body)}"
      case MlttTerm.Apply(function, argument) =>
        s"${display(store, function)}(${display(store, argument)})"
      case MlttTerm.Pair(first, second) =>
        s"(${display(store, first)}, ${display(store, second)})"
      case MlttTerm.Fst(pair) =>
        s"fst(${display(store, pair)})"
      case MlttTerm.Snd(pair) =>
        s"snd(${display(store, pair)})"
      case MlttTerm.Let(name, value, body) =>
        s"let $name = ${display(store, value)}; ${display(store, body)}"
      case MlttTerm.Inductive(name, params, indices) =>
        headDisplay(store, name, params ::: indices)
      case MlttTerm.Constructor(name, args) =>
        headDisplay(store, name, args)
      case MlttTerm.Eq(valueType, left, right) =>
        s"Eq(${display(store, valueType)}, ${display(store, left)}, ${display(store, right)})"
      case MlttTerm.Refl(value) =>
        s"Refl(${display(store, value)})"
      case MlttTerm.Neutral(head, spine) =>
        headDisplay(store, head, spine)
      case MlttTerm.Meta(index) =>
        s"?m$index"
      case MlttTerm.Error =>
        "<error>"

  private def headDisplay(
      store: MlttTermStore,
      name: String,
      args: List[Int],
  ): String =
    if args.isEmpty then name
    else s"$name(${args.map(display(store, _)).mkString(", ")})"

  private def diagnostic(
      code: String,
      message: String,
      context: MlttContext,
      expected: String,
      actual: String,
      span: Option[SourceSpan] = None,
  ): MlttDiagnostic =
    MlttDiagnostic(
      code,
      message,
      span,
      CheckerProfiles.MlttCore.id,
      context.summary,
      expected,
      actual,
    )

  /** Implements MLTT synthesis rules for every core term constructor.
    *
    * Rules:
    *
    * {{{
    * Type0          => Type1
    * (x: A) -> B    => Type(max(level(A), level(B)))
    * f(a)           => B[a/x] when f : (x: A) -> B
    * }}}
    *
    * The function returns a term id for the inferred type and appends
    * diagnostics instead of throwing when a rule cannot synthesize.
    */
  private def inferTerm(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    store.termValue(term) match
      case MlttTerm.Var(name) =>
        inferVar(store, context, name, diagnostics)
      case MlttTerm.Universe(level) =>
        store.allocUniverse(level + 1)
      case MlttTerm.Pi(name, domain, body) =>
        inferPiLike(store, context, name, domain, body, diagnostics)
      case MlttTerm.Sigma(name, first, second) =>
        inferPiLike(store, context, name, first, second, diagnostics)
      case MlttTerm.Lambda(name, annotation, body) =>
        inferLambda(store, context, name, annotation, body, diagnostics)
      case MlttTerm.Apply(function, argument) =>
        inferApply(store, context, term, function, argument, diagnostics)
      case MlttTerm.Pair(_, _) =>
        diagnostics += diagnostic(
          UnsupportedInferenceCode,
          "pair inference requires an expected Sigma type",
          context,
          "Sigma",
          display(store, term),
        )
        store.allocError()
      case MlttTerm.Fst(pair) =>
        inferProjection(store, context, term, pair, diagnostics, first = true)
      case MlttTerm.Snd(pair) =>
        inferProjection(store, context, term, pair, diagnostics, first = false)
      case MlttTerm.Let(name, value, body) =>
        val valueType = inferTerm(store, context, value, diagnostics)
        inferTerm(
          store,
          context.extendDefinition(
            name,
            valueType,
            value,
            transparent = true,
            pure = true,
          ),
          body,
          diagnostics,
        )
      case MlttTerm.Inductive(_, _, _) =>
        store.allocUniverse(0)
      case MlttTerm.Constructor(name, args) =>
        inferConstructor(store, context, term, name, args, diagnostics)
      case MlttTerm.Eq(valueType, left, right) =>
        inferEq(store, context, valueType, left, right, diagnostics)
      case MlttTerm.Refl(_) =>
        diagnostics += diagnostic(
          UnsupportedInferenceCode,
          "Refl inference requires an expected equality type",
          context,
          "Eq",
          display(store, term),
        )
        store.allocError()
      case MlttTerm.Neutral(head, _) =>
        diagnostics += diagnostic(
          UnsupportedInferenceCode,
          "neutral term inference requires an elaborated type annotation",
          context,
          "annotated neutral",
          head,
        )
        store.allocError()
      case MlttTerm.Meta(_) =>
        diagnostics += diagnostic(
          UnsolvedConstraintCode,
          "metavariable cannot be inferred until it is solved",
          context,
          "solved metavariable",
          display(store, term),
        )
        store.allocError()
      case MlttTerm.Error =>
        store.allocError()

  /** Infers a variable from the local context.
    *
    * Rules:
    *
    * {{{
    * A : Type0, x : A |- x => A
    * }}}
    */
  private def inferVar(
      store: MlttTermStore,
      context: MlttContext,
      name: String,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    context.lookup(name) match
      case Some(entry) => entry.valueType
      case None =>
        diagnostics += diagnostic(
          UnknownVariableCode,
          s"unknown variable $name",
          context,
          "bound local",
          name,
        )
        store.allocError()

  /** Infers the universe level of Pi and Sigma type formers.
    *
    * Rules:
    *
    * {{{
    * A : Type0
    * B : Type0
    * (x: A) -> B : Type0
    * Sigma(x: A). B : Type0
    * }}}
    *
    * Both the domain and body must themselves be classified by universes. The
    * resulting universe is the maximum body/domain level.
    */
  private def inferPiLike(
      store: MlttTermStore,
      context: MlttContext,
      name: String,
      domain: Int,
      body: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    val domainType = inferTerm(store, context, domain, diagnostics)
    val bodyType =
      inferTerm(store, context.extendLocal(name, domain), body, diagnostics)
    val domainLevel = typeUniverseLevel(store, domainType)
    val bodyLevel = typeUniverseLevel(store, bodyType)
    (domainLevel, bodyLevel) match
      case (Some(left), Some(right)) =>
        store.allocUniverse(left.max(right))
      case (None, _) =>
        universeMismatch(store, context, domainType, diagnostics)
        store.allocError()
      case (_, None) =>
        universeMismatch(store, context, bodyType, diagnostics)
        store.allocError()

  /** Infers a Pi type for an annotated lambda.
    *
    * Rules:
    *
    * {{{
    * fun x: A => x  =>  (x: A) -> A
    * }}}
    *
    * The annotation must be universe-classified, then the body is inferred in a
    * context extended with the lambda parameter.
    */
  private def inferLambda(
      store: MlttTermStore,
      context: MlttContext,
      name: String,
      annotation: Int,
      body: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    val annotationType = inferTerm(store, context, annotation, diagnostics)
    if typeUniverseLevel(store, annotationType).isEmpty then
      universeMismatch(store, context, annotationType, diagnostics)
    val bodyType =
      inferTerm(store, context.extendLocal(name, annotation), body, diagnostics)
    store.allocPi(name, annotation, bodyType)

  /** Infers application by eliminating a Pi type.
    *
    * Rules:
    *
    * {{{
    * f : (x: A) -> B
    * a : A
    * f(a) => B[a/x]
    * }}}
    *
    * The callee type is reduced to WHNF before requiring a Pi. The argument is
    * checked against the domain, then substituted into the codomain.
    */
  private def inferApply(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      function: Int,
      argument: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    val functionType = inferTerm(store, context, function, diagnostics)
    val functionWhnf = whnf(store, context, functionType, diagnostics)
    store.termValue(functionWhnf) match
      case MlttTerm.Pi(name, domain, body) =>
        checkTerm(store, context, argument, domain, diagnostics)
        substitute(store, body, name, argument)
      case MlttTerm.Error =>
        store.allocError()
      case _ =>
        diagnostics += diagnostic(
          NonFunctionApplicationCode,
          "application callee does not have a Pi type",
          context,
          "Pi",
          display(store, functionWhnf),
        )
        store.allocError()

  /** Infers Sigma projections.
    *
    * Rules:
    *
    * {{{
    * p : Sigma(first: A). B(first)
    * fst(p) => A
    * snd(p) => B(fst(p))
    * }}}
    *
    * The pair type is reduced to WHNF before requiring a Sigma type.
    */
  private def inferProjection(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      pair: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
      first: Boolean,
  ): Int =
    val pairType = whnf(
      store,
      context,
      inferTerm(store, context, pair, diagnostics),
      diagnostics,
    )
    store.termValue(pairType) match
      case MlttTerm.Sigma(name, firstType, secondType) =>
        if first then firstType
        else substitute(store, secondType, name, store.allocFst(pair))
      case MlttTerm.Error =>
        store.allocError()
      case _ =>
        diagnostics += diagnostic(
          TypeMismatchCode,
          "projection target does not have a Sigma type",
          context,
          "Sigma",
          display(store, pairType),
        )
        store.allocError()

  /** Synthesizes constructor types where the profile supports unambiguous
    * inference.
    *
    * Rules:
    *
    * {{{
    * Z => Nat
    * S(Z) => Nat
    * }}}
    *
    * Indexed constructors such as Vec generally require an expected family and
    * are handled by checking instead of synthesis.
    */
  private def inferConstructor(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      name: String,
      args: List[Int],
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    if name == "Z" && args.isEmpty then return store.allocInductive("Nat")
    if name == "S" && args.length == 1 then
      val nat = store.allocInductive("Nat")
      checkTerm(store, context, args.head, nat, diagnostics)
      return nat
    diagnostics += diagnostic(
      UnsupportedInferenceCode,
      "constructor inference requires an expected inductive family",
      context,
      "constructor type",
      display(store, term),
    )
    store.allocError()

  /** Infers the universe of an equality type.
    *
    * Rules:
    *
    * {{{
    * A : Type0
    * x : A
    * Eq(A, x, x) => Type0
    * }}}
    *
    * The value type must be universe-classified, and both sides are checked
    * against that value type.
    */
  private def inferEq(
      store: MlttTermStore,
      context: MlttContext,
      valueType: Int,
      left: Int,
      right: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    val valueTypeType = inferTerm(store, context, valueType, diagnostics)
    if typeUniverseLevel(store, valueTypeType).isEmpty then
      universeMismatch(store, context, valueTypeType, diagnostics)
    checkTerm(store, context, left, valueType, diagnostics)
    checkTerm(store, context, right, valueType, diagnostics)
    store.allocUniverse(0)

  /** Implements MLTT checking rules for introduction forms.
    *
    * Rules:
    *
    * {{{
    * fun x: A => body <= (x: A) -> B
    * (x, y) <= Sigma(first: A). B
    * Refl(x) <= Eq(A, x, x)
    * Cons(k, head, tail) <= Vec(A, S(k))
    * }}}
    *
    * If the term is not a recognized introduction for the expected WHNF type,
    * the checker infers the term and converts inferred and expected types.
    */
  private def checkTerm(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      expected: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Int =
    val expectedWhnf = whnf(store, context, expected, diagnostics)
    store.termValue(term) match
      case MlttTerm.Lambda(name, _, body)
          if checkLambda(
            store,
            context,
            name,
            body,
            expectedWhnf,
            diagnostics,
          ) =>
        return expected
      case MlttTerm.Pair(first, second)
          if checkPair(
            store,
            context,
            first,
            second,
            expectedWhnf,
            diagnostics,
          ) =>
        return expected
      case MlttTerm.Refl(value)
          if checkRefl(store, context, value, expectedWhnf, diagnostics) =>
        return expected
      case MlttTerm.Constructor(name, args)
          if checkConstructor(
            store,
            context,
            name,
            args,
            expectedWhnf,
            diagnostics,
          ) =>
        return expected
      case MlttTerm.Error =>
        return store.allocError()
      case _ =>

    val inferred = inferTerm(store, context, term, diagnostics)
    val converted = convert(store, context, inferred, expected)
    diagnostics ++= converted.diagnostics
    if !converted.isOk then
      diagnostics += diagnostic(
        TypeMismatchCode,
        "type mismatch",
        context,
        display(store, expected),
        display(store, inferred),
      )
    expected

  /** Checks lambda introduction against an expected Pi type.
    *
    * Rules:
    *
    * {{{
    * fun y: A => y <= (x: A) -> A
    * }}}
    *
    * The expected codomain is substituted with the source lambda binder before
    * checking the body.
    */
  private def checkLambda(
      store: MlttTermStore,
      context: MlttContext,
      name: String,
      body: Int,
      expected: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    store.termValue(expected) match
      case MlttTerm.Pi(expectedName, domain, codomain) =>
        val bodyType =
          substitute(store, codomain, expectedName, store.allocVar(name))
        checkTerm(
          store,
          context.extendLocal(name, domain),
          body,
          bodyType,
          diagnostics,
        )
        true
      case MlttTerm.Error => true
      case _              => false

  /** Checks pair introduction against an expected Sigma type.
    *
    * Rules:
    *
    * {{{
    * (x, y) <= Sigma(first: A). B(first)
    * }}}
    *
    * The first component checks against the Sigma first type. The second checks
    * against the second type after substituting the first component.
    */
  private def checkPair(
      store: MlttTermStore,
      context: MlttContext,
      first: Int,
      second: Int,
      expected: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    store.termValue(expected) match
      case MlttTerm.Sigma(name, firstType, secondType) =>
        checkTerm(store, context, first, firstType, diagnostics)
        checkTerm(
          store,
          context,
          second,
          substitute(store, secondType, name, first),
          diagnostics,
        )
        true
      case MlttTerm.Error => true
      case _              => false

  /** Checks reflexivity introduction against an equality type.
    *
    * Rules:
    *
    * {{{
    * Refl(x) <= Eq(A, x, x)
    * }}}
    *
    * The witness checks against the equality value type, then must convert to
    * both sides of the equality.
    */
  private def checkRefl(
      store: MlttTermStore,
      context: MlttContext,
      value: Int,
      expected: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    store.termValue(expected) match
      case MlttTerm.Eq(valueType, left, right) =>
        checkTerm(store, context, value, valueType, diagnostics)
        val leftOk = convert(store, context, value, left)
        val rightOk = convert(store, context, value, right)
        diagnostics ++= leftOk.diagnostics
        diagnostics ++= rightOk.diagnostics
        leftOk.isOk && rightOk.isOk
      case MlttTerm.Error => true
      case _              => false

  /** Checks constructors against an expected inductive family.
    *
    * Rules:
    *
    * {{{
    * Z <= Nat
    * Nil <= Vec(A, Z)
    * Cons(k, head, tail) <= Vec(A, S(k))
    * }}}
    *
    * This dispatcher keeps family-specific indexed constructor rules localized
    * to Nat and Vec helpers.
    */
  private def checkConstructor(
      store: MlttTermStore,
      context: MlttContext,
      name: String,
      args: List[Int],
      expected: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    store.termValue(expected) match
      case MlttTerm.Inductive("Nat", _, _) =>
        checkNatConstructor(store, context, name, args, expected, diagnostics)
      case MlttTerm.Inductive("Vec", params, indices) =>
        checkVecConstructor(
          store,
          context,
          name,
          args,
          params,
          indices,
          diagnostics,
        )
      case MlttTerm.Error => true
      case _              => false

  /** Checks Nat constructors.
    *
    * Rules:
    *
    * {{{
    * Z <= Nat
    * S(Z) <= Nat
    * }}}
    *
    * `S` recursively checks its predecessor against the expected Nat type.
    */
  private def checkNatConstructor(
      store: MlttTermStore,
      context: MlttContext,
      name: String,
      args: List[Int],
      expected: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    if name == "Z" && args.isEmpty then true
    else if name == "S" && args.length == 1 then
      checkTerm(store, context, args.head, expected, diagnostics)
      true
    else false

  /** Checks Vec constructors against the expected element and length indices.
    *
    * Rules:
    *
    * {{{
    * Nil <= Vec(A, Z)
    * Cons(k, head, tail) <= Vec(A, S(k))
    * }}}
    *
    * `Nil` is valid only at zero length. `Cons` is valid only when the expected
    * length normalizes to a successor.
    */
  private def checkVecConstructor(
      store: MlttTermStore,
      context: MlttContext,
      name: String,
      args: List[Int],
      params: List[Int],
      indices: List[Int],
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    if params.length != 1 || indices.length != 1 then return false
    if name == "Nil" && args.isEmpty then
      return isConstructor(store, indices.head, "Z", 0)
    if name == "Cons" && args.length == 3 then
      return checkVecConsConstructor(
        store,
        context,
        args,
        params.head,
        indices.head,
        diagnostics,
      )
    false

  /** Checks the indexed payload of `Cons`.
    *
    * Rules:
    *
    * {{{
    * k : Nat
    * head : A
    * tail : Vec(A, k)
    * Cons(k, head, tail) <= Vec(A, S(k))
    * }}}
    *
    * The explicit length argument must convert to the predecessor exposed by
    * the expected `S(k)` index.
    */
  private def checkVecConsConstructor(
      store: MlttTermStore,
      context: MlttContext,
      args: List[Int],
      elementType: Int,
      index: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    store.termValue(index) match
      case MlttTerm.Constructor("S", predecessor :: Nil) =>
        val nat = store.allocInductive("Nat")
        checkTerm(store, context, args(0), nat, diagnostics)
        checkTerm(store, context, args(1), elementType, diagnostics)
        val tailType =
          store.allocInductive("Vec", List(elementType), List(predecessor))
        checkTerm(store, context, args(2), tailType, diagnostics)
        val lengthOk = convert(store, context, args(0), predecessor)
        diagnostics ++= lengthOk.diagnostics
        lengthOk.isOk
      case _ => false

  /** Recognizes a constructor literal with the expected arity.
    *
    * Rules:
    *
    * {{{
    * S(k) // constructor "S" with arity 1
    * Nil  // constructor "Nil" with arity 0
    * }}}
    *
    * Vec checking uses this as a small premise before applying the
    * constructor-specific indexed introduction rule.
    */
  private def isConstructor(
      store: MlttTermStore,
      term: Int,
      name: String,
      arity: Int,
  ): Boolean =
    store.termValue(term) match
      case MlttTerm.Constructor(ctorName, args) =>
        ctorName == name && args.length == arity
      case _ => false

  /** Recursively checks definitional equality after WHNF reduction.
    *
    * Rules:
    *
    * {{{
    * (fun x: A => x)(y) == y
    * Vec(A, S(k)) == Vec(A, S(k))
    * Eq(A, x, x) == Eq(A, x, x)
    * }}}
    *
    * The rule first tries display-stable exact equality, then WHNF equality,
    * then recursively compares compatible term constructors.
    */
  private def convertTerms(
      store: MlttTermStore,
      context: MlttContext,
      left: Int,
      right: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    if sameTerm(store, left, right) then return true
    val leftWhnf = whnf(store, context, left, diagnostics)
    val rightWhnf = whnf(store, context, right, diagnostics)
    if sameTerm(store, leftWhnf, rightWhnf) then return true

    (store.termValue(leftWhnf), store.termValue(rightWhnf)) match
      case (
            MlttTerm.Pi(_, leftDomain, leftBody),
            MlttTerm.Pi(_, rightDomain, rightBody),
          ) =>
        convertTerms(store, context, leftDomain, rightDomain, diagnostics) &&
        convertTerms(store, context, leftBody, rightBody, diagnostics)
      case (
            MlttTerm.Sigma(_, leftFirst, leftSecond),
            MlttTerm.Sigma(_, rightFirst, rightSecond),
          ) =>
        convertTerms(store, context, leftFirst, rightFirst, diagnostics) &&
        convertTerms(store, context, leftSecond, rightSecond, diagnostics)
      case (
            MlttTerm.Apply(leftFunction, leftArgument),
            MlttTerm.Apply(rightFunction, rightArgument),
          ) =>
        convertTerms(
          store,
          context,
          leftFunction,
          rightFunction,
          diagnostics,
        ) &&
        convertTerms(store, context, leftArgument, rightArgument, diagnostics)
      case (
            MlttTerm.Inductive(leftName, leftParams, leftIndices),
            MlttTerm.Inductive(rightName, rightParams, rightIndices),
          ) =>
        leftName == rightName &&
        convertTermLists(
          store,
          context,
          leftParams,
          rightParams,
          diagnostics,
        ) &&
        convertTermLists(store, context, leftIndices, rightIndices, diagnostics)
      case (
            MlttTerm.Constructor(leftName, leftArgs),
            MlttTerm.Constructor(rightName, rightArgs),
          ) =>
        leftName == rightName && convertTermLists(
          store,
          context,
          leftArgs,
          rightArgs,
          diagnostics,
        )
      case (
            MlttTerm.Eq(leftType, leftLeft, leftRight),
            MlttTerm.Eq(rightType, rightLeft, rightRight),
          ) =>
        convertTerms(store, context, leftType, rightType, diagnostics) &&
        convertTerms(store, context, leftLeft, rightLeft, diagnostics) &&
        convertTerms(store, context, leftRight, rightRight, diagnostics)
      case (MlttTerm.Error, _) | (_, MlttTerm.Error) => true
      case _                                         => false

  /** Converts corresponding elements in parameter, index, or spine lists.
    *
    * Rules:
    *
    * {{{
    * Vec(A, S(k)) == Vec(A, S(k))
    * }}}
    *
    * Lists must have equal length and every pair of terms must convert.
    */
  private def convertTermLists(
      store: MlttTermStore,
      context: MlttContext,
      left: List[Int],
      right: List[Int],
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Boolean =
    left.length == right.length &&
      left.zip(right).forall { case (leftTerm, rightTerm) =>
        convertTerms(store, context, leftTerm, rightTerm, diagnostics)
      }

  /** Reduces a term to weak-head normal form for conversion.
    *
    * Rules:
    *
    * {{{
    * (fun x: A => x)(y) --> y
    * let z = y; z       --> y
    * fst((x, y))        --> x
    * }}}
    *
    * WHNF only reduces enough to expose the head constructor needed by
    * conversion and elimination rules.
    */
  private def whnf(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
      fuel: Int = 64,
  ): Int =
    if fuel <= 0 then return term
    store.termValue(term) match
      case MlttTerm.Var(name) =>
        whnfVar(store, context, term, name, diagnostics, fuel)
      case MlttTerm.Apply(function, argument) =>
        whnfApply(store, context, term, function, argument, diagnostics, fuel)
      case MlttTerm.Let(name, value, body) =>
        whnf(
          store,
          context,
          substitute(store, body, name, value),
          diagnostics,
          fuel - 1,
        )
      case MlttTerm.Fst(pair) =>
        whnfFst(store, context, term, pair, diagnostics, fuel)
      case MlttTerm.Snd(pair) =>
        whnfSnd(store, context, term, pair, diagnostics, fuel)
      case _ => term

  /** Reduces transparent pure definitions during conversion.
    *
    * Rules:
    *
    * {{{
    * add_z_n = n
    * add_z_n == n
    * }}}
    *
    * Opaque or effectful definitions are not reduced and produce a conversion
    * diagnostic.
    */
  private def whnfVar(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      name: String,
      diagnostics: ListBuffer[MlttDiagnostic],
      fuel: Int,
  ): Int =
    context.lookup(name).flatMap(_.value) match
      case Some(value) =>
        val entry = context.lookup(name).get
        if entry.transparent && entry.pure then
          whnf(store, context, value, diagnostics, fuel - 1)
        else
          diagnostics += diagnostic(
            EffectfulConversionCode,
            "conversion cannot reduce an opaque or effectful definition",
            context,
            "transparent pure definition",
            name,
          )
          term
      case None => term

  /** Performs beta reduction when a WHNF callee is a lambda.
    *
    * Rules:
    *
    * {{{
    * (fun x: A => x)(y) --> y
    * }}}
    */
  private def whnfApply(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      function: Int,
      argument: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
      fuel: Int,
  ): Int =
    store.termValue(whnf(store, context, function, diagnostics, fuel - 1)) match
      case MlttTerm.Lambda(name, _, body) =>
        whnf(
          store,
          context,
          substitute(store, body, name, argument),
          diagnostics,
          fuel - 1,
        )
      case _ => term

  /** Reduces first projection from a concrete pair.
    *
    * Rules:
    *
    * {{{
    * fst((x, y)) --> x
    * }}}
    */
  private def whnfFst(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      pair: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
      fuel: Int,
  ): Int =
    store.termValue(whnf(store, context, pair, diagnostics, fuel - 1)) match
      case MlttTerm.Pair(first, _) =>
        whnf(store, context, first, diagnostics, fuel - 1)
      case _ => term

  /** Reduces second projection from a concrete pair.
    *
    * Rules:
    *
    * {{{
    * snd((x, y)) --> y
    * }}}
    */
  private def whnfSnd(
      store: MlttTermStore,
      context: MlttContext,
      term: Int,
      pair: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
      fuel: Int,
  ): Int =
    store.termValue(whnf(store, context, pair, diagnostics, fuel - 1)) match
      case MlttTerm.Pair(_, second) =>
        whnf(store, context, second, diagnostics, fuel - 1)
      case _ => term

  /** Performs capture-oblivious substitution over the stored core term tree.
    *
    * Rules:
    *
    * {{{
    * substitute(x, x := y) = y
    * substitute((z: A) -> x, x := y) = (z: A) -> y
    * }}}
    *
    * Current fixtures avoid binder capture cases. The helper preserves binders
    * that shadow the substituted name and rebuilds all other term forms.
    */
  private def substitute(
      store: MlttTermStore,
      term: Int,
      name: String,
      replacement: Int,
  ): Int =
    store.termValue(term) match
      case MlttTerm.Var(varName) =>
        if varName == name then replacement else store.allocVar(varName)
      case MlttTerm.Universe(level) =>
        store.allocUniverse(level)
      case MlttTerm.Pi(param, domain, body) =>
        val nextDomain = substitute(store, domain, name, replacement)
        val nextBody =
          if param == name then body
          else substitute(store, body, name, replacement)
        store.allocPi(param, nextDomain, nextBody)
      case MlttTerm.Sigma(param, first, second) =>
        val nextFirst = substitute(store, first, name, replacement)
        val nextSecond =
          if param == name then second
          else substitute(store, second, name, replacement)
        store.allocSigma(param, nextFirst, nextSecond)
      case MlttTerm.Lambda(param, annotation, body) =>
        val nextAnnotation = substitute(store, annotation, name, replacement)
        val nextBody =
          if param == name then body
          else substitute(store, body, name, replacement)
        store.allocLambda(param, nextAnnotation, nextBody)
      case MlttTerm.Apply(function, argument) =>
        store.allocApply(
          substitute(store, function, name, replacement),
          substitute(store, argument, name, replacement),
        )
      case MlttTerm.Pair(first, second) =>
        store.allocPair(
          substitute(store, first, name, replacement),
          substitute(store, second, name, replacement),
        )
      case MlttTerm.Fst(pair) =>
        store.allocFst(substitute(store, pair, name, replacement))
      case MlttTerm.Snd(pair) =>
        store.allocSnd(substitute(store, pair, name, replacement))
      case MlttTerm.Let(localName, value, body) =>
        val nextValue = substitute(store, value, name, replacement)
        val nextBody =
          if localName == name then body
          else substitute(store, body, name, replacement)
        store.allocLet(localName, nextValue, nextBody)
      case MlttTerm.Inductive(inductiveName, params, indices) =>
        store.allocInductive(
          inductiveName,
          params.map(substitute(store, _, name, replacement)),
          indices.map(substitute(store, _, name, replacement)),
        )
      case MlttTerm.Constructor(ctorName, args) =>
        store.allocConstructor(
          ctorName,
          args.map(substitute(store, _, name, replacement)),
        )
      case MlttTerm.Eq(valueType, left, right) =>
        store.allocEq(
          substitute(store, valueType, name, replacement),
          substitute(store, left, name, replacement),
          substitute(store, right, name, replacement),
        )
      case MlttTerm.Refl(value) =>
        store.allocRefl(substitute(store, value, name, replacement))
      case MlttTerm.Neutral(head, spine) =>
        store.allocNeutral(
          head,
          spine.map(substitute(store, _, name, replacement)),
        )
      case MlttTerm.Meta(index) =>
        store.allocMeta(index)
      case MlttTerm.Error =>
        store.allocError()

  /** Compares stored terms by stable syntax before recursive conversion.
    *
    * Rules:
    *
    * {{{
    * Vec(A, S(k)) == Vec(A, S(k))
    * }}}
    *
    * This is the cheap equality premise used before WHNF reduction and
    * structural conversion recurse into compatible constructors.
    */
  private def sameTerm(store: MlttTermStore, left: Int, right: Int): Boolean =
    display(store, left) == display(store, right)

  /** Reads a universe level from a term.
    *
    * Rules:
    *
    * {{{
    * Type0 => Some(0)
    * A     => None
    * }}}
    */
  private def universeLevelOfTerm(store: MlttTermStore, id: Int): Option[Int] =
    store.termValue(id) match
      case MlttTerm.Universe(level) => Some(level)
      case _                        => None

  /** Converts an inferred classifier `Type(n + 1)` into the classified type's
    * universe level `n`.
    *
    * Rules:
    *
    * {{{
    * infer(Type0) = Type1
    * typeUniverseLevel(Type1) = 0
    * }}}
    */
  private def typeUniverseLevel(
      store: MlttTermStore,
      inferredType: Int,
  ): Option[Int] =
    universeLevelOfTerm(store, inferredType).map(level => (level - 1).max(0))

  /** Reports that a type former operand did not infer to a universe.
    *
    * Rules:
    *
    * {{{
    * (x: not_a_type) -> x
    * }}}
    */
  private def universeMismatch(
      store: MlttTermStore,
      context: MlttContext,
      actual: Int,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Unit =
    diagnostics += diagnostic(
      UniverseMismatchCode,
      "expected a universe-classified type",
      context,
      "Type",
      display(store, actual),
    )

  /** Reports public metavariables that remain unsolved after checking.
    *
    * Rules:
    *
    * {{{
    * ?m0 <= A
    * }}}
    *
    * The current checker does not perform general unification, so public metas
    * must be solved exactly by the harness before check completion.
    */
  private def reportUnsolvedMetas(
      context: MlttContext,
      metas: MlttMetaStore,
      diagnostics: ListBuffer[MlttDiagnostic],
  ): Unit =
    if metas.hasUnsolvedPublicMeta then
      diagnostics += diagnostic(
        UnsolvedConstraintCode,
        "checking completed with an unsolved metavariable",
        context,
        "solved constraints",
        "unsolved metavariable",
      )
