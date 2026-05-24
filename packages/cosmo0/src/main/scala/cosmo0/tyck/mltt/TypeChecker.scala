package cosmo0

import scala.collection.mutable.ListBuffer

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

  def termStore(): MlttTermStore =
    MlttTermStore()

  def context(): MlttContext =
    MlttContext()

  def declarationEnv(): MlttDeclarationEnv =
    MlttDeclarationEnv()

  def metaStore(): MlttMetaStore =
    MlttMetaStore()

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

  def higherOrderUnificationDiagnostic(context: MlttContext): MlttDiagnostic =
    diagnostic(
      HigherOrderUnificationCode,
      "arbitrary higher-order unification is outside mltt.core",
      context,
      "first-order pattern constraint",
      "higher-order constraint",
    )

  def addNatFixture(store: MlttTermStore, env: MlttDeclarationEnv): Unit =
    val type0 = store.allocUniverse(0)
    val nat = store.allocInductive("Nat")
    val z = MlttConstructorDecl("Z", Nil, nat)
    val s = MlttConstructorDecl("S", List(MlttBinder("pred", nat)), nat)
    env.addInductive(MlttInductiveDecl("Nat", Nil, Nil, List(z, s)))
    env.addDefinition(
      MlttDefinitionDecl("Nat", type0, nat, transparent = true, pure = true),
    )

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

  private def sameTerm(store: MlttTermStore, left: Int, right: Int): Boolean =
    display(store, left) == display(store, right)

  private def universeLevelOfTerm(store: MlttTermStore, id: Int): Option[Int] =
    store.termValue(id) match
      case MlttTerm.Universe(level) => Some(level)
      case _                        => None

  private def typeUniverseLevel(
      store: MlttTermStore,
      inferredType: Int,
  ): Option[Int] =
    universeLevelOfTerm(store, inferredType).map(level => (level - 1).max(0))

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
