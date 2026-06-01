package cosmo0

class MlttProblemLadderTests extends munit.FunSuite:
  private final case class SupportedProblem(id: String, run: () => Boolean)

  private val supportedProblems = List(
    SupportedProblem("mltt-core-display", () => coreDisplay()),
    SupportedProblem("mltt-universe-variable", () => universeVariable()),
    SupportedProblem("mltt-pi-lambda-application", () => piLambdaApplication()),
    SupportedProblem("mltt-sigma-refl", () => sigmaRefl()),
    SupportedProblem("mltt-projection-whnf", () => projectionWhnf()),
    SupportedProblem(
      "mltt-conversion-beta-let-def",
      () => conversionBetaLetDef(),
    ),
    SupportedProblem(
      "mltt-conversion-effect-boundary",
      () => conversionEffectBoundary(),
    ),
    SupportedProblem("mltt-nat-vec-constructors", () => natVecConstructors()),
    SupportedProblem(
      "mltt-meta-exact-higher-order-reject",
      () => metaExactHigherOrderReject(),
    ),
  )

  private val futureProblemIds = List(
    "mltt-surface-holes",
    "mltt-implicit-arguments",
    "mltt-pruning-pattern-unification",
    "mltt-first-class-polymorphism",
    "mltt-dependent-pattern-impossible-branch",
    "mltt-identity-eliminator-j",
    "mltt-eta-cumulativity",
  )

  private val sourceUrls = List(
    "https://github.com/AndrasKovacs/elaboration-zoo",
    "https://davidchristiansen.dk/tutorials/nbe/",
    "https://www.cs.cmu.edu/~rwh/courses/atpl/pdfs/dependency.pdf",
    "https://wiki.portal.chalmers.se/agda/ReferenceManual/FindingTheValuesOfImplicitArguments",
    "https://leanprover-community.github.io/lean4-metaprogramming-book/main/02_overview.html",
    "https://research.chalmers.se/en/publication/519011",
    "https://arxiv.org/abs/2207.02129",
    "https://github.com/sweirich/pi-forall",
    "https://www.cs.ru.nl/~herman/TT/opg6.pdf",
  )

  test("MLTT problem ladder doc is source-tagged and linked from the book"):
    val doc =
      ParserFixtureManifest.readFile("docs/cosmo/mltt-problem-ladder.typ")
    val book = ParserFixtureManifest.readFile("docs/cosmo/book.typ")
    val intro = ParserFixtureManifest.readFile("docs/cosmo/intro.typ")

    assert(
      book.contains(
        "#prefix-chapter(\"mltt-problem-ladder.typ\")[MLTT Problem Ladder]",
      ),
    )
    assert(
      intro.contains(
        "#cross-link(\"/mltt-problem-ladder.typ\")[MLTT Problem Ladder]",
      ),
    )
    sourceUrls.foreach(url =>
      assert(doc.contains(url), s"missing MLTT source URL: $url"),
    )
    supportedProblems.foreach(problem =>
      assertProblemBlock(doc, problem.id, "supported-now"),
    )
    futureProblemIds.foreach(id =>
      assertProblemBlock(doc, id, "source-backed-future"),
    )
    assertProblemBlock(
      doc,
      "mltt-logic-proof-term-derivations",
      "paper-exercise",
    )

  test(
    "supported MLTT problem ladder entries execute against the Scala checker",
  ):
    supportedProblems.foreach: problem =>
      assert(problem.run(), s"supported MLTT problem failed: ${problem.id}")

  private def assertProblemBlock(
      doc: String,
      id: String,
      status: String,
  ): Unit =
    val marker = s"problem: `$id`"
    val start = doc.indexOf(marker)
    assert(start >= 0, s"missing MLTT problem marker: $id")

    val next = doc.indexOf("problem: `", start + marker.length)
    val block =
      if next < 0 then doc.substring(start) else doc.substring(start, next)
    assert(
      block.contains(s"status: `$status`"),
      s"wrong or missing status for MLTT problem: $id",
    )
    assert(
      block.contains("sources:"),
      s"missing source tag for MLTT problem: $id",
    )

  private def coreDisplay(): Boolean =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val n = store.allocVar("n")
    val vec = store.allocInductive("Vec", List(a), List(n))
    val pi = store.allocPi("x", a, a)
    val sigma = store.allocSigma("x", a, a)
    val eq = store.allocEq(a, n, n)

    MlttTypeChecker.display(store, type0) == "Type0" &&
    MlttTypeChecker.display(store, pi) == "(x: A) -> A" &&
    MlttTypeChecker.display(store, sigma) == "Sigma(x: A). A" &&
    MlttTypeChecker.display(store, eq) == "Eq(A, n, n)" &&
    MlttTypeChecker.display(store, vec) == "Vec(A, n)"

  private def universeVariable(): Boolean =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val universe =
      MlttTypeChecker.infer(store, MlttTypeChecker.context(), type0)
    val context = MlttTypeChecker.context().extendLocal("A", type0)
    val variable = MlttTypeChecker.infer(store, context, store.allocVar("A"))
    val missing = MlttTypeChecker.infer(
      store,
      MlttTypeChecker.context(),
      store.allocVar("missing"),
    )

    universe.isOk &&
    MlttTypeChecker.display(store, universe.valueType) == "Type1" &&
    variable.isOk &&
    MlttTypeChecker.display(store, variable.valueType) == "Type0" &&
    !missing.isOk &&
    missing.firstCode == MlttTypeChecker.UnknownVariableCode &&
    missing.diagnostics.head.profileId == CheckerProfiles.MlttCore.id

  private def piLambdaApplication(): Boolean =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val context = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("y", a)
    val x = store.allocVar("x")
    val lambda = store.allocLambda("x", a, x)
    val pi = store.allocPi("x", a, a)
    val checked = MlttTypeChecker.check(store, context, lambda, pi)
    val inferred = MlttTypeChecker.infer(
      store,
      context,
      store.allocApply(lambda, store.allocVar("y")),
    )

    checked.isOk &&
    checked.artifactSummary == "mltt.core|mltt-core-term|accepted|mltt.whnf-conversion" &&
    inferred.isOk &&
    MlttTypeChecker.display(store, inferred.valueType) == "A"

  private def sigmaRefl(): Boolean =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val x = store.allocVar("x")
    val context = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("x", a)
    val sigma = store.allocSigma("left", a, a)
    val pair = store.allocPair(x, x)
    val eq = store.allocEq(a, x, x)
    val refl = store.allocRefl(x)

    MlttTypeChecker.check(store, context, pair, sigma).isOk &&
    MlttTypeChecker.check(store, context, refl, eq).isOk

  private def projectionWhnf(): Boolean =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val x = store.allocVar("x")
    val y = store.allocVar("y")
    val context = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("x", a)
      .extendLocal("y", a)
    val pair = store.allocPair(x, y)
    val fst = store.allocFst(pair)
    val snd = store.allocSnd(pair)

    MlttTypeChecker.convert(store, context, fst, x).isOk &&
    MlttTypeChecker.convert(store, context, snd, y).isOk

  private def conversionBetaLetDef(): Boolean =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val context = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("y", a)
    val x = store.allocVar("x")
    val lambda = store.allocLambda("x", a, x)
    val y = store.allocVar("y")
    val beta = store.allocApply(lambda, y)
    val letTerm = store.allocLet("z", y, store.allocVar("z"))
    val nat = store.allocInductive("Nat")
    val n = store.allocVar("n")
    val natContext = MlttTypeChecker
      .context()
      .extendLocal("n", nat)
      .extendDefinition("add_Z_n", nat, n, transparent = true, pure = true)
    val addZN = store.allocVar("add_Z_n")

    MlttTypeChecker.convert(store, context, beta, y).isOk &&
    MlttTypeChecker.convert(store, context, letTerm, y).isOk &&
    MlttTypeChecker.convert(store, natContext, addZN, n).isOk

  private def conversionEffectBoundary(): Boolean =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val y = store.allocVar("y")
    val context = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("y", a)
      .extendDefinition("effectful", a, y, transparent = false, pure = false)
    val effectful = store.allocVar("effectful")
    val effectfulResult = MlttTypeChecker.convert(store, context, effectful, y)
    val strategyResult =
      MlttTypeChecker.convert(
        store,
        MlttTypeChecker.context(),
        type0,
        type0,
        "mltt.full-normalization",
      )

    !effectfulResult.isOk &&
    effectfulResult.firstCode == MlttTypeChecker.EffectfulConversionCode &&
    !strategyResult.isOk &&
    strategyResult.firstCode == MlttTypeChecker.UnsupportedNormalizationProfileCode

  private def natVecConstructors(): Boolean =
    val store = MlttTypeChecker.termStore()
    val env = MlttTypeChecker.declarationEnv()
    MlttTypeChecker.addNatFixture(store, env)
    MlttTypeChecker.addVecFixture(store, env)

    val natMetadataOk =
      env
        .findInductive("Nat")
        .exists(_.constructors.map(_.name) == List("Z", "S"))
    val vecMetadataOk =
      env
        .findInductive("Vec")
        .exists: vec =>
          vec.parameters.map(_.name) == List("A") &&
            vec.indices.map(_.name) == List("n") &&
            MlttTypeChecker.display(
              store,
              vec.constructors.head.result,
            ) == "Vec(A, Z)" &&
            MlttTypeChecker.display(
              store,
              vec.constructors(1).result,
            ) == "Vec(A, S(k))"
    if !natMetadataOk || !vecMetadataOk then return false

    val type0 = store.allocUniverse(0)
    val nat = store.allocInductive("Nat")
    val z = store.allocConstructor("Z")
    val sZ = store.allocConstructor("S", List(z))
    val natChecks =
      MlttTypeChecker.check(store, MlttTypeChecker.context(), sZ, nat).isOk

    val a = store.allocVar("A")
    val k = store.allocVar("k")
    val head = store.allocVar("head")
    val tail = store.allocVar("tail")
    val tailType = store.allocInductive("Vec", List(a), List(k))
    val vecContext = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("k", nat)
      .extendLocal("head", a)
      .extendLocal("tail", tailType)
    val vecAZ = store.allocInductive("Vec", List(a), List(z))
    val nil = store.allocConstructor("Nil")
    val sK = store.allocConstructor("S", List(k))
    val vecASK = store.allocInductive("Vec", List(a), List(sK))
    val cons = store.allocConstructor("Cons", List(k, head, tail))

    natChecks &&
    MlttTypeChecker.check(store, vecContext, nil, vecAZ).isOk &&
    MlttTypeChecker.check(store, vecContext, cons, vecASK).isOk

  private def metaExactHigherOrderReject(): Boolean =
    val store = MlttTypeChecker.termStore()
    val metas = MlttTypeChecker.metaStore()
    val type0 = store.allocUniverse(0)
    val metaId = metas.newMeta(MlttTypeChecker.context(), type0)
    val metaTerm = store.allocMeta(metaId)
    val diagnostic = MlttTypeChecker.higherOrderUnificationDiagnostic(
      MlttTypeChecker.context(),
    )

    metaId == 0 &&
    MlttTypeChecker.display(store, metaTerm) == "?m0" &&
    metas.hasUnsolvedPublicMeta &&
    metas.solveExact(metaId, type0) &&
    !metas.hasUnsolvedPublicMeta &&
    diagnostic.code == MlttTypeChecker.HigherOrderUnificationCode &&
    diagnostic.expected == "first-order pattern constraint"
end MlttProblemLadderTests
