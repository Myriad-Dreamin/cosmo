package cosmo0

class MlttTypeCheckerTests extends munit.FunSuite:
  test("Scala MLTT core display covers Pi, Sigma, equality, Nat, and Vec"):
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val n = store.allocVar("n")
    val vec = store.allocInductive("Vec", List(a), List(n))
    val pi = store.allocPi("x", a, a)
    val sigma = store.allocSigma("x", a, a)
    val eq = store.allocEq(a, n, n)

    assertEquals(MlttTypeChecker.display(store, type0), "Type0")
    assertEquals(MlttTypeChecker.display(store, pi), "(x: A) -> A")
    assertEquals(MlttTypeChecker.display(store, sigma), "Sigma(x: A). A")
    assertEquals(MlttTypeChecker.display(store, eq), "Eq(A, n, n)")
    assertEquals(MlttTypeChecker.display(store, vec), "Vec(A, n)")

  test("Scala MLTT checker infers universes and variables"):
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val universe =
      MlttTypeChecker.infer(store, MlttTypeChecker.context(), type0)
    val context = MlttTypeChecker.context().extendLocal("A", type0)
    val a = store.allocVar("A")
    val variable = MlttTypeChecker.infer(store, context, a)

    assert(universe.isOk)
    assertEquals(MlttTypeChecker.display(store, universe.valueType), "Type1")
    assert(variable.isOk)
    assertEquals(MlttTypeChecker.display(store, variable.valueType), "Type0")

  test("Scala MLTT checker checks lambdas and infers applications through Pi"):
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    var context = MlttTypeChecker.context().extendLocal("A", type0)
    context = context.extendLocal("y", a)

    val x = store.allocVar("x")
    val lambda = store.allocLambda("x", a, x)
    val pi = store.allocPi("x", a, a)
    val checked = MlttTypeChecker.check(store, context, lambda, pi)

    val y = store.allocVar("y")
    val apply = store.allocApply(lambda, y)
    val inferred = MlttTypeChecker.infer(store, context, apply)

    assert(checked.isOk)
    assertEquals(checked.profileId, CheckerProfiles.MlttCore.id)
    assertEquals(checked.artifactKind, "mltt-core-term")
    assertEquals(
      checked.artifactSummary,
      "mltt.core|mltt-core-term|accepted|mltt.whnf-conversion",
    )
    assert(inferred.isOk)
    assertEquals(MlttTypeChecker.display(store, inferred.valueType), "A")

  test(
    "Scala MLTT checker reports deterministic unknown variable and non-function diagnostics",
  ):
    val store = MlttTypeChecker.termStore()
    val missing = store.allocVar("missing")
    val missingResult =
      MlttTypeChecker.infer(store, MlttTypeChecker.context(), missing)

    val type0 = store.allocUniverse(0)
    val apply = store.allocApply(type0, type0)
    val applyResult =
      MlttTypeChecker.infer(store, MlttTypeChecker.context(), apply)

    assert(!missingResult.isOk)
    assertEquals(missingResult.firstCode, MlttTypeChecker.UnknownVariableCode)
    assertEquals(
      missingResult.diagnostics.head.profileId,
      CheckerProfiles.MlttCore.id,
    )
    assertEquals(missingResult.diagnostics.head.contextSummary, "<empty>")
    assert(!applyResult.isOk)
    assertEquals(
      applyResult.firstCode,
      MlttTypeChecker.NonFunctionApplicationCode,
    )

  test(
    "Scala MLTT conversion accepts beta, lets, and transparent Nat-style definitions",
  ):
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    var context = MlttTypeChecker.context().extendLocal("A", type0)
    context = context.extendLocal("y", a)

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

    assert(MlttTypeChecker.convert(store, context, beta, y).isOk)
    assert(MlttTypeChecker.convert(store, context, letTerm, y).isOk)
    assert(MlttTypeChecker.convert(store, natContext, addZN, n).isOk)

  test(
    "Scala MLTT conversion rejects effectful definitions and unknown strategies",
  ):
    val store = MlttTypeChecker.termStore()
    val a = store.allocVar("A")
    val y = store.allocVar("y")
    val context = MlttTypeChecker
      .context()
      .extendLocal("y", a)
      .extendDefinition("effectful", a, y, transparent = false, pure = false)
    val effectful = store.allocVar("effectful")
    val effectfulResult = MlttTypeChecker.convert(store, context, effectful, y)
    val type0 = store.allocUniverse(0)
    val strategyResult =
      MlttTypeChecker.convert(
        store,
        MlttTypeChecker.context(),
        type0,
        type0,
        "mltt.full-normalization",
      )

    assert(!effectfulResult.isOk)
    assertEquals(
      effectfulResult.firstCode,
      MlttTypeChecker.EffectfulConversionCode,
    )
    assert(!strategyResult.isOk)
    assertEquals(
      strategyResult.firstCode,
      MlttTypeChecker.UnsupportedNormalizationProfileCode,
    )

  test(
    "Scala MLTT checker handles Sigma pairs, Refl, Nat, and Vec constructor signatures",
  ):
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val x = store.allocVar("x")
    var context = MlttTypeChecker.context().extendLocal("A", type0)
    context = context.extendLocal("x", a)
    val sigma = store.allocSigma("left", a, a)
    val pair = store.allocPair(x, x)
    val eq = store.allocEq(a, x, x)
    val refl = store.allocRefl(x)

    assert(MlttTypeChecker.check(store, context, pair, sigma).isOk)
    assert(MlttTypeChecker.check(store, context, refl, eq).isOk)

    val nat = store.allocInductive("Nat")
    val z = store.allocConstructor("Z")
    val sZ = store.allocConstructor("S", List(z))
    assert(
      MlttTypeChecker.check(store, MlttTypeChecker.context(), sZ, nat).isOk,
    )

    val k = store.allocVar("k")
    val head = store.allocVar("head")
    val tail = store.allocVar("tail")
    var vecContext = MlttTypeChecker.context().extendLocal("A", type0)
    vecContext = vecContext.extendLocal("k", nat)
    vecContext = vecContext.extendLocal("head", a)
    val tailType = store.allocInductive("Vec", List(a), List(k))
    vecContext = vecContext.extendLocal("tail", tailType)
    val vecAZ = store.allocInductive("Vec", List(a), List(z))
    val nil = store.allocConstructor("Nil")
    val sK = store.allocConstructor("S", List(k))
    val vecASK = store.allocInductive("Vec", List(a), List(sK))
    val cons = store.allocConstructor("Cons", List(k, head, tail))

    assert(MlttTypeChecker.check(store, vecContext, nil, vecAZ).isOk)
    assert(MlttTypeChecker.check(store, vecContext, cons, vecASK).isOk)

  test(
    "Scala MLTT declaration metadata and metavariables match the cosmoc profile shape",
  ):
    val store = MlttTypeChecker.termStore()
    val env = MlttTypeChecker.declarationEnv()
    MlttTypeChecker.addNatFixture(store, env)
    MlttTypeChecker.addVecFixture(store, env)

    val nat = env.findInductive("Nat").get
    val vec = env.findInductive("Vec").get
    assertEquals(nat.constructors.map(_.name), List("Z", "S"))
    assertEquals(vec.parameters.map(_.name), List("A"))
    assertEquals(vec.indices.map(_.name), List("n"))
    assertEquals(
      MlttTypeChecker.display(store, vec.constructors.head.result),
      "Vec(A, Z)",
    )
    assertEquals(
      MlttTypeChecker.display(store, vec.constructors(1).result),
      "Vec(A, S(k))",
    )

    val metas = MlttTypeChecker.metaStore()
    val type0 = store.allocUniverse(0)
    val metaId = metas.newMeta(MlttTypeChecker.context(), type0)
    val metaTerm = store.allocMeta(metaId)
    assertEquals(metaId, 0)
    assertEquals(MlttTypeChecker.display(store, metaTerm), "?m0")
    assert(metas.hasUnsolvedPublicMeta)
    assert(metas.solveExact(metaId, type0))
    assert(!metas.hasUnsolvedPublicMeta)

    val diagnostic = MlttTypeChecker.higherOrderUnificationDiagnostic(
      MlttTypeChecker.context(),
    )
    assertEquals(diagnostic.code, MlttTypeChecker.HigherOrderUnificationCode)
    assertEquals(diagnostic.profileId, CheckerProfiles.MlttCore.id)
end MlttTypeCheckerTests
