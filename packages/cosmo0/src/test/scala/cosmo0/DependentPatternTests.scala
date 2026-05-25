package cosmo0

class DependentPatternTests extends munit.FunSuite:
  test("cosmo0 subset profile elaborates dependent Vec head pattern"):
    val fixture = vecHeadFixture(CheckerProfiles.Cosmo0Subset)
    val tree = fixture.tree

    assert(
      DependentPatterns.profileSupportsElaboration(CheckerProfiles.Cosmo0Subset),
    )
    assert(tree.isOk)
    assertEquals(tree.diagnostics, Nil)
    assertEquals(tree.familyDisplay, "Vec(A, S(n))")
    assertEquals(tree.branches.length, 1)
    assertEquals(tree.branches.head.constructor, "Cons")
    assertEquals(tree.branches.head.binders, List("head", "tail"))
    assertEquals(tree.branches.head.impossible, false)
    assertEquals(tree.refinements.length, 1)
    assertEquals(
      tree.refinements.head.contextBindings.map(_.name),
      List("head", "tail"),
    )
    assertEquals(tree.refinements.head.substitutionSummary, "n=k")
    assertEquals(tree.refinements.head.expectedType, "A under n=k")
    assertEquals(
      DependentPatterns.renderCaseTree(tree),
      "case xs : Vec(A, S(n)) of\n  Cons(head, tail) -> head [n=k]",
    )

  test(
    "Scala dependent pattern gate rejects profiles without elaboration support",
  ):
    val fixture = vecHeadFixture(CheckerProfiles.MlttCore)
    val tree = fixture.tree

    assert(
      !DependentPatterns.profileSupportsElaboration(CheckerProfiles.MlttCore),
    )
    assert(!tree.isOk)
    assertEquals(tree.branches, Nil)
    assertEquals(
      tree.firstCode,
      CheckerProfiles.UnsupportedDependentPatternCode,
    )
    assertEquals(tree.diagnostics.head.summary, CheckerProfiles.MlttCore.id)

  test("Scala dependent pattern unifier marks impossible constructor indices"):
    val store = DependentPatterns.termStore()
    val env = DependentPatterns.env()
    val patterns = DependentPatterns.sourcePatternStore()
    DependentPatterns.addNatFixture(store, env)
    DependentPatterns.addVecFixture(store, env)

    val n = store.allocVar("n")
    val sN = DependentPatterns.natSuccessor(store, n)
    val scrutineeIndices = DependentPatterns.vecIndices(store, sN)
    val nil = patterns.allocConstructor("Nil", Nil, span(8, 11))
    val clauses = List(DependentPatternClause(nil, "absurd", span(8, 19)))
    val tree =
      DependentPatterns.elaborateClauses(
        CheckerProfiles.Cosmo0Subset,
        env,
        store,
        patterns,
        "Vec",
        "xs",
        scrutineeIndices,
        "A",
        clauses,
      )

    assert(!tree.isOk)
    assertEquals(tree.branches.length, 1)
    assertEquals(tree.branches.head.constructor, "Nil")
    assertEquals(tree.branches.head.impossible, true)
    assertEquals(
      tree.firstCode,
      "cosmo.type.dependent-pattern.impossible-branch",
    )
    assertEquals(tree.diagnostics.head.summary, "S cannot refine Z")

  private final case class VecHeadFixture(tree: DependentCaseTree)

  private def vecHeadFixture(profile: CheckerProfile): VecHeadFixture =
    val store = DependentPatterns.termStore()
    val env = DependentPatterns.env()
    val patterns = DependentPatterns.sourcePatternStore()
    DependentPatterns.addNatFixture(store, env)
    DependentPatterns.addVecFixture(store, env)

    val n = store.allocVar("n")
    val sN = DependentPatterns.natSuccessor(store, n)
    val scrutineeIndices = DependentPatterns.vecIndices(store, sN)
    val headPat = patterns.allocVariable("head", span(12, 16))
    val tailPat = patterns.allocVariable("tail", span(18, 22))
    val cons =
      patterns.allocConstructor("Cons", List(headPat, tailPat), span(8, 23))
    val clauses = List(DependentPatternClause(cons, "head", span(8, 31)))
    val tree =
      DependentPatterns.elaborateClauses(
        profile,
        env,
        store,
        patterns,
        "Vec",
        "xs",
        scrutineeIndices,
        "A",
        clauses,
      )
    VecHeadFixture(tree)

  private def span(start: Int, end: Int): SourceSpan =
    SourceFile("<dependent-pattern-test>", " " * 80).span(start, end)
end DependentPatternTests
