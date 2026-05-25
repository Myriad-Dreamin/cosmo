package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Index and family term fragment used by dependent-pattern elaboration.
  *
  * Term examples:
  *
  * {{{
  * n               // Var("n")
  * ?m0             // Meta(0)
  * Z               // Constructor("Z", Nil)
  * S(n)            // Constructor("S", List(n))
  * Vec(A, S(n))    // Family("Vec", List(A, S(n)))
  * }}}
  *
  * These terms model constructor result indices, not ordinary cosmo0
  * expressions. They are stored by integer ids in `DependentPatternTermStore`
  * so case-tree refinements can share the same compact representation as the
  * Scala MLTT mirror.
  */
enum DependentPatternTerm:
  case Var(name: String)
  case Meta(index: Int)
  case Constructor(name: String, args: List[Int])
  case Family(name: String, args: List[Int])
  case Error

final class DependentPatternTermStore:
  private val terms = ListBuffer.empty[DependentPatternTerm]

  def alloc(value: DependentPatternTerm): Int =
    val id = terms.length
    terms += value
    id

  def termValue(id: Int): DependentPatternTerm =
    terms(id)

  def allocVar(name: String): Int =
    alloc(DependentPatternTerm.Var(name))

  def allocMeta(index: Int): Int =
    alloc(DependentPatternTerm.Meta(index))

  def allocConstructor(name: String, args: List[Int] = Nil): Int =
    alloc(DependentPatternTerm.Constructor(name, args))

  def allocFamily(name: String, args: List[Int] = Nil): Int =
    alloc(DependentPatternTerm.Family(name, args))

  def allocError(): Int =
    alloc(DependentPatternTerm.Error)

  def size: Int =
    terms.length

final case class DependentPatternBinding(
    name: String,
    typ: Int,
)

final case class DependentPatternConstructorDecl(
    name: String,
    telescope: List[DependentPatternBinding],
    resultFamily: String,
    resultIndices: List[Int],
)

final case class DependentPatternInductiveDecl(
    name: String,
    parameters: List[DependentPatternBinding],
    indices: List[DependentPatternBinding],
    constructors: List[DependentPatternConstructorDecl],
)

final class DependentPatternEnv:
  private val inductives = ListBuffer.empty[DependentPatternInductiveDecl]

  def addInductive(decl: DependentPatternInductiveDecl): Unit =
    inductives += decl

  def findInductive(name: String): Option[DependentPatternInductiveDecl] =
    inductives.find(_.name == name)

  def findConstructor(name: String): Option[DependentPatternConstructorDecl] =
    inductives.iterator
      .flatMap(_.constructors.iterator)
      .find(_.name == name)

final case class DependentPatternDiagnostic(
    code: String,
    message: String,
    span: SourceSpan,
    summary: String,
)

final case class DependentPatternUnifyResult(
    ok: Boolean,
    impossible: Boolean,
    substitutions: Map[String, Int],
    diagnostics: List[DependentPatternDiagnostic],
):
  def isOk: Boolean =
    ok && diagnostics.isEmpty

  def firstCode: String =
    diagnostics.headOption.map(_.code).getOrElse("")

/** Source-level pattern fragment accepted by the dependent-pattern experiment.
  *
  * Pattern examples for `xs : Vec(A, S(n))`:
  *
  * {{{
  * Cons(head, tail)  // refines the length index by unifying S(n) with S(k)
  * _                 // catch-all branch
  * impossible        // source marks an unreachable branch
  * left = right      // represented but currently rejected by elaboration
  * }}}
  */
enum DependentSourcePattern:
  case Constructor(name: String, args: List[Int], span: SourceSpan)
  case Variable(name: String, span: SourceSpan)
  case Wildcard(span: SourceSpan)
  case Impossible(span: SourceSpan)
  case Equality(left: Int, right: Int, span: SourceSpan)

final class DependentSourcePatternStore:
  private val patterns = ListBuffer.empty[DependentSourcePattern]

  def alloc(value: DependentSourcePattern): Int =
    val id = patterns.length
    patterns += value
    id

  def patternValue(id: Int): DependentSourcePattern =
    patterns(id)

  def allocConstructor(name: String, args: List[Int], span: SourceSpan): Int =
    alloc(DependentSourcePattern.Constructor(name, args, span))

  def allocVariable(name: String, span: SourceSpan): Int =
    alloc(DependentSourcePattern.Variable(name, span))

  def allocWildcard(span: SourceSpan): Int =
    alloc(DependentSourcePattern.Wildcard(span))

  def allocImpossible(span: SourceSpan): Int =
    alloc(DependentSourcePattern.Impossible(span))

  def allocEquality(left: Int, right: Int, span: SourceSpan): Int =
    alloc(DependentSourcePattern.Equality(left, right, span))

final case class DependentPatternClause(
    pattern: Int,
    body: String,
    span: SourceSpan,
)

final case class DependentPatternBranchRefinement(
    constructor: String,
    impossible: Boolean,
    contextBindings: List[DependentPatternBinding],
    substitutionSummary: String,
    expectedType: String,
    span: SourceSpan,
)

final case class DependentCaseTreeBranch(
    constructor: String,
    binders: List[String],
    body: String,
    impossible: Boolean,
    span: SourceSpan,
    refinementSummary: String,
)

final case class DependentCaseTree(
    scrutinee: String,
    familyDisplay: String,
    branches: List[DependentCaseTreeBranch],
    refinements: List[DependentPatternBranchRefinement],
    diagnostics: List[DependentPatternDiagnostic],
):
  def isOk: Boolean =
    diagnostics.isEmpty

  def firstCode: String =
    diagnostics.headOption.map(_.code).getOrElse("")

/** Elaborates dependent source pattern clauses into a small case tree.
  *
  * Program examples:
  *
  * {{{
  * case xs : Vec(A, S(n)) of
  *   Cons(head, tail) -> head
  *
  * case xs : Vec(A, S(n)) of
  *   Nil -> absurd
  * }}}
  *
  * Direct harness example:
  *
  * {{{
  * val store = DependentPatterns.termStore()
  * val env = DependentPatterns.env()
  * val patterns = DependentPatterns.sourcePatternStore()
  * DependentPatterns.addNatFixture(store, env)
  * DependentPatterns.addVecFixture(store, env)
  *
  * val n = store.allocVar("n")
  * val xsType = DependentPatterns.vecIndices(
  *   store,
  *   DependentPatterns.natSuccessor(store, n),
  * )
  * val cons = patterns.allocConstructor(
  *   "Cons",
  *   List(patterns.allocVariable("head", span), patterns.allocVariable("tail", span)),
  *   span,
  * )
  * DependentPatterns.elaborateClauses(
  *   CheckerProfiles.MlttDependentPatterns,
  *   env,
  *   store,
  *   patterns,
  *   "Vec",
  *   "xs",
  *   xsType,
  *   "A",
  *   List(DependentPatternClause(cons, "head", span)),
  * )
  * }}}
  *
  * Rules:
  *
  * {{{
  * profile supports dependent-pattern-elaboration
  *   --------------------------------------------------- ProfileGate
  *   elaborate clauses under profile
  *
  * ctor : (tel) -> F(actualIndices)
  * unify(expectedIndices, actualIndices) = subst
  *   --------------------------------------------------- ConstructorBranch
  *   case x : F(expectedIndices) of ctor(patterns) -> body
  *     elaborates with tel binders and branch refinement subst
  *
  * unify(expectedIndices, actualIndices) = impossible
  *   --------------------------------------------------- ImpossibleBranch
  *   ctor branch is preserved but marked impossible
  *
  * case x : F(indices) of y -> body
  * case x : F(indices) of _ -> body
  *   --------------------------------------------------- CatchAll
  *   branch covers every remaining possible constructor
  *
  * case x : F(indices) of left = right -> body
  *   --------------------------------------------------- EqualityPattern
  *   rejected as outside the accepted dependent-pattern fragment
  *
  * forall ctor in F.constructors.
  *   unify(scrutineeIndices, ctor.resultIndices) is impossible
  *   or ctor is covered
  *   or a catch-all branch exists
  *   --------------------------------------------------- Coverage
  *   case tree is exhaustive
  *
  * unify(v, term) = { v := term } when v notin FV(term)
  * unify(H(xs), H(ys)) = zipWith unify xs ys
  * unify(H(xs), K(ys)) = impossible when H != K
  * unify(v, term) = occurs-check failure when v in FV(term)
  * }}}
  *
  * Explanation:
  *
  * Elaboration is driven by constructor result indices. A `Cons` branch for
  * `Vec(A, S(n))` compares the scrutinee indices `[A, S(n)]` with the
  * constructor result indices `[A, S(k)]`, yielding the refinement `n=k`. A
  * `Nil` branch compares `[A, S(n)]` with `[A, Z]`, so the branch is diagnosed
  * as impossible instead of being treated as a valid head case.
  *
  * Pipeline note: `MlttTypeChecker` owns the public profile entry points and
  * calls this object for dependent-pattern directives and source-match hooks.
  */
object DependentPatterns:
  private val UnsupportedUnificationCode =
    "cosmo.type.dependent-pattern.unsupported-unification"
  private val ImpossibleBranchCode =
    "cosmo.type.dependent-pattern.impossible-branch"
  val NoSourceAssertionsCode =
    "cosmo.type.dependent-pattern.no-profile-assertions"
  val UnknownSourceAssertionCode =
    "cosmo.type.dependent-pattern.unknown-profile-assertion"
  val SourceAssertionFailedCode =
    "cosmo.type.dependent-pattern.profile-assertion-failed"

  private val SourceDirectivePrefix = "dependent-pattern:"

  def termStore(): DependentPatternTermStore =
    DependentPatternTermStore()

  def sourcePatternStore(): DependentSourcePatternStore =
    DependentSourcePatternStore()

  def env(): DependentPatternEnv =
    DependentPatternEnv()

  private[cosmo0] def hasSourceAssertions(sources: List[SourceFile]): Boolean =
    ProfileDirectiveParser.extract(sources, SourceDirectivePrefix).nonEmpty

  /** Checks dependent-pattern assertion directives for the MLTT checker.
    *
    * Directive examples:
    *
    * {{{
    * dependent-pattern: vec-head-elaborates
    * dependent-pattern: impossible-nil-diagnostic
    * }}}
    *
    * The source directives construct dependent-pattern fixtures inside this
    * object and exercise the same case-tree elaboration and index-refinement
    * rules as direct calls to `elaborateClauses`. `MlttTypeChecker` owns the
    * profile-level source/package entry points and calls this helper when the
    * selected profile is `mltt.dependent-patterns`.
    */
  private[cosmo0] def checkSourceAssertions(
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
            "mltt.dependent-patterns source did not declare any dependent-pattern assertions",
            wholeSourceSpan(sources),
          ),
        ),
      )

    val diagnostics = directives.flatMap(runSourceAssertion)
    if diagnostics.nonEmpty then return Result.failure(Phase.Check, diagnostics)

    Result.success(
      Phase.Check,
      syntheticModule(moduleName, directives, "dependent_pattern"),
    )

  /** Dispatches a dependent-pattern source directive to the corresponding
    * elaboration rule.
    */
  private def runSourceAssertion(
      directive: ProfileDirective,
  ): List[Diagnostic] =
    directive.name match
      case "vec-head-elaborates" =>
        vecHeadElaboratesAssertion(directive.span)
      case "impossible-nil-diagnostic" =>
        impossibleNilDiagnosticAssertion(directive.span)
      case other =>
        List(
          sourceDiagnostic(
            UnknownSourceAssertionCode,
            s"unknown dependent-pattern assertion $other",
            directive.span,
          ),
        )

  /** Checks constructor-pattern elaboration for a non-empty vector.
    *
    * Program examples:
    *
    * {{{
    * case xs : Vec(A, S(n)) of
    *   Cons(head, tail) -> head
    * }}}
    *
    * Expected case tree:
    *
    * {{{
    * case xs : Vec(A, S(n)) of
    *   Cons(head, tail) -> head [n=k]
    * }}}
    */
  private def vecHeadElaboratesAssertion(
      span: SourceSpan,
  ): List[Diagnostic] =
    val tree = vecHeadTree(CheckerProfiles.MlttDependentPatterns, span)
    val rendered = renderCaseTree(tree)
    val expected = "case xs : Vec(A, S(n)) of\n  Cons(head, tail) -> head [n=k]"

    sourceDiagnostics(tree.diagnostics) :::
      assertSourceCondition(
        tree.isOk &&
          tree.branches.length == 1 &&
          tree.branches.head.constructor == "Cons" &&
          rendered == expected,
        "Vec head pattern did not elaborate to the expected case tree",
        expected,
        rendered,
        span,
      )

  /** Checks that impossible constructor indices are diagnosed.
    *
    * Program examples:
    *
    * {{{
    * case xs : Vec(A, S(n)) of
    *   Nil -> absurd
    * }}}
    */
  private def impossibleNilDiagnosticAssertion(
      span: SourceSpan,
  ): List[Diagnostic] =
    val store = termStore()
    val envValue = env()
    val patterns = sourcePatternStore()
    addNatFixture(store, envValue)
    addVecFixture(store, envValue)

    val n = store.allocVar("n")
    val sN = natSuccessor(store, n)
    val scrutineeIndices = vecIndices(store, sN)
    val nil = patterns.allocConstructor("Nil", Nil, span)
    val clauses = List(DependentPatternClause(nil, "absurd", span))
    val tree =
      elaborateClauses(
        CheckerProfiles.MlttDependentPatterns,
        envValue,
        store,
        patterns,
        "Vec",
        "xs",
        scrutineeIndices,
        "A",
        clauses,
      )

    assertSourceCondition(
      !tree.isOk &&
        tree.firstCode == ImpossibleBranchCode &&
        tree.branches.headOption.exists(_.impossible),
      "Nil branch did not report impossible indices for Vec(A, S(n))",
      ImpossibleBranchCode,
      tree.firstCode,
      span,
    )

  /** Builds the reusable `Vec(A, S(n))` head-elaboration program. */
  private def vecHeadTree(
      profile: CheckerProfile,
      span: SourceSpan,
  ): DependentCaseTree =
    val store = termStore()
    val envValue = env()
    val patterns = sourcePatternStore()
    addNatFixture(store, envValue)
    addVecFixture(store, envValue)

    val n = store.allocVar("n")
    val sN = natSuccessor(store, n)
    val scrutineeIndices = vecIndices(store, sN)
    val headPat = patterns.allocVariable("head", span)
    val tailPat = patterns.allocVariable("tail", span)
    val cons = patterns.allocConstructor("Cons", List(headPat, tailPat), span)
    val clauses = List(DependentPatternClause(cons, "head", span))
    elaborateClauses(
      profile,
      envValue,
      store,
      patterns,
      "Vec",
      "xs",
      scrutineeIndices,
      "A",
      clauses,
    )

  private def sourceDiagnostics(
      diagnostics: List[DependentPatternDiagnostic],
  ): List[Diagnostic] =
    diagnostics.map { diagnosticValue =>
      sourceDiagnostic(
        diagnosticValue.code,
        s"${diagnosticValue.message}; ${diagnosticValue.summary}",
        diagnosticValue.span,
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

  private def sanitizeName(value: String): String =
    value.map {
      case char if char.isLetterOrDigit => char
      case _                            => '_'
    }

  private def wholeSourceSpan(sources: List[SourceFile]): SourceSpan =
    sources.headOption match
      case Some(source) => source.span(0, source.text.length)
      case None         => SourceFile("<dependent-pattern>", "").span(0, 0)

  private def sourceDiagnostic(
      code: String,
      message: String,
      span: SourceSpan,
  ): Diagnostic =
    Diagnostic(Phase.Check, DiagnosticSeverity.Error, code, message, Some(span))

  /** Reports whether the selected checker profile admits the elaboration rule.
    *
    * Program examples:
    *
    * {{{
    * checkerProfile: "mltt.dependent-patterns"
    *
    * case xs : Vec(A, S(n)) of
    *   Cons(head, tail) -> head
    * }}}
    *
    * This gate is the profile-side premise for every dependent-pattern rule
    * below. Profiles without `dependent-pattern-elaboration` cannot elaborate
    * constructor refinements or coverage obligations.
    */
  def profileSupportsElaboration(profile: CheckerProfile): Boolean =
    profile.supports(CheckerProfiles.DependentPatternElaborationFeature)

  def unsupportedProfileDiagnostic(
      profile: CheckerProfile,
      span: SourceSpan,
  ): DependentPatternDiagnostic =
    DependentPatternDiagnostic(
      CheckerProfiles.UnsupportedDependentPatternCode,
      "checker profile does not support dependent pattern elaboration",
      span,
      profile.id,
    )

  /** Unifies expected scrutinee indices with constructor result indices.
    *
    * Program examples:
    *
    * {{{
    * xs : Vec(A, S(n))
    *
    * Cons(head, tail) -> head
    * // Cons result index is Vec(A, S(k)); unification records n = k.
    * }}}
    *
    * Rules:
    *
    * {{{
    * unifyIndices([A, S(n)], [A, S(k)]) = { n := k }
    * unifyIndices([A, S(n)], [A, Z])    = impossible
    * }}}
    *
    * Explanation:
    *
    * The expected index list comes from the scrutinee type, and the actual
    * index list comes from the constructor result family. Each pair is sent to
    * the first-order index unifier; arity mismatch is a hard diagnostic.
    */
  def unifyIndices(
      store: DependentPatternTermStore,
      expected: List[Int],
      actual: List[Int],
      span: SourceSpan,
  ): DependentPatternUnifyResult =
    val unifier = DependentPatternUnifier(store, span)
    if expected.length != actual.length then
      unifier.fail(
        UnsupportedUnificationCode,
        "family index arity mismatch",
        s"expected ${expected.length} indices",
      )
      return unifier.finish()

    expected.zip(actual).foreach { case (left, right) =>
      unifier.unify(left, right)
    }
    unifier.finish()

  /** Elaborates source clauses into a dependent case tree.
    *
    * Program examples:
    *
    * {{{
    * case xs : Vec(A, S(n)) of
    *   Cons(head, tail) -> head
    * }}}
    *
    * The constructor rule checks that `Cons` can refine the scrutinee family,
    * imports the constructor telescope as branch binders, specializes the
    * expected branch type using the unifier result, and then records coverage
    * obligations for any remaining possible constructors.
    *
    * Explanation:
    *
    * This is the orchestration point for profile gating, constructor
    * elaboration, wildcard handling, impossible branches, unsupported equality
    * patterns, and final coverage checking.
    */
  def elaborateClauses(
      profile: CheckerProfile,
      env: DependentPatternEnv,
      store: DependentPatternTermStore,
      patterns: DependentSourcePatternStore,
      familyName: String,
      scrutinee: String,
      scrutineeIndices: List[Int],
      expectedType: String,
      clauses: List[DependentPatternClause],
  ): DependentCaseTree =
    val diagnostics = ListBuffer.empty[DependentPatternDiagnostic]
    val branches = ListBuffer.empty[DependentCaseTreeBranch]
    val refinements = ListBuffer.empty[DependentPatternBranchRefinement]

    if !profileSupportsElaboration(profile) then
      diagnostics += unsupportedProfileDiagnostic(
        profile,
        firstClauseSpan(clauses),
      )
      return DependentCaseTree(
        scrutinee,
        familyDisplay(store, familyName, scrutineeIndices),
        branches.toList,
        refinements.toList,
        diagnostics.toList,
      )

    val seen = ListBuffer.empty[String]
    var wildcardSeen = false

    clauses.foreach { clause =>
      patterns.patternValue(clause.pattern) match
        case DependentSourcePattern.Constructor(name, args, span) =>
          if wildcardSeen || seen.contains(name) then
            diagnostics += DependentPatternDiagnostic(
              "cosmo.type.dependent-pattern.redundant-branch",
              "an earlier clause already covers this refined constructor space",
              span,
              name,
            )
          elaborateConstructorClause(
            env,
            store,
            name,
            span,
            expectedType,
            clause.body,
            scrutineeIndices,
            branches,
            refinements,
            diagnostics,
          )
          if !seen.contains(name) then seen += name

        case DependentSourcePattern.Variable(name, span) =>
          if wildcardSeen then
            diagnostics += DependentPatternDiagnostic(
              "cosmo.type.dependent-pattern.redundant-branch",
              "an earlier wildcard already covers this refined space",
              span,
              name,
            )
          wildcardSeen = true
          branches += DependentCaseTreeBranch(
            "_",
            Nil,
            clause.body,
            impossible = false,
            span,
            "catch-all",
          )

        case DependentSourcePattern.Wildcard(span) =>
          if wildcardSeen then
            diagnostics += DependentPatternDiagnostic(
              "cosmo.type.dependent-pattern.redundant-branch",
              "an earlier wildcard already covers this refined space",
              span,
              "_",
            )
          wildcardSeen = true
          branches += DependentCaseTreeBranch(
            "_",
            Nil,
            clause.body,
            impossible = false,
            span,
            "catch-all",
          )

        case DependentSourcePattern.Impossible(span) =>
          branches += DependentCaseTreeBranch(
            "!",
            Nil,
            clause.body,
            impossible = true,
            span,
            "source-impossible",
          )

        case DependentSourcePattern.Equality(left, right, span) =>
          diagnostics += DependentPatternDiagnostic(
            "cosmo.type.unsupported-equality-pattern",
            "equality pattern matching is outside the accepted dependent-pattern fragment",
            span,
            s"${termDisplay(store, left)} = ${termDisplay(store, right)}",
          )
    }

    checkCoverage(
      env,
      store,
      familyName,
      scrutineeIndices,
      seen.toList,
      wildcardSeen,
      diagnostics,
    )
    DependentCaseTree(
      scrutinee,
      familyDisplay(store, familyName, scrutineeIndices),
      branches.toList,
      refinements.toList,
      diagnostics.toList,
    )

  def addNatFixture(
      store: DependentPatternTermStore,
      env: DependentPatternEnv,
  ): Unit =
    val natType = store.allocFamily("Nat")
    val zCtor = DependentPatternConstructorDecl("Z", Nil, "Nat", Nil)
    val predBinding = DependentPatternBinding("pred", natType)
    val sCtor =
      DependentPatternConstructorDecl("S", List(predBinding), "Nat", Nil)
    env.addInductive(
      DependentPatternInductiveDecl("Nat", Nil, Nil, List(zCtor, sCtor)),
    )

  def addVecFixture(
      store: DependentPatternTermStore,
      env: DependentPatternEnv,
  ): Unit =
    val typeUniverse = store.allocFamily("Type")
    val natType = store.allocFamily("Nat")
    val a = store.allocVar("A")
    val k = store.allocVar("k")
    val z = store.allocConstructor("Z")
    val sK = store.allocConstructor("S", List(k))
    val vecAK = store.allocFamily("Vec", List(a, k))

    val parameters = List(DependentPatternBinding("A", typeUniverse))
    val indices = List(DependentPatternBinding("n", natType))
    val nilCtor = DependentPatternConstructorDecl("Nil", Nil, "Vec", List(a, z))
    val consTelescope =
      List(
        DependentPatternBinding("head", a),
        DependentPatternBinding("tail", vecAK),
      )
    val consCtor =
      DependentPatternConstructorDecl("Cons", consTelescope, "Vec", List(a, sK))
    env.addInductive(
      DependentPatternInductiveDecl(
        "Vec",
        parameters,
        indices,
        List(nilCtor, consCtor),
      ),
    )

  def vecIndices(
      store: DependentPatternTermStore,
      length: Int,
  ): List[Int] =
    List(store.allocVar("A"), length)

  def natSuccessor(
      store: DependentPatternTermStore,
      value: Int,
  ): Int =
    store.allocConstructor("S", List(value))

  def familyDisplay(
      store: DependentPatternTermStore,
      name: String,
      indices: List[Int],
  ): String =
    s"$name(${indicesDisplay(store, indices)})"

  def indicesDisplay(
      store: DependentPatternTermStore,
      indices: List[Int],
  ): String =
    indices.map(termDisplay(store, _)).mkString(", ")

  def termDisplay(
      store: DependentPatternTermStore,
      id: Int,
  ): String =
    store.termValue(id) match
      case DependentPatternTerm.Var(name) =>
        name
      case DependentPatternTerm.Meta(index) =>
        s"?m$index"
      case DependentPatternTerm.Constructor(name, args) =>
        headDisplay(store, name, args)
      case DependentPatternTerm.Family(name, args) =>
        headDisplay(store, name, args)
      case DependentPatternTerm.Error =>
        "<error>"

  def renderCaseTree(tree: DependentCaseTree): String =
    val head = s"case ${tree.scrutinee} : ${tree.familyDisplay} of"
    val branchText =
      tree.branches.map(branch => s"  ${renderCaseBranch(branch)}")
    (head :: branchText).mkString("\n")

  /** Elaborates one constructor branch and its index refinement.
    *
    * Program examples:
    *
    * {{{
    * case xs : Vec(A, S(n)) of
    *   Cons(head, tail) -> head
    * }}}
    *
    * `Cons` contributes binders from its telescope:
    *
    * {{{
    * head : A
    * tail : Vec(A, k)
    * }}}
    *
    * Rules:
    *
    * {{{
    * ctor : telescope -> Vec(A, S(k))
    * unify([A, S(n)], [A, S(k)]) = { n := k }
    *   --------------------------------------------------- ConstructorBranch
    *   Cons(head, tail) elaborates with binders and refinement n=k
    * }}}
    *
    * Explanation:
    *
    * The constructor telescope becomes branch binders. The constructor result
    * indices are unified with the scrutinee indices; if the same rule is tried
    * with `Nil`, the heads `S` and `Z` differ and the branch is marked
    * impossible.
    */
  private def elaborateConstructorClause(
      env: DependentPatternEnv,
      store: DependentPatternTermStore,
      name: String,
      span: SourceSpan,
      expectedType: String,
      body: String,
      scrutineeIndices: List[Int],
      branches: ListBuffer[DependentCaseTreeBranch],
      refinements: ListBuffer[DependentPatternBranchRefinement],
      diagnostics: ListBuffer[DependentPatternDiagnostic],
  ): Unit =
    env.findConstructor(name) match
      case Some(ctor) =>
        val unified =
          unifyIndices(store, scrutineeIndices, ctor.resultIndices, span)
        diagnostics ++= unified.diagnostics
        val binders = ctor.telescope.map(_.name)
        val summary = refinementSummary(store, unified)
        val specializedExpected = specializedExpectedType(expectedType, summary)
        branches += DependentCaseTreeBranch(
          name,
          binders,
          body,
          unified.impossible,
          span,
          summary,
        )
        refinements += DependentPatternBranchRefinement(
          name,
          unified.impossible,
          ctor.telescope,
          summary,
          specializedExpected,
          span,
        )
      case None =>
        diagnostics += DependentPatternDiagnostic(
          "cosmo.type.dependent-pattern.unknown-constructor",
          "constructor is unavailable in the dependent-pattern environment",
          span,
          name,
        )

  /** Checks that all possible constructors are covered after refinement.
    *
    * Program examples:
    *
    * {{{
    * case xs : Vec(A, S(n)) of
    *   Cons(head, tail) -> head
    * }}}
    *
    * Rules:
    *
    * {{{
    * forall ctor in Vec.constructors.
    *   unify(scrutineeIndices, ctor.resultIndices) is impossible
    *   or ctor has a branch
    *   or a wildcard branch exists
    * }}}
    *
    * Explanation:
    *
    * Coverage does not require `Nil` here, because `Vec(A, S(n))` cannot unify
    * with `Nil`'s result index `Vec(A, Z)`. For `xs : Vec(A, n)`, both `Nil`
    * and `Cons` remain possible unless a wildcard branch appears.
    */
  private def checkCoverage(
      env: DependentPatternEnv,
      store: DependentPatternTermStore,
      familyName: String,
      scrutineeIndices: List[Int],
      seen: List[String],
      wildcardSeen: Boolean,
      diagnostics: ListBuffer[DependentPatternDiagnostic],
  ): Unit =
    env.findInductive(familyName) match
      case Some(family) =>
        family.constructors.foreach { ctor =>
          val unified =
            unifyIndices(store, scrutineeIndices, ctor.resultIndices, emptySpan)
          if !unified.impossible && !wildcardSeen && !seen.contains(ctor.name)
          then
            diagnostics += DependentPatternDiagnostic(
              "cosmo.type.dependent-pattern.missing-branch",
              "missing constructor branch for dependent pattern match",
              emptySpan,
              s"$familyName ${indicesDisplay(store, scrutineeIndices)} lacks ${ctor.name}",
            )
        }
      case None =>
        diagnostics += DependentPatternDiagnostic(
          "cosmo.type.dependent-pattern.unknown-family",
          "family is unavailable in the dependent-pattern environment",
          emptySpan,
          familyName,
        )

  private def firstClauseSpan(
      clauses: List[DependentPatternClause],
  ): SourceSpan =
    clauses.headOption.map(_.span).getOrElse(emptySpan)

  private def headDisplay(
      store: DependentPatternTermStore,
      name: String,
      args: List[Int],
  ): String =
    if args.isEmpty then name
    else s"$name(${indicesDisplay(store, args)})"

  /** Renders the branch refinement inferred by constructor-index unification.
    *
    * Rules:
    *
    * {{{
    * Cons(head, tail) -> head  // summary: n=k
    * Nil -> absurd             // summary: impossible for Vec(A, S(n))
    * _ -> fallback             // summary: identity
    * }}}
    */
  private def refinementSummary(
      store: DependentPatternTermStore,
      result: DependentPatternUnifyResult,
  ): String =
    if result.impossible then return "impossible"
    if result.diagnostics.nonEmpty then return result.firstCode

    solutionSummary(store, result, "n")
      .orElse(solutionSummary(store, result, "A"))
      .getOrElse("identity")

  /** Extracts a named substitution for human-readable branch summaries.
    *
    * Rules:
    *
    * {{{
    * unify S(n) with S(k)
    * solutionSummary(result, "n") == Some("n=k")
    * }}}
    */
  private def solutionSummary(
      store: DependentPatternTermStore,
      result: DependentPatternUnifyResult,
      name: String,
  ): Option[String] =
    result.substitutions
      .get(name)
      .map(value => s"$name=${termDisplay(store, value)}")

  /** Specializes the displayed expected branch type with the refinement.
    *
    * Rules:
    *
    * {{{
    * xs : Vec(A, S(n))
    * Cons(head, tail) -> head
    *
    * expected branch type: A
    * specialized display: A under n=k
    * }}}
    */
  private def specializedExpectedType(
      expectedType: String,
      refinementSummary: String,
  ): String =
    refinementSummary match
      case "identity" | "" =>
        expectedType
      case "impossible" =>
        s"$expectedType under impossible"
      case other =>
        s"$expectedType under $other"

  private def renderCaseBranch(branch: DependentCaseTreeBranch): String =
    val binders = binderDisplay(branch.binders)
    if branch.impossible then s"${branch.constructor}$binders -> impossible"
    else
      s"${branch.constructor}$binders -> ${branch.body} [${branch.refinementSummary}]"

  private def binderDisplay(binders: List[String]): String =
    if binders.isEmpty then ""
    else binders.mkString("(", ", ", ")")

  private def emptySpan: SourceSpan =
    SourceFile("<dependent-pattern>", "").span(0, 0)

private final class DependentPatternUnifier(
    store: DependentPatternTermStore,
    span: SourceSpan,
):
  private val substitutions = mutable.LinkedHashMap.empty[String, Int]
  private val diagnostics = ListBuffer.empty[DependentPatternDiagnostic]
  private var impossible = false
  private var failed = false

  /** Dispatches the first-order index unification rule by the left term head.
    *
    * Rules:
    *
    * {{{
    * unify(n, k)       // records n := k
    * unify(S(n), S(k)) // recurses and records n := k
    * unify(S(n), Z)    // marks the branch impossible
    * }}}
    */
  def unify(left: Int, right: Int): Unit =
    if failed || impossible then return

    store.termValue(left) match
      case DependentPatternTerm.Var(name) =>
        unifyVariable(name, right)
      case DependentPatternTerm.Meta(index) =>
        unifyVariable(s"?m$index", right)
      case DependentPatternTerm.Constructor(name, args) =>
        unifyConstructorLike(left, name, args, right, family = false)
      case DependentPatternTerm.Family(name, args) =>
        unifyConstructorLike(left, name, args, right, family = true)
      case DependentPatternTerm.Error =>
        fail(
          "cosmo.type.dependent-pattern.unsupported-unification",
          "cannot unify an error index term",
          "<error>",
        )

  def fail(
      code: String,
      message: String,
      summary: String,
  ): Unit =
    failed = true
    addDiagnostic(code, message, summary)

  def finish(): DependentPatternUnifyResult =
    DependentPatternUnifyResult(
      ok = !failed && !impossible,
      impossible = impossible,
      substitutions = substitutions.toMap,
      diagnostics = diagnostics.toList,
    )

  /** Solves an index variable or reuses the existing solution.
    *
    * Rules:
    *
    * {{{
    * n = k     // stores n := k
    * n = S(n)  // rejected by occurs check
    * n = n     // accepted without adding a substitution
    * }}}
    */
  private def unifyVariable(name: String, value: Int): Unit =
    substitutions.get(name) match
      case Some(solution) =>
        unify(solution, value)
      case None =>
        if termIsSameVariable(name, value) then return
        if occurs(name, value) then
          fail(
            "cosmo.type.dependent-pattern.occurs-check",
            "index metavariable occurs in its own solution",
            s"$name in ${DependentPatterns.termDisplay(store, value)}",
          )
        else substitutions.update(name, value)

  /** Unifies a constructor or family head against the right-hand term.
    *
    * Rules:
    *
    * {{{
    * S(n) ~ S(k)             // same constructor head, recurse on args
    * Vec(A, S(n)) ~ Vec(A,k) // same family head, recurse on indices
    * S(n) ~ Z                // different constructor head, impossible
    * }}}
    */
  private def unifyConstructorLike(
      left: Int,
      name: String,
      args: List[Int],
      right: Int,
      family: Boolean,
  ): Unit =
    store.termValue(right) match
      case DependentPatternTerm.Var(rightName) =>
        unifyVariable(rightName, left)
      case DependentPatternTerm.Meta(index) =>
        unifyVariable(s"?m$index", left)
      case DependentPatternTerm.Constructor(rightName, rightArgs) =>
        unifySameHead(
          name,
          args,
          rightName,
          rightArgs,
          family,
          rightFamily = false,
        )
      case DependentPatternTerm.Family(rightName, rightArgs) =>
        unifySameHead(
          name,
          args,
          rightName,
          rightArgs,
          family,
          rightFamily = true,
        )
      case DependentPatternTerm.Error =>
        fail(
          "cosmo.type.dependent-pattern.unsupported-unification",
          "cannot unify an error index term",
          "<error>",
        )

  /** Applies the same-head unification rule for constructors and families.
    *
    * Rules:
    *
    * {{{
    * Vec(A, S(n)) ~ Vec(A, S(k))
    * // heads and arities match, then A ~ A and S(n) ~ S(k)
    * }}}
    */
  private def unifySameHead(
      leftName: String,
      leftArgs: List[Int],
      rightName: String,
      rightArgs: List[Int],
      leftFamily: Boolean,
      rightFamily: Boolean,
  ): Unit =
    if leftFamily != rightFamily || leftName != rightName then
      markImpossible(s"$leftName cannot refine $rightName")
      return

    if leftArgs.length != rightArgs.length then
      fail(
        "cosmo.type.dependent-pattern.unsupported-unification",
        "constructor index arity mismatch",
        s"$leftName/$rightName",
      )
      return

    leftArgs.zip(rightArgs).foreach { case (left, right) =>
      unify(left, right)
    }

  /** Recognizes the reflexive variable unification case.
    *
    * Rules:
    *
    * {{{
    * unify(n, n)    // no substitution is needed
    * unify(?m0, ?m0) // no substitution is needed
    * }}}
    */
  private def termIsSameVariable(name: String, value: Int): Boolean =
    store.termValue(value) match
      case DependentPatternTerm.Var(valueName) =>
        name == valueName
      case DependentPatternTerm.Meta(index) =>
        name == s"?m$index"
      case _: DependentPatternTerm.Constructor =>
        false
      case _: DependentPatternTerm.Family =>
        false
      case DependentPatternTerm.Error =>
        false

  /** Detects whether a candidate variable solution would be recursive.
    *
    * Rules:
    *
    * {{{
    * ?m = Vec(A, ?m)  // rejected: ?m occurs in its own solution
    * }}}
    */
  private def occurs(name: String, value: Int): Boolean =
    store.termValue(value) match
      case DependentPatternTerm.Var(valueName) =>
        name == valueName
      case DependentPatternTerm.Meta(index) =>
        name == s"?m$index"
      case DependentPatternTerm.Constructor(_, args) =>
        args.exists(occurs(name, _))
      case DependentPatternTerm.Family(_, args) =>
        args.exists(occurs(name, _))
      case DependentPatternTerm.Error =>
        false

  /** Records that the current constructor branch cannot refine the scrutinee.
    *
    * Rules:
    *
    * {{{
    * case xs : Vec(A, S(n)) of
    *   Nil -> absurd
    *
    * // Nil has index Vec(A, Z), and S(n) cannot refine Z.
    * }}}
    */
  private def markImpossible(summary: String): Unit =
    impossible = true
    addDiagnostic(
      "cosmo.type.dependent-pattern.impossible-branch",
      "constructor result indices cannot refine scrutinee indices",
      summary,
    )

  private def addDiagnostic(
      code: String,
      message: String,
      summary: String,
  ): Unit =
    diagnostics += DependentPatternDiagnostic(code, message, span, summary)
