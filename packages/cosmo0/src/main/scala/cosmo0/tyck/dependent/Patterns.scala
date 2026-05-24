package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

object DependentPatterns:
  private val UnsupportedUnificationCode =
    "cosmo.type.dependent-pattern.unsupported-unification"
  private val ImpossibleBranchCode =
    "cosmo.type.dependent-pattern.impossible-branch"

  def termStore(): DependentPatternTermStore =
    DependentPatternTermStore()

  def sourcePatternStore(): DependentSourcePatternStore =
    DependentSourcePatternStore()

  def env(): DependentPatternEnv =
    DependentPatternEnv()

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

  private def refinementSummary(
      store: DependentPatternTermStore,
      result: DependentPatternUnifyResult,
  ): String =
    if result.impossible then return "impossible"
    if result.diagnostics.nonEmpty then return result.firstCode

    solutionSummary(store, result, "n")
      .orElse(solutionSummary(store, result, "A"))
      .getOrElse("identity")

  private def solutionSummary(
      store: DependentPatternTermStore,
      result: DependentPatternUnifyResult,
      name: String,
  ): Option[String] =
    result.substitutions
      .get(name)
      .map(value => s"$name=${termDisplay(store, value)}")

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
