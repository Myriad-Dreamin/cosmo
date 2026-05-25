package cosmo0

import scala.collection.mutable.ListBuffer

private[cosmo0] final case class ProfileDirective(
    name: String,
    span: SourceSpan,
)

/** Extracts profile assertion directives from profile-specific source.
  *
  * Accepted forms:
  *
  * {{{
  * mltt: lambda-checks-pi
  * // mltt: lambda-checks-pi
  * dependent-pattern: vec-head-elaborates
  * }}}
  */
private[cosmo0] object ProfileDirectiveParser:
  def extract(
      sources: List[SourceFile],
      prefix: String,
  ): List[ProfileDirective] =
    sources.flatMap(extract(_, prefix))

  private def extract(
      source: SourceFile,
      prefix: String,
  ): List[ProfileDirective] =
    val directives = ListBuffer.empty[ProfileDirective]
    var offset = 0

    source.text.linesIterator.foreach { line =>
      val lineStart = offset
      val lineEnd = lineStart + line.length
      directiveName(line, prefix).foreach { name =>
        directives += ProfileDirective(name, source.span(lineStart, lineEnd))
      }
      offset = lineEnd + 1
    }

    directives.toList

  private def directiveName(line: String, prefix: String): Option[String] =
    val trimmed = line.trim
    val body =
      if trimmed.startsWith("//") then trimmed.dropWhile(_ == '/').trim
      else trimmed
    if !body.startsWith(prefix) then return None

    val name = body.drop(prefix.length).trim
    if name.nonEmpty then Some(name) else None

/** Source adapter for `checkerProfile: "mltt.core"`.
  *
  * The profile source names MLTT capabilities to assert. Each assertion builds
  * a concrete core term and calls `MlttTypeChecker`, so package-level fixtures
  * fail when the checker stops accepting the advertised MLTT fragment.
  */
private[cosmo0] object MlttProfileChecker:
  private val DirectivePrefix = "mltt:"
  private val NoAssertionsCode = "cosmo.type.mltt.no-profile-assertions"
  private val UnknownAssertionCode = "cosmo.type.mltt.unknown-profile-assertion"
  private val AssertionFailedCode = "cosmo.type.mltt.profile-assertion-failed"

  def checkSource(source: SourceFile): Result[TypedModule] =
    checkSources(List(source), source.name)

  def checkSources(
      sources: List[SourceFile],
      moduleName: String,
  ): Result[TypedModule] =
    val directives = ProfileDirectiveParser.extract(sources, DirectivePrefix)
    if directives.isEmpty then
      return Result.failure(
        Phase.Check,
        List(
          diagnostic(
            NoAssertionsCode,
            "mltt.core profile source did not declare any MLTT assertions",
            wholeSourceSpan(sources),
          ),
        ),
      )

    val diagnostics = directives.flatMap(runAssertion)
    if diagnostics.nonEmpty then return Result.failure(Phase.Check, diagnostics)

    Result.success(
      Phase.Check,
      syntheticModule(moduleName, directives, "mltt_core"),
    )

  private def runAssertion(
      directive: ProfileDirective,
  ): List[Diagnostic] =
    directive.name match
      case "lambda-checks-pi" =>
        lambdaChecksPi(directive.span)
      case "application-infers-through-pi" =>
        applicationInfersThroughPi(directive.span)
      case "sigma-pair-checks" =>
        sigmaPairChecks(directive.span)
      case "equality-refl-checks" =>
        equalityReflChecks(directive.span)
      case "conversion-beta-let" =>
        conversionBetaLet(directive.span)
      case "vec-constructors-check" =>
        vecConstructorsCheck(directive.span)
      case other =>
        List(
          diagnostic(
            UnknownAssertionCode,
            s"unknown mltt.core assertion $other",
            directive.span,
          ),
        )

  private def lambdaChecksPi(span: SourceSpan): List[Diagnostic] =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val body = store.allocVar("x")
    val lambda = store.allocLambda("x", a, body)
    val expected = store.allocPi("x", a, a)
    val context = MlttTypeChecker.context().extendLocal("A", type0)
    val checked = MlttTypeChecker.check(store, context, lambda, expected)
    val expectedSummary =
      "mltt.core|mltt-core-term|accepted|mltt.whnf-conversion"

    mlttDiagnostics(checked.diagnostics, span) :::
      assertCondition(
        checked.isOk && checked.artifactSummary == expectedSummary,
        "lambda did not check against the expected Pi type",
        expectedSummary,
        checked.artifactSummary,
        span,
      )

  private def applicationInfersThroughPi(span: SourceSpan): List[Diagnostic] =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    var context = MlttTypeChecker.context().extendLocal("A", type0)
    context = context.extendLocal("y", a)

    val body = store.allocVar("x")
    val lambda = store.allocLambda("x", a, body)
    val y = store.allocVar("y")
    val apply = store.allocApply(lambda, y)
    val inferred = MlttTypeChecker.infer(store, context, apply)
    val actual = MlttTypeChecker.display(store, inferred.valueType)

    mlttDiagnostics(inferred.diagnostics, span) :::
      assertCondition(
        inferred.isOk && actual == "A",
        "application did not infer through Pi elimination",
        "A",
        actual,
        span,
      )

  private def sigmaPairChecks(span: SourceSpan): List[Diagnostic] =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val x = store.allocVar("x")
    val y = store.allocVar("y")
    var context = MlttTypeChecker.context().extendLocal("A", type0)
    context = context.extendLocal("x", a).extendLocal("y", a)

    val pair = store.allocPair(x, y)
    val sigma = store.allocSigma("first", a, a)
    val checked = MlttTypeChecker.check(store, context, pair, sigma)

    mlttDiagnostics(checked.diagnostics, span) :::
      assertCondition(
        checked.isOk,
        "pair did not check against the expected Sigma type",
        "accepted",
        checked.status,
        span,
      )

  private def equalityReflChecks(span: SourceSpan): List[Diagnostic] =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val x = store.allocVar("x")
    val context = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("x", a)

    val eq = store.allocEq(a, x, x)
    val refl = store.allocRefl(x)
    val checked = MlttTypeChecker.check(store, context, refl, eq)

    mlttDiagnostics(checked.diagnostics, span) :::
      assertCondition(
        checked.isOk,
        "Refl did not check against the expected equality type",
        "accepted",
        checked.status,
        span,
      )

  private def conversionBetaLet(span: SourceSpan): List[Diagnostic] =
    val store = MlttTypeChecker.termStore()
    val type0 = store.allocUniverse(0)
    val a = store.allocVar("A")
    val y = store.allocVar("y")
    val context = MlttTypeChecker
      .context()
      .extendLocal("A", type0)
      .extendLocal("y", a)

    val lambda = store.allocLambda("x", a, store.allocVar("x"))
    val beta = store.allocApply(lambda, y)
    val letTerm = store.allocLet("z", y, store.allocVar("z"))
    val betaResult = MlttTypeChecker.convert(store, context, beta, y)
    val letResult = MlttTypeChecker.convert(store, context, letTerm, y)

    mlttDiagnostics(betaResult.diagnostics, span) :::
      mlttDiagnostics(letResult.diagnostics, span) :::
      assertCondition(
        betaResult.isOk && letResult.isOk,
        "WHNF conversion did not accept beta and let reductions",
        "both conversions accepted",
        s"beta=${betaResult.isOk}, let=${letResult.isOk}",
        span,
      )

  private def vecConstructorsCheck(span: SourceSpan): List[Diagnostic] =
    val store = MlttTypeChecker.termStore()
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
    var context = MlttTypeChecker.context().extendLocal("A", type0)
    context = context
      .extendLocal("k", nat)
      .extendLocal("head", a)
      .extendLocal("tail", vecAK)

    val nilChecked = MlttTypeChecker.check(store, context, nil, vecAZ)
    val consChecked = MlttTypeChecker.check(store, context, cons, vecASK)

    mlttDiagnostics(nilChecked.diagnostics, span) :::
      mlttDiagnostics(consChecked.diagnostics, span) :::
      assertCondition(
        nilChecked.isOk && consChecked.isOk,
        "Vec constructors did not check against indexed Vec families",
        "Nil and Cons accepted",
        s"Nil=${nilChecked.isOk}, Cons=${consChecked.isOk}",
        span,
      )

  private def mlttDiagnostics(
      diagnostics: List[MlttDiagnostic],
      span: SourceSpan,
  ): List[Diagnostic] =
    diagnostics.map { diagnosticValue =>
      diagnostic(
        diagnosticValue.code,
        s"${diagnosticValue.message}; expected ${diagnosticValue.expected}, got ${diagnosticValue.actual}",
        diagnosticValue.span.getOrElse(span),
      )
    }

  private def assertCondition(
      condition: Boolean,
      message: String,
      expected: String,
      actual: String,
      span: SourceSpan,
  ): List[Diagnostic] =
    if condition then Nil
    else
      List(
        diagnostic(
          AssertionFailedCode,
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
      case None         => SourceFile("<mltt-profile>", "").span(0, 0)

  private def diagnostic(
      code: String,
      message: String,
      span: SourceSpan,
  ): Diagnostic =
    Diagnostic(Phase.Check, DiagnosticSeverity.Error, code, message, Some(span))

/** Source adapter for `checkerProfile: "mltt.dependent-patterns"`.
  *
  * Assertions construct dependent-pattern fixtures and call `DependentPatterns`
  * so the profile verifies case-tree elaboration and impossible-index
  * diagnostics through the package checker path.
  */
private[cosmo0] object DependentPatternProfileChecker:
  private val DirectivePrefix = "dependent-pattern:"
  private val NoAssertionsCode =
    "cosmo.type.dependent-pattern.no-profile-assertions"
  private val UnknownAssertionCode =
    "cosmo.type.dependent-pattern.unknown-profile-assertion"
  private val AssertionFailedCode =
    "cosmo.type.dependent-pattern.profile-assertion-failed"

  def checkSource(source: SourceFile): Result[TypedModule] =
    checkSources(List(source), source.name)

  def checkSources(
      sources: List[SourceFile],
      moduleName: String,
  ): Result[TypedModule] =
    val directives = ProfileDirectiveParser.extract(sources, DirectivePrefix)
    if directives.isEmpty then
      return Result.failure(
        Phase.Check,
        List(
          diagnostic(
            NoAssertionsCode,
            "mltt.dependent-patterns profile source did not declare any dependent-pattern assertions",
            wholeSourceSpan(sources),
          ),
        ),
      )

    val diagnostics = directives.flatMap(runAssertion)
    if diagnostics.nonEmpty then return Result.failure(Phase.Check, diagnostics)

    Result.success(
      Phase.Check,
      syntheticModule(moduleName, directives, "dependent_pattern"),
    )

  private def runAssertion(
      directive: ProfileDirective,
  ): List[Diagnostic] =
    directive.name match
      case "vec-head-elaborates" =>
        vecHeadElaborates(directive.span)
      case "impossible-nil-diagnostic" =>
        impossibleNilDiagnostic(directive.span)
      case other =>
        List(
          diagnostic(
            UnknownAssertionCode,
            s"unknown dependent-pattern assertion $other",
            directive.span,
          ),
        )

  private def vecHeadElaborates(span: SourceSpan): List[Diagnostic] =
    val tree = vecHeadTree(CheckerProfiles.MlttDependentPatterns, span)
    val rendered = DependentPatterns.renderCaseTree(tree)
    val expected = "case xs : Vec(A, S(n)) of\n  Cons(head, tail) -> head [n=k]"

    dependentDiagnostics(tree.diagnostics, span) :::
      assertCondition(
        tree.isOk &&
          tree.branches.length == 1 &&
          tree.branches.head.constructor == "Cons" &&
          rendered == expected,
        "Vec head pattern did not elaborate to the expected case tree",
        expected,
        rendered,
        span,
      )

  private def impossibleNilDiagnostic(span: SourceSpan): List[Diagnostic] =
    val store = DependentPatterns.termStore()
    val env = DependentPatterns.env()
    val patterns = DependentPatterns.sourcePatternStore()
    DependentPatterns.addNatFixture(store, env)
    DependentPatterns.addVecFixture(store, env)

    val n = store.allocVar("n")
    val sN = DependentPatterns.natSuccessor(store, n)
    val scrutineeIndices = DependentPatterns.vecIndices(store, sN)
    val nil = patterns.allocConstructor("Nil", Nil, span)
    val clauses = List(DependentPatternClause(nil, "absurd", span))
    val tree =
      DependentPatterns.elaborateClauses(
        CheckerProfiles.MlttDependentPatterns,
        env,
        store,
        patterns,
        "Vec",
        "xs",
        scrutineeIndices,
        "A",
        clauses,
      )

    assertCondition(
      !tree.isOk &&
        tree.firstCode == "cosmo.type.dependent-pattern.impossible-branch" &&
        tree.branches.headOption.exists(_.impossible),
      "Nil branch did not report impossible indices for Vec(A, S(n))",
      "cosmo.type.dependent-pattern.impossible-branch",
      tree.firstCode,
      span,
    )

  private def vecHeadTree(
      profile: CheckerProfile,
      span: SourceSpan,
  ): DependentCaseTree =
    val store = DependentPatterns.termStore()
    val env = DependentPatterns.env()
    val patterns = DependentPatterns.sourcePatternStore()
    DependentPatterns.addNatFixture(store, env)
    DependentPatterns.addVecFixture(store, env)

    val n = store.allocVar("n")
    val sN = DependentPatterns.natSuccessor(store, n)
    val scrutineeIndices = DependentPatterns.vecIndices(store, sN)
    val headPat = patterns.allocVariable("head", span)
    val tailPat = patterns.allocVariable("tail", span)
    val cons = patterns.allocConstructor("Cons", List(headPat, tailPat), span)
    val clauses = List(DependentPatternClause(cons, "head", span))
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

  private def dependentDiagnostics(
      diagnostics: List[DependentPatternDiagnostic],
      span: SourceSpan,
  ): List[Diagnostic] =
    diagnostics.map { diagnosticValue =>
      diagnostic(
        diagnosticValue.code,
        s"${diagnosticValue.message}; ${diagnosticValue.summary}",
        diagnosticValue.span,
      )
    }

  private def assertCondition(
      condition: Boolean,
      message: String,
      expected: String,
      actual: String,
      span: SourceSpan,
  ): List[Diagnostic] =
    if condition then Nil
    else
      List(
        diagnostic(
          AssertionFailedCode,
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
      case None => SourceFile("<dependent-pattern-profile>", "").span(0, 0)

  private def diagnostic(
      code: String,
      message: String,
      span: SourceSpan,
  ): Diagnostic =
    Diagnostic(Phase.Check, DiagnosticSeverity.Error, code, message, Some(span))
