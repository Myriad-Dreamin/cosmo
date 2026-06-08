package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import cosmo.syntax.*

final case class Cosmo0CteFunctionCallGraphEntry(
    name: String,
    args: List[BigInt] = Nil,
)

final case class Cosmo0CteFunctionCallGraphOptions(
    enabled: Boolean,
    entries: List[Cosmo0CteFunctionCallGraphEntry] = Nil,
    toolchainIdentity: Option[CosmoEvalToolchainIdentity] = None,
    target: CosmoEvalTargetSettings = CosmoEval.defaultTarget,
    imports: List[String] = Nil,
    headers: List[String] = Nil,
    includePaths: List[String] = Nil,
    librarySearchPaths: List[String] = Nil,
    compileOptions: List[String] = Nil,
    supportLibraryIdentities: List[String] = Nil,
    backend: String = CosmoEval.CompiledProviderBackend,
    recursionStepLimit: Int = 64,
)

trait Cosmo0CteFunctionCallGraphRunner:
  def evaluate(
      request: CosmoEvalRequest,
  ): Cosmo0CteCompileProbeEvalOutcome

final case class Cosmo0CteCallable(
    id: String,
    name: String,
    params: List[String],
    span: SourceSpan,
)

final case class Cosmo0CteCallSite(
    id: String,
    caller: String,
    callee: String,
    span: SourceSpan,
)

final case class Cosmo0CteCallableArtifact(
    id: String,
    callableNames: List[String],
    recursive: Boolean,
    generatedSource: String,
)

final case class Cosmo0CteFunctionCallGraphPlan(
    callables: List[Cosmo0CteCallable],
    callSites: List[Cosmo0CteCallSite],
    artifacts: List[Cosmo0CteCallableArtifact],
    callEdges: Map[String, List[String]],
    artifactByCallable: Map[String, String],
    callableCheckCounts: Map[String, Int],
):
  def stableSummary: String =
    val callableSummary =
      callables.map(value => s"${value.id}:${value.params.mkString(",")}")
    val callSiteSummary =
      callSites.map(value => s"${value.id}:${value.caller}->${value.callee}")
    val artifactSummary =
      artifacts.map(value =>
        s"${value.id}:${value.callableNames.mkString("+")}:${value.recursive}",
      )
    (callableSummary ::: callSiteSummary ::: artifactSummary).mkString("|")

  def callableCheckCount(name: String): Int =
    callableCheckCounts.getOrElse(name, 0)

  def recursiveArtifactsForEntry(
      entry: Cosmo0CteFunctionCallGraphEntry,
  ): List[Cosmo0CteCallableArtifact] =
    val reachableArtifacts = reachableArtifactIds(entry.name)
    artifacts.filter(artifact =>
      artifact.recursive && reachableArtifacts.contains(artifact.id),
    )

  def reachableArtifactIds(name: String): Set[String] =
    reachableCallableNames(name).flatMap(artifactByCallable.get)

  def reachableCallableNames(name: String): Set[String] =
    val seen = mutable.LinkedHashSet.empty[String]
    def visit(current: String): Unit =
      if !seen.contains(current) then
        seen += current
        callEdges.getOrElse(current, Nil).foreach(visit)
    visit(name)
    seen.toSet

final case class Cosmo0CteFunctionCallGraphResult(
    entryId: String,
    entry: Cosmo0CteFunctionCallGraphEntry,
    value: BigInt,
    request: CosmoEvalRequest,
    evalResult: CosmoEvalResult,
)

final case class Cosmo0CteFunctionCallGraphModule(
    source: SourceFile,
    plan: Cosmo0CteFunctionCallGraphPlan,
    results: List[Cosmo0CteFunctionCallGraphResult],
):
  def resultValue(name: String): Option[BigInt] =
    results.find(_.entry.name == name).map(_.value)

object Cosmo0CteFunctionCallGraph:
  val UnresolvedCallCode = "cosmo0.cte-function-call-graph.unresolved-call"
  val UnsupportedCallShapeCode =
    "cosmo0.cte-function-call-graph.unsupported-call-shape"
  val UnavailableCode = "cosmo0.cte-function-call-graph.unavailable"
  val CompileFailedCode = "cosmo0.cte-function-call-graph.compile-failed"
  val ExecutionFailedCode = "cosmo0.cte-function-call-graph.execution-failed"
  val RecursionBoundCode =
    "cosmo0.cte-function-call-graph.recursion-bound-exceeded"

  def check(
      sourceText: String,
      options: Cosmo0CteFunctionCallGraphOptions,
      runner: Cosmo0CteFunctionCallGraphRunner,
  ): Result[Cosmo0CteFunctionCallGraphModule] =
    check(SourceFile("<memory>", sourceText), options, runner)

  def check(
      source: SourceFile,
      options: Cosmo0CteFunctionCallGraphOptions,
      runner: Cosmo0CteFunctionCallGraphRunner,
  ): Result[Cosmo0CteFunctionCallGraphModule] =
    if !options.enabled then return disabledResult(source)

    options.toolchainIdentity match
      case None =>
        Result.failure(
          Phase.Check,
          List(
            diagnostic(
              UnavailableCode,
              "cosmo0 compile-time callable graph validation is enabled, but eval mode has no configured toolchain identity",
              source.span(0, source.text.length),
            ),
          ),
        )
      case Some(toolchainIdentity) =>
        parse(source) match
          case parsed if parsed.isSuccess =>
            checkParsed(
              parsed.value.get,
              options,
              toolchainIdentity,
              runner,
            )
          case failed =>
            Result.failure(Phase.Check, failed.diagnostics)

  private def disabledResult(
      source: SourceFile,
  ): Result[Cosmo0CteFunctionCallGraphModule] =
    val ordinary = Cosmo0().check(source)
    if ordinary.isSuccess then
      Result.success(
        Phase.Check,
        Cosmo0CteFunctionCallGraphModule(source, emptyPlan, Nil),
      )
    else Result(Phase.Check, ordinary.status, None, ordinary.diagnostics)

  private def checkParsed(
      parsed: ParsedModule,
      options: Cosmo0CteFunctionCallGraphOptions,
      toolchainIdentity: CosmoEvalToolchainIdentity,
      runner: Cosmo0CteFunctionCallGraphRunner,
  ): Result[Cosmo0CteFunctionCallGraphModule] =
    val planner = Planner(parsed)
    planner.plan() match
      case Left(diagnostics) =>
        Result.failure(Phase.Check, diagnostics)
      case Right(plan) =>
        executeEntries(parsed.source, plan, options, toolchainIdentity, runner)

  private def executeEntries(
      source: SourceFile,
      plan: Cosmo0CteFunctionCallGraphPlan,
      options: Cosmo0CteFunctionCallGraphOptions,
      toolchainIdentity: CosmoEvalToolchainIdentity,
      runner: Cosmo0CteFunctionCallGraphRunner,
  ): Result[Cosmo0CteFunctionCallGraphModule] =
    val diagnostics = ListBuffer.empty[Diagnostic]
    val results = ListBuffer.empty[Cosmo0CteFunctionCallGraphResult]

    options.entries.zipWithIndex.foreach { case (entry, index) =>
      entrySpan(plan, entry, source) match
        case None =>
          diagnostics += diagnostic(
            UnresolvedCallCode,
            s"compile-time entry ${entry.name} does not resolve to a callable header",
            source.span(0, source.text.length),
          )
        case Some(span) if recursionBoundExceeded(plan, entry, options) =>
          diagnostics += diagnostic(
            RecursionBoundCode,
            s"recursive compile-time entry ${entry.name} exceeded validation bound ${options.recursionStepLimit}",
            span,
          )
        case Some(span) =>
          val request = entryRequest(
            source,
            plan,
            entry,
            index,
            options,
            toolchainIdentity,
          )
          consumeEvalResult(
            entry,
            index,
            span,
            request,
            runner.evaluate(request),
            diagnostics,
            results,
          )
    }

    if diagnostics.nonEmpty then Result.failure(Phase.Check, diagnostics.toList)
    else
      Result.success(
        Phase.Check,
        Cosmo0CteFunctionCallGraphModule(source, plan, results.toList),
      )

  private def entrySpan(
      plan: Cosmo0CteFunctionCallGraphPlan,
      entry: Cosmo0CteFunctionCallGraphEntry,
      source: SourceFile,
  ): Option[SourceSpan] =
    plan.callables
      .find(_.name == entry.name)
      .map(_.span)
      .orElse(Some(source.span(0, source.text.length)))
      .filter(_ => plan.callableCheckCounts.contains(entry.name))

  private def recursionBoundExceeded(
      plan: Cosmo0CteFunctionCallGraphPlan,
      entry: Cosmo0CteFunctionCallGraphEntry,
      options: Cosmo0CteFunctionCallGraphOptions,
  ): Boolean =
    options.recursionStepLimit <= 0 &&
      plan.recursiveArtifactsForEntry(entry).nonEmpty

  private def entryRequest(
      source: SourceFile,
      plan: Cosmo0CteFunctionCallGraphPlan,
      entry: Cosmo0CteFunctionCallGraphEntry,
      index: Int,
      options: Cosmo0CteFunctionCallGraphOptions,
      toolchainIdentity: CosmoEvalToolchainIdentity,
  ): CosmoEvalRequest =
    val reachableArtifacts =
      plan.reachableArtifactIds(entry.name)
    val artifactInputs =
      plan.artifacts.filter(artifact =>
        reachableArtifacts.contains(artifact.id),
      )
    val providerSource =
      providerEntrySource(plan, artifactInputs, entry)

    CosmoEval.request(
      compilerId = "cosmo0",
      evalIdentity =
        s"cosmo0.cte-function-call-graph:${source.name}:${entry.name}:$index",
      providerIdentity =
        artifactInputs.map(_.id).mkString("cosmo0.cte.artifacts[", ",", "]"),
      serializedInput = serializedEntryInput(source, entry, options),
      imports = options.imports,
      headers = options.headers,
      includePaths = options.includePaths,
      librarySearchPaths = options.librarySearchPaths,
      compileOptions = options.compileOptions,
      generatedEntrySource = providerSource,
      target = options.target,
      supportLibraryIdentities = options.supportLibraryIdentities,
      toolchainIdentity = toolchainIdentity,
      requestedFacts =
        List(s"entry:${entry.name}") ::: artifactInputs.map(_.id),
      backend = options.backend,
    )

  private def providerEntrySource(
      plan: Cosmo0CteFunctionCallGraphPlan,
      artifacts: List[Cosmo0CteCallableArtifact],
      entry: Cosmo0CteFunctionCallGraphEntry,
  ): String =
    val reachableNames = plan.reachableCallableNames(entry.name)
    val prototypes =
      plan.callables
        .filter(callable => reachableNames.contains(callable.name))
        .map(callable =>
          s"static int ${callable.name}(${cppParams(callable.params)});",
        )
        .mkString("\n")
    val definitions = artifacts.map(_.generatedSource).mkString("\n\n")
    val args = entry.args.map(_.toString).mkString(", ")
    s"""$prototypes
       |
       |$definitions
       |
       |extern "C" int cosmo_eval_entry() {
       |  return ${entry.name}($args);
       |}
       |""".stripMargin

  private def serializedEntryInput(
      source: SourceFile,
      entry: Cosmo0CteFunctionCallGraphEntry,
      options: Cosmo0CteFunctionCallGraphOptions,
  ): String =
    CosmoEvalStableJson.obj(
      List(
        "sourceIdentity" -> CosmoEvalStableJson.string(source.name),
        "entryIdentity" -> CosmoEvalStableJson.string(entry.name),
        "argumentPayload" -> CosmoEvalStableJson.stringArray(
          entry.args.map(_.toString),
        ),
        "recursionStepLimit" -> CosmoEvalStableJson.number(
          options.recursionStepLimit,
        ),
      ),
    )

  private def consumeEvalResult(
      entry: Cosmo0CteFunctionCallGraphEntry,
      index: Int,
      span: SourceSpan,
      request: CosmoEvalRequest,
      outcome: Cosmo0CteCompileProbeEvalOutcome,
      diagnostics: ListBuffer[Diagnostic],
      results: ListBuffer[Cosmo0CteFunctionCallGraphResult],
  ): Unit =
    outcome match
      case Cosmo0CteCompileProbeEvalOutcome.Unavailable(message) =>
        diagnostics += diagnostic(UnavailableCode, message, span)
      case Cosmo0CteCompileProbeEvalOutcome.Completed(result) =>
        consumeCompletedEvalResult(
          entry,
          index,
          span,
          request,
          result,
          diagnostics,
          results,
        )

  private def consumeCompletedEvalResult(
      entry: Cosmo0CteFunctionCallGraphEntry,
      index: Int,
      span: SourceSpan,
      request: CosmoEvalRequest,
      result: CosmoEvalResult,
      diagnostics: ListBuffer[Diagnostic],
      results: ListBuffer[Cosmo0CteFunctionCallGraphResult],
  ): Unit =
    result.status match
      case CosmoEvalStatus.Succeeded =>
        result.serializedOutput.flatMap(integerOutput) match
          case Some(value) =>
            results += Cosmo0CteFunctionCallGraphResult(
              s"cte.entry.$index.${entry.name}",
              entry,
              value,
              request,
              result,
            )
          case None =>
            diagnostics += diagnostic(
              ExecutionFailedCode,
              s"cosmo0 eval returned a successful callable graph result without an integer output; ${diagnosticSummary(result)}",
              span,
            )
      case CosmoEvalStatus.Unsupported =>
        diagnostics += diagnostic(
          UnavailableCode,
          s"cosmo0 eval is unavailable for the callable graph request; ${diagnosticSummary(result)}",
          span,
        )
      case CosmoEvalStatus.Failed =>
        val code = failedResultCode(result)
        diagnostics += diagnostic(
          code,
          s"${failedResultMessage(code)}; ${diagnosticSummary(result)}",
          span,
        )

  private def integerOutput(value: String): Option[BigInt] =
    val trimmed = value.trim
    val unsignedDigits = trimmed.forall(_.isDigit)
    val signedDigits = startsWithSign(trimmed) && hasDigits(trimmed.drop(1))

    if trimmed.isEmpty then None
    else if unsignedDigits then Some(BigInt(trimmed))
    else if signedDigits then Some(BigInt(trimmed))
    else None

  private def startsWithSign(value: String): Boolean =
    value.startsWith("+") || value.startsWith("-")

  private def hasDigits(value: String): Boolean =
    value.nonEmpty && value.forall(_.isDigit)

  private def failedResultCode(result: CosmoEvalResult): String =
    val codes = result.diagnostics.map(_.code)
    if codes.exists(_.contains("recursion")) || codes.exists(
        _.contains("bound"),
      )
    then RecursionBoundCode
    else if codes.exists(_.contains("compile")) then CompileFailedCode
    else ExecutionFailedCode

  private def failedResultMessage(code: String): String =
    code match
      case RecursionBoundCode =>
        "recursive compile-time callable execution exceeded validation bounds"
      case CompileFailedCode =>
        "compile-time callable provider entry compile failed"
      case _ =>
        "compile-time callable provider entry execution failed"

  private def diagnosticSummary(result: CosmoEvalResult): String =
    if result.diagnostics.isEmpty then s"eval status ${result.status.id}"
    else
      result.diagnostics
        .map(diagnostic =>
          s"${diagnostic.code}: ${publicDiagnosticMessage(diagnostic.message)}",
        )
        .mkString("; ")

  private def publicDiagnosticMessage(message: String): String =
    message.replace("clang::", "clang").replace("llvm::", "llvm")

  private def parse(source: SourceFile): Result[ParsedModule] =
    Cosmo0().parse(source)

  private def diagnostic(
      code: String,
      message: String,
      span: SourceSpan,
  ): Diagnostic =
    Diagnostic(Phase.Check, DiagnosticSeverity.Error, code, message, Some(span))

  private val emptyPlan =
    Cosmo0CteFunctionCallGraphPlan(
      Nil,
      Nil,
      Nil,
      Map.empty,
      Map.empty,
      Map.empty,
    )

  private final case class CallableRecord(
      public: Cosmo0CteCallable,
      node: Def,
      body: Node,
      sourceOrder: Int,
  )

  private final case class PlanContext(
      source: SourceFile,
      callables: Map[String, CallableRecord],
  )

  private final class Planner(parsed: ParsedModule):
    private val source = parsed.source

    def plan(): Either[List[Diagnostic], Cosmo0CteFunctionCallGraphPlan] =
      val diagnostics = ListBuffer.empty[Diagnostic]
      val callables = collectCallables(diagnostics)
      val context = PlanContext(source, callables)
      val callSites = collectCallSites(context, diagnostics)

      if diagnostics.nonEmpty then return Left(diagnostics.toList)

      val edges = callEdges(callables, callSites)
      val sccs = stronglyConnectedComponents(callables.keys.toList, edges)
      val artifactData = artifacts(context, sccs, edges)
      val schedule = scheduleArtifacts(artifactData, edges)
      val artifactByCallable =
        schedule
          .flatMap(artifact =>
            artifact.callableNames.map(name => name -> artifact.id),
          )
          .toMap

      Right(
        Cosmo0CteFunctionCallGraphPlan(
          callables.values.toList
            .sortBy(_.public.name)
            .map(_.public),
          callSites.sortBy(_.id),
          schedule,
          edges.view.mapValues(_.sorted).toMap,
          artifactByCallable,
          callables.keys.map(_ -> 1).toMap,
        ),
      )

    private def collectCallables(
        diagnostics: ListBuffer[Diagnostic],
    ): Map[String, CallableRecord] =
      val values = mutable.LinkedHashMap.empty[String, CallableRecord]
      parsed.ast.stmts.zipWithIndex.foreach { case (statement, index) =>
        unwrapSemi(statement) match
          case Some(node: Def) if node.rhs.nonEmpty =>
            if values.contains(node.name.name) then
              diagnostics += diagnostic(
                UnsupportedCallShapeCode,
                s"compile-time callable ${node.name.name} is declared more than once",
                nodeSpan(node),
              )
            else values.update(node.name.name, callableRecord(node, index))
          case _ =>
      }
      values.toMap

    private def callableRecord(node: Def, sourceOrder: Int): CallableRecord =
      val params = node.params.getOrElse(Nil).map(_.name.name)
      val public = Cosmo0CteCallable(
        s"cte.callable.${node.name.name}",
        node.name.name,
        params,
        nodeSpan(node),
      )
      CallableRecord(public, node, node.rhs.get, sourceOrder)

    private def collectCallSites(
        context: PlanContext,
        diagnostics: ListBuffer[Diagnostic],
    ): List[Cosmo0CteCallSite] =
      val callSites = ListBuffer.empty[Cosmo0CteCallSite]
      context.callables.values.toList
        .sortBy(_.sourceOrder)
        .foreach { callable =>
          var callIndex = 0
          collectCallSitesInExpr(
            callable.public.name,
            callable.body,
            context,
            diagnostics,
            callSites,
            () =>
              val index = callIndex
              callIndex += 1
              index,
          )
        }
      callSites.toList

    private def collectCallSitesInExpr(
        caller: String,
        node: Node,
        context: PlanContext,
        diagnostics: ListBuffer[Diagnostic],
        callSites: ListBuffer[Cosmo0CteCallSite],
        nextIndex: () => Int,
    ): Unit =
      node match
        case Apply(lhs, args, false) =>
          recordCallSite(
            caller,
            lhs,
            node,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
          args.foreach(
            collectCallSitesInExpr(
              caller,
              _,
              context,
              diagnostics,
              callSites,
              nextIndex,
            ),
          )
        case Apply(lhs, args, true) =>
          diagnostics += diagnostic(
            UnsupportedCallShapeCode,
            "compile-time callable graph validation does not support compile-time type application inside callable bodies",
            nodeSpan(lhs),
          )
          args.foreach(
            collectCallSitesInExpr(
              caller,
              _,
              context,
              diagnostics,
              callSites,
              nextIndex,
            ),
          )
        case BinOp(_, lhs, rhs) =>
          collectCallSitesInExpr(
            caller,
            lhs,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
          collectCallSitesInExpr(
            caller,
            rhs,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
        case UnOp(_, value) =>
          collectCallSitesInExpr(
            caller,
            value,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
        case If(cond, thenExp, elseExp) =>
          collectCallSitesInExpr(
            caller,
            cond,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
          collectCallSitesInExpr(
            caller,
            thenExp,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
          elseExp.foreach(
            collectCallSitesInExpr(
              caller,
              _,
              context,
              diagnostics,
              callSites,
              nextIndex,
            ),
          )
        case Block(stmts) =>
          stmts.foreach(
            collectCallSitesInExpr(
              caller,
              _,
              context,
              diagnostics,
              callSites,
              nextIndex,
            ),
          )
        case Semi(Some(value)) =>
          collectCallSitesInExpr(
            caller,
            value,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
        case Return(value) =>
          collectCallSitesInExpr(
            caller,
            value,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
        case Match(lhs, rhs) =>
          collectCallSitesInExpr(
            caller,
            lhs,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
          collectCallSitesInExpr(
            caller,
            rhs,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
        case Case(cond, body) =>
          collectCallSitesInExpr(
            caller,
            cond,
            context,
            diagnostics,
            callSites,
            nextIndex,
          )
          body.foreach(
            collectCallSitesInExpr(
              caller,
              _,
              context,
              diagnostics,
              callSites,
              nextIndex,
            ),
          )
        case _ =>

    private def recordCallSite(
        caller: String,
        lhs: Node,
        applyNode: Node,
        context: PlanContext,
        diagnostics: ListBuffer[Diagnostic],
        callSites: ListBuffer[Cosmo0CteCallSite],
        nextIndex: () => Int,
    ): Unit =
      callHead(lhs) match
        case Some(CallHead(name, span, true))
            if context.callables.contains(name) =>
          val index = nextIndex()
          callSites += Cosmo0CteCallSite(
            s"cte.callsite.$caller.$index.$name",
            caller,
            name,
            span,
          )
        case Some(CallHead(name, span, true)) =>
          diagnostics += diagnostic(
            UnresolvedCallCode,
            s"compile-time call head $name does not resolve to a callable header",
            span,
          )
        case Some(CallHead(name, span, false))
            if context.callables.contains(name) =>
          diagnostics += diagnostic(
            UnsupportedCallShapeCode,
            s"compile-time call ${callShape(lhs)} uses a suffix outside the callable graph validation profile",
            nodeSpan(applyNode),
          )
        case Some(CallHead(name, span, false)) =>
          diagnostics += diagnostic(
            UnresolvedCallCode,
            s"compile-time call head $name does not resolve to a callable header",
            span,
          )
        case None =>
          diagnostics += diagnostic(
            UnsupportedCallShapeCode,
            s"compile-time call ${callShape(lhs)} does not have an identifier call head",
            nodeSpan(applyNode),
          )

    private final case class CallHead(
        name: String,
        span: SourceSpan,
        suffixSupported: Boolean,
    )

    private def callHead(node: Node): Option[CallHead] =
      node match
        case ident: Ident =>
          Some(CallHead(ident.name, nodeSpan(ident), suffixSupported = true))
        case Select(lhs, _, _) =>
          callHead(lhs).map(value => value.copy(suffixSupported = false))
        case _ =>
          None

    private def callShape(node: Node): String =
      node match
        case Ident(name) => name
        case Select(lhs, rhs, _) =>
          s"${callShape(lhs)}.${rhs.name}"
        case other =>
          other.getClass.getSimpleName.stripSuffix("$")

    private def callEdges(
        callables: Map[String, CallableRecord],
        callSites: List[Cosmo0CteCallSite],
    ): Map[String, List[String]] =
      val grouped = callSites.groupBy(_.caller).view.mapValues(_.map(_.callee))
      callables.keys
        .map(name => name -> grouped.getOrElse(name, Nil).distinct)
        .toMap

    private def artifacts(
        context: PlanContext,
        sccs: List[List[String]],
        edges: Map[String, List[String]],
    ): List[Cosmo0CteCallableArtifact] =
      sccs.map { names =>
        val sortedNames = names.sorted
        val recursive =
          sortedNames.length > 1 ||
            sortedNames.exists(name =>
              edges.getOrElse(name, Nil).contains(name),
            )
        val id =
          if recursive then s"cte.artifact.scc.${sortedNames.mkString("+")}"
          else s"cte.artifact.${sortedNames.head}"
        Cosmo0CteCallableArtifact(
          id,
          sortedNames,
          recursive,
          sortedNames
            .flatMap(context.callables.get)
            .map(record => cppDefinition(record))
            .mkString("\n"),
        )
      }

    private def scheduleArtifacts(
        artifacts: List[Cosmo0CteCallableArtifact],
        edges: Map[String, List[String]],
    ): List[Cosmo0CteCallableArtifact] =
      val byCallable =
        artifacts
          .flatMap(artifact =>
            artifact.callableNames.map(name => name -> artifact.id),
          )
          .toMap
      val byId = artifacts.map(artifact => artifact.id -> artifact).toMap
      val outgoing = mutable.LinkedHashMap.empty[String, Set[String]]
      artifacts.foreach(artifact => outgoing.update(artifact.id, Set.empty))

      edges.foreach { case (caller, callees) =>
        val callerArtifact = byCallable(caller)
        callees.foreach { callee =>
          val calleeArtifact = byCallable(callee)
          if callerArtifact != calleeArtifact then
            val next =
              outgoing.getOrElse(calleeArtifact, Set.empty) + callerArtifact
            outgoing.update(calleeArtifact, next)
        }
      }

      val incomingCounts = mutable.LinkedHashMap.empty[String, Int]
      artifacts.foreach(artifact => incomingCounts.update(artifact.id, 0))
      outgoing.values.flatten.foreach { id =>
        incomingCounts.update(id, incomingCounts.getOrElse(id, 0) + 1)
      }

      val scheduled = ListBuffer.empty[Cosmo0CteCallableArtifact]
      val ready = mutable.TreeSet.from(
        incomingCounts.collect { case (id, 0) => id },
      )

      while ready.nonEmpty do
        val id = ready.head
        ready -= id
        scheduled += byId(id)
        outgoing.getOrElse(id, Set.empty).toList.sorted.foreach { dependent =>
          val remaining = incomingCounts(dependent) - 1
          incomingCounts.update(dependent, remaining)
          if remaining == 0 then ready += dependent
        }

      scheduled.toList

    private def cppDefinition(record: CallableRecord): String =
      val params = cppParams(record.public.params)
      val body = exprToCpp(record.body)
      s"static int ${record.public.name}($params) { return $body; }"

    private def exprToCpp(node: Node): String =
      unwrapSemi(node) match
        case None =>
          "0"
        case Some(IntLit(value)) =>
          value.toString
        case Some(Ident(name)) =>
          name
        case Some(BinOp(op, lhs, rhs)) =>
          s"(${exprToCpp(lhs)} $op ${exprToCpp(rhs)})"
        case Some(UnOp(op @ ("+" | "-"), value)) =>
          s"($op${exprToCpp(value)})"
        case Some(Apply(Ident(name), args, false)) =>
          s"$name(${args.map(exprToCpp).mkString(", ")})"
        case Some(If(cond, thenExp, Some(elseExp))) =>
          s"((${exprToCpp(cond)}) ? (${exprToCpp(thenExp)}) : (${exprToCpp(elseExp)}))"
        case Some(Block(stmts)) =>
          stmts.lastOption.map(exprToCpp).getOrElse("0")
        case Some(Return(value)) =>
          exprToCpp(value)
        case _ =>
          "0"

    private def stronglyConnectedComponents(
        nodes: List[String],
        edges: Map[String, List[String]],
    ): List[List[String]] =
      val state = SccState()
      nodes.sorted.foreach { node =>
        if !state.indices.contains(node) then strongConnect(node, edges, state)
      }
      state.components.toList

    private final case class SccState(
        indices: mutable.Map[String, Int] = mutable.LinkedHashMap.empty,
        lowLinks: mutable.Map[String, Int] = mutable.LinkedHashMap.empty,
        stack: ListBuffer[String] = ListBuffer.empty,
        onStack: mutable.Set[String] = mutable.LinkedHashSet.empty,
        components: ListBuffer[List[String]] = ListBuffer.empty,
        var nextIndex: Int = 0,
    )

    private def strongConnect(
        node: String,
        edges: Map[String, List[String]],
        state: SccState,
    ): Unit =
      state.indices.update(node, state.nextIndex)
      state.lowLinks.update(node, state.nextIndex)
      state.nextIndex += 1
      state.stack += node
      state.onStack += node

      edges.getOrElse(node, Nil).sorted.foreach { next =>
        if !state.indices.contains(next) then
          strongConnect(next, edges, state)
          state.lowLinks.update(
            node,
            state.lowLinks(node).min(state.lowLinks(next)),
          )
        else if state.onStack.contains(next) then
          state.lowLinks.update(
            node,
            state.lowLinks(node).min(state.indices(next)),
          )
      }

      if state.lowLinks(node) == state.indices(node) then
        val component = ListBuffer.empty[String]
        var done = false
        while !done && state.stack.nonEmpty do
          val value = state.stack.remove(state.stack.length - 1)
          state.onStack -= value
          component += value
          done = value == node
        state.components += component.toList.sorted

    private def nodeSpan(node: Node): SourceSpan =
      if node.offset >= 0 && node.end >= node.offset then
        source.span(node.offset, node.end)
      else source.span(0, source.text.length)
  end Planner

  private def cppParams(params: List[String]): String =
    params.map(name => s"int $name").mkString(", ")

  private def unwrapSemi(node: Node): Option[Node] =
    node match
      case Semi(None)        => None
      case Semi(Some(inner)) => unwrapSemi(inner)
      case other             => Some(other)
end Cosmo0CteFunctionCallGraph
