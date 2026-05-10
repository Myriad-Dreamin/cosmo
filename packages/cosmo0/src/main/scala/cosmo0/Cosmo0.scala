package cosmo0

import scala.util.control.NonFatal

import fastparse.Parsed
import fastparse.parse as fastParse

final class Cosmo0:
  def parse(sourceText: String): Result[ParsedModule] =
    parse(SourceFile("<memory>", sourceText))

  def parse(source: SourceFile): Result[ParsedModule] =
    try
      fastParse(source.text, Parser.root(_)) match
        case Parsed.Success(ast, _) =>
          Result.success(
            Phase.Parse,
            ParsedModule(source, ast),
          )
        case failure: Parsed.Failure =>
          Result.failure(
            Phase.Parse,
            List(parseFailureDiagnostic(source, failure)),
          )
    catch
      case NonFatal(error) =>
        Result.failure(
          Phase.Parse,
          List(
            Diagnostic(
              Phase.Parse,
              DiagnosticSeverity.Error,
              "cosmo0.parse.exception",
              Option(error.getMessage).getOrElse(error.getClass.getName),
            ),
          ),
        )

  def check(sourceText: String): Result[CheckedModule] =
    check(SourceFile("<memory>", sourceText))

  def elaborate(sourceText: String): Result[UntypedModule] =
    elaborate(SourceFile("<memory>", sourceText))

  def elaborate(source: SourceFile): Result[UntypedModule] =
    parse(source) match
      case parsed if parsed.isSuccess =>
        UntypedElaborator().elaborate(parsed.value.get)
      case failed =>
        Result.failure(Phase.Check, failed.diagnostics)

  def check(source: SourceFile): Result[CheckedModule] =
    elaborate(source) match
      case elaborated if elaborated.isSuccess =>
        SourceTyper().check(elaborated.value.get) match
          case checked if checked.isSuccess =>
            Result.success(Phase.Check, CheckedModule(checked.value.get))
          case failed =>
            Result.failure(Phase.Check, failed.diagnostics)
      case unsupported if unsupported.isUnsupported =>
        Result(
          Phase.Check,
          PhaseStatus.Unsupported,
          None,
          unsupported.diagnostics,
        )
      case failed =>
        Result.failure(Phase.Check, failed.diagnostics)

  def compile(sourceText: String): Result[CompiledModule] =
    compile(SourceFile("<memory>", sourceText))

  def lower(sourceText: String): Result[LoweredModule] =
    lower(SourceFile("<memory>", sourceText))

  def lower(source: SourceFile): Result[LoweredModule] =
    check(source) match
      case checked if checked.isFailure =>
        Result.failure(Phase.Compile, checked.diagnostics)
      case checked if checked.isUnsupported =>
        Result(
          Phase.Compile,
          PhaseStatus.Unsupported,
          None,
          checked.diagnostics,
        )
      case checked =>
        val checkedModule = checked.value.get
        LirLowerer().lower(checkedModule.typed) match
          case lowered if lowered.isSuccess =>
            Result.success(Phase.Compile, LoweredModule(checkedModule, lowered.value.get))
          case failed =>
            Result.failure(Phase.Compile, failed.diagnostics)

  def compile(source: SourceFile): Result[CompiledModule] =
    lower(source) match
      case lowered if lowered.isFailure =>
        Result.failure(Phase.Compile, lowered.diagnostics)
      case lowered if lowered.isUnsupported =>
        Result(
          Phase.Compile,
          PhaseStatus.Unsupported,
          None,
          lowered.diagnostics,
        )
      case _ =>
        Result.unsupported(
          Phase.Compile,
          pendingDiagnostic(
            Phase.Compile,
            "cosmo0.compile.unsupported",
            "cosmo0 compilation is not implemented yet",
          ),
        )

  private def parseFailureDiagnostic(
      source: SourceFile,
      failure: Parsed.Failure,
  ): Diagnostic =
    Diagnostic(
      Phase.Parse,
      DiagnosticSeverity.Error,
      "cosmo0.parse.failed",
      failure.trace().longAggregateMsg,
      Some(source.span(failure.index, failure.index)),
    )

  private def pendingDiagnostic(
      phase: Phase,
      code: String,
      message: String,
  ): Diagnostic =
    Diagnostic(
      phase,
      DiagnosticSeverity.Info,
      code,
      message,
    )

object Cosmo0:
  def apply(): Cosmo0 = new Cosmo0()
