package cosmo0

import scala.util.control.NonFatal

import fastparse.Parsed
import fastparse.parse as fastParse

final class Cosmo0:
  def parse(sourceText: String): Cosmo0Result[Cosmo0ParsedModule] =
    parse(Cosmo0SourceFile("<memory>", sourceText))

  def parse(source: Cosmo0SourceFile): Cosmo0Result[Cosmo0ParsedModule] =
    try
      fastParse(source.text, Parser.root(_)) match
        case Parsed.Success(ast, _) =>
          Cosmo0Result.success(
            Cosmo0Phase.Parse,
            Cosmo0ParsedModule(source, ast),
          )
        case failure: Parsed.Failure =>
          Cosmo0Result.failure(
            Cosmo0Phase.Parse,
            List(parseFailureDiagnostic(source, failure)),
          )
    catch
      case NonFatal(error) =>
        Cosmo0Result.failure(
          Cosmo0Phase.Parse,
          List(
            Cosmo0Diagnostic(
              Cosmo0Phase.Parse,
              Cosmo0DiagnosticSeverity.Error,
              "cosmo0.parse.exception",
              Option(error.getMessage).getOrElse(error.getClass.getName),
            ),
          ),
        )

  def check(sourceText: String): Cosmo0Result[Cosmo0CheckedModule] =
    check(Cosmo0SourceFile("<memory>", sourceText))

  def elaborate(sourceText: String): Cosmo0Result[Cosmo0UntypedModule] =
    elaborate(Cosmo0SourceFile("<memory>", sourceText))

  def elaborate(source: Cosmo0SourceFile): Cosmo0Result[Cosmo0UntypedModule] =
    parse(source) match
      case parsed if parsed.isSuccess =>
        Cosmo0UntypedElaborator().elaborate(parsed.value.get)
      case failed =>
        Cosmo0Result.failure(Cosmo0Phase.Check, failed.diagnostics)

  def check(source: Cosmo0SourceFile): Cosmo0Result[Cosmo0CheckedModule] =
    elaborate(source) match
      case elaborated if elaborated.isSuccess =>
        Cosmo0Result.pending(
          Cosmo0Phase.Check,
          pendingDiagnostic(
            Cosmo0Phase.Check,
            "cosmo0.check.pending",
            "cosmo0 source typing is not implemented yet",
          ),
        )
      case unsupported if unsupported.isUnsupported =>
        Cosmo0Result(
          Cosmo0Phase.Check,
          Cosmo0PhaseStatus.Unsupported,
          None,
          unsupported.diagnostics,
        )
      case failed =>
        Cosmo0Result.failure(Cosmo0Phase.Check, failed.diagnostics)

  def compile(sourceText: String): Cosmo0Result[Cosmo0CompiledModule] =
    compile(Cosmo0SourceFile("<memory>", sourceText))

  def compile(source: Cosmo0SourceFile): Cosmo0Result[Cosmo0CompiledModule] =
    check(source) match
      case checked if checked.isFailure =>
        Cosmo0Result.failure(Cosmo0Phase.Compile, checked.diagnostics)
      case checked if checked.isUnsupported =>
        Cosmo0Result(
          Cosmo0Phase.Compile,
          Cosmo0PhaseStatus.Unsupported,
          None,
          checked.diagnostics,
        )
      case _ =>
        Cosmo0Result.unsupported(
          Cosmo0Phase.Compile,
          pendingDiagnostic(
            Cosmo0Phase.Compile,
            "cosmo0.compile.unsupported",
            "cosmo0 compilation is not implemented yet",
          ),
        )

  private def parseFailureDiagnostic(
      source: Cosmo0SourceFile,
      failure: Parsed.Failure,
  ): Cosmo0Diagnostic =
    Cosmo0Diagnostic(
      Cosmo0Phase.Parse,
      Cosmo0DiagnosticSeverity.Error,
      "cosmo0.parse.failed",
      failure.trace().longAggregateMsg,
      Some(source.span(failure.index, failure.index)),
    )

  private def pendingDiagnostic(
      phase: Cosmo0Phase,
      code: String,
      message: String,
  ): Cosmo0Diagnostic =
    Cosmo0Diagnostic(
      phase,
      Cosmo0DiagnosticSeverity.Info,
      code,
      message,
    )

object Cosmo0:
  def apply(): Cosmo0 = new Cosmo0()
