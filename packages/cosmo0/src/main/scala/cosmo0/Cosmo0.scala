package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportAll
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.control.NonFatal

import fastparse.Parsed
import fastparse.parse as fastParse

@JSExportTopLevel("Cosmo0")
final class Cosmo0:
  private val packagePipeline = PackagePipeline(this)

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
      case lowered =>
        val loweredModule = lowered.value.get
        CppBackend().emit(loweredModule.lir) match
          case emitted if emitted.isSuccess =>
            Result.success(
              Phase.Compile,
              CompiledModule(loweredModule.checked, emitted.value.get.source),
            )
          case failed =>
            Result.failure(Phase.Compile, failed.diagnostics)

  def loadPackage(rootPath: String): Result[Cosmo0Package] =
    packagePipeline.load(rootPath)

  def checkPackage(rootPath: String): Result[CheckedPackage] =
    loadPackage(rootPath) match
      case loaded if loaded.isSuccess =>
        checkPackage(loaded.value.get)
      case failed =>
        Result(
          Phase.Check,
          failed.status,
          None,
          failed.diagnostics,
        )

  def checkPackage(pkg: Cosmo0Package): Result[CheckedPackage] =
    packagePipeline.check(pkg)

  def compilePackage(rootPath: String): Result[CompiledPackage] =
    loadPackage(rootPath) match
      case loaded if loaded.isSuccess =>
        compilePackage(loaded.value.get)
      case failed =>
        Result(
          Phase.Compile,
          failed.status,
          None,
          failed.diagnostics,
        )

  def compilePackage(pkg: Cosmo0Package): Result[CompiledPackage] =
    checkPackage(pkg) match
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
        val checkedPackage = checked.value.get
        CppBackend().emit(checkedPackage.lowered.lir) match
          case emitted if emitted.isSuccess =>
            Result.success(
              Phase.Compile,
              CompiledPackage(checkedPackage, emitted.value.get),
            )
          case failed =>
            Result.failure(Phase.Compile, failed.diagnostics)

  def checkRunnablePackage(rootPath: String): Result[CheckedPackage] =
    checkPackage(rootPath) match
      case checked if checked.isSuccess =>
        validateRunnableEntrypoint(checked.value.get)
      case failed =>
        Result(
          Phase.Check,
          failed.status,
          None,
          failed.diagnostics,
        )

  def compileRunnablePackage(rootPath: String): Result[CompiledPackage] =
    checkRunnablePackage(rootPath) match
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
        val checkedPackage = checked.value.get
        CppBackend().emit(checkedPackage.lowered.lir) match
          case emitted if emitted.isSuccess =>
            Result.success(
              Phase.Compile,
              CompiledPackage(checkedPackage, emitted.value.get),
            )
          case failed =>
            Result.failure(Phase.Compile, failed.diagnostics)

  @JSExport
  def compileRunnablePackageForHost(rootPath: String): Cosmo0HostCompileResult =
    Cosmo0HostCompileResult.fromResult(compileRunnablePackage(rootPath))

  @JSExport
  def compilePackageForHost(rootPath: String): Cosmo0HostCompileResult =
    Cosmo0HostCompileResult.fromResult(compilePackage(rootPath))

  private def validateRunnableEntrypoint(pkg: CheckedPackage): Result[CheckedPackage] =
    runnableEntrypoint(pkg) match
      case Some(main) if isRunnableEntrypoint(main) =>
        Result.success(Phase.Check, pkg)
      case Some(main) =>
        Result.failure(
          Phase.Check,
          List(
            Diagnostic(
              Phase.Check,
              DiagnosticSeverity.Error,
              "cosmo0.package.invalid-run-entrypoint",
              s"cosmo0 package ${pkg.metadata.name} run entrypoint must be a body-backed top-level zero-argument main returning Unit or an integer",
              Some(main.span),
            ),
          ),
        )
      case None =>
        Result.failure(
          Phase.Check,
          List(
            Diagnostic(
              Phase.Check,
              DiagnosticSeverity.Error,
              "cosmo0.package.missing-run-entrypoint",
              s"cosmo0 package ${pkg.metadata.name} does not expose a top-level zero-argument main run entrypoint",
            ),
          ),
        )

  private def runnableEntrypoint(pkg: CheckedPackage): Option[TypedFunction] =
    pkg.checked.typed.declarations.collectFirst {
      case fn: TypedFunction if fn.owner.isEmpty && fn.name == "main" => fn
    }

  private def isRunnableEntrypoint(fn: TypedFunction): Boolean =
    fn.params.isEmpty &&
      fn.externBinding.isEmpty &&
      fn.body.nonEmpty &&
      runnableReturnType(fn.returnType)

  private def runnableReturnType(valueType: SourceType): Boolean =
    SourceType.same(valueType, SourceType.Unit) || SourceType.isInteger(valueType)

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

@JSExportAll
final class Cosmo0HostDiagnostic(
    val phase: String,
    val severity: String,
    val code: String,
    val message: String,
    val fileName: String,
    val line: Int,
    val column: Int,
)

object Cosmo0HostDiagnostic:
  def fromDiagnostic(diagnostic: Diagnostic): Cosmo0HostDiagnostic =
    val position = diagnostic.span.map(_.start)
    new Cosmo0HostDiagnostic(
      diagnostic.phase.toString,
      diagnostic.severity.toString,
      diagnostic.code,
      diagnostic.message,
      diagnostic.span.map(_.fileName).getOrElse(""),
      position.map(_.line).getOrElse(0),
      position.map(_.column).getOrElse(0),
    )

@JSExportAll
final class Cosmo0HostCompileResult(
    val ok: Boolean,
    val status: String,
    val moduleName: String,
    val output: String,
    val supportLibraryLinkArguments: js.Array[String],
    val diagnostics: js.Array[Cosmo0HostDiagnostic],
)

object Cosmo0HostCompileResult:
  def fromResult(result: Result[CompiledPackage]): Cosmo0HostCompileResult =
    val output = result.value.map(_.output)
    new Cosmo0HostCompileResult(
      result.isSuccess,
      result.status.toString,
      output.map(_.moduleName).getOrElse(""),
      output.map(_.source).getOrElse(""),
      js.Array(output.map(_.supportLibraryLinkArguments).getOrElse(Nil)*),
      js.Array(result.diagnostics.map(Cosmo0HostDiagnostic.fromDiagnostic)*),
    )
