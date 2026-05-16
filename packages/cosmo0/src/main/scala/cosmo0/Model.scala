package cosmo0

import cosmo.syntax

enum Phase:
  case Parse, Check, Compile

enum PhaseStatus:
  case Succeeded, Pending, Unsupported, Failed

enum DiagnosticSeverity:
  case Info, Warning, Error

final case class SourcePosition(
    offset: Int,
    line: Int,
    column: Int,
)

final case class SourceSpan(
    fileName: String,
    start: SourcePosition,
    end: SourcePosition,
)

final case class SourceFile(
    name: String,
    text: String,
):
  private lazy val lineStarts: Array[Int] =
    val starts = scala.collection.mutable.ArrayBuffer(0)
    var i = 0
    while i < text.length do
      if text.charAt(i) == '\n' then starts += i + 1
      i += 1
    starts.toArray

  def positionAt(offset: Int): SourcePosition =
    val boundedOffset = offset.max(0).min(text.length)
    val lineIndex = bestLineStartIndex(boundedOffset)
    SourcePosition(
      boundedOffset,
      lineIndex + 1,
      boundedOffset - lineStarts(lineIndex) + 1,
    )

  def span(startOffset: Int, endOffset: Int): SourceSpan =
    SourceSpan(
      name,
      positionAt(startOffset),
      positionAt(endOffset.max(startOffset)),
    )

  private def bestLineStartIndex(offset: Int): Int =
    var low = 0
    var high = lineStarts.length - 1
    var best = 0
    while low <= high do
      val mid = (low + high) / 2
      if lineStarts(mid) <= offset then
        best = mid
        low = mid + 1
      else high = mid - 1
    best

final case class Diagnostic(
    phase: Phase,
    severity: DiagnosticSeverity,
    code: String,
    message: String,
    span: Option[SourceSpan] = None,
)

final case class Result[+A](
    phase: Phase,
    status: PhaseStatus,
    value: Option[A],
    diagnostics: List[Diagnostic] = Nil,
):
  def isSuccess: Boolean = status == PhaseStatus.Succeeded
  def isPending: Boolean = status == PhaseStatus.Pending
  def isUnsupported: Boolean = status == PhaseStatus.Unsupported
  def isFailure: Boolean = status == PhaseStatus.Failed

object Result:
  def success[A](phase: Phase, value: A): Result[A] =
    Result(phase, PhaseStatus.Succeeded, Some(value))

  def pending[A](
      phase: Phase,
      diagnostic: Diagnostic,
      value: Option[A] = None,
  ): Result[A] =
    Result(phase, PhaseStatus.Pending, value, List(diagnostic))

  def unsupported[A](
      phase: Phase,
      diagnostic: Diagnostic,
      value: Option[A] = None,
  ): Result[A] =
    Result(phase, PhaseStatus.Unsupported, value, List(diagnostic))

  def failure[A](
      phase: Phase,
      diagnostics: List[Diagnostic],
  ): Result[A] =
    Result(phase, PhaseStatus.Failed, None, diagnostics)

final case class ParsedModule(
    source: SourceFile,
    ast: syntax.Block,
)

final case class CheckedModule(
    typed: TypedModule,
)

final case class LoweredModule(
    checked: CheckedModule,
    lir: LirModule,
)

final case class CompiledModule(
    checked: CheckedModule,
    output: String,
)

final case class Cosmo0PackageMetadata(
    name: String,
    version: String,
    sourceRoot: String = "src",
    target: Option[String] = None,
    stageProfile: Option[String] = None,
    sourceFiles: Option[List[String]] = None,
    dependencies: List[String] = Nil,
):
  def outputModuleName: String =
    val raw = name.stripPrefix("@").replace('/', '_')
    val builder = new StringBuilder
    raw.foreach { char =>
      if char.isLetterOrDigit || char == '_' then builder.append(char)
      else if builder.nonEmpty && builder.last != '_' then builder.append('_')
    }
    val cleaned = builder.toString.stripPrefix("_").stripSuffix("_")
    if cleaned.nonEmpty then cleaned else "package"

final case class Cosmo0PackageModule(
    modulePath: List[String],
    source: SourceFile,
):
  def name: String = modulePath.mkString("::")

final case class Cosmo0Package(
    rootPath: String,
    metadata: Cosmo0PackageMetadata,
    modules: List[Cosmo0PackageModule],
)

final case class CheckedPackage(
    metadata: Cosmo0PackageMetadata,
    modules: List[Cosmo0PackageModule],
    moduleOrder: List[String],
    checked: CheckedModule,
    lowered: LoweredModule,
)

final case class CompiledPackage(
    checked: CheckedPackage,
    output: CppOutput,
)
