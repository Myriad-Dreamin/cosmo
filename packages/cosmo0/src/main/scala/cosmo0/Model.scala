package cosmo0

import cosmo.syntax

enum Cosmo0Phase:
  case Parse, Check, Compile

enum Cosmo0PhaseStatus:
  case Succeeded, Pending, Unsupported, Failed

enum Cosmo0DiagnosticSeverity:
  case Info, Warning, Error

final case class Cosmo0SourcePosition(
    offset: Int,
    line: Int,
    column: Int,
)

final case class Cosmo0SourceSpan(
    fileName: String,
    start: Cosmo0SourcePosition,
    end: Cosmo0SourcePosition,
)

final case class Cosmo0SourceFile(
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

  def positionAt(offset: Int): Cosmo0SourcePosition =
    val boundedOffset = offset.max(0).min(text.length)
    val lineIndex = bestLineStartIndex(boundedOffset)
    Cosmo0SourcePosition(
      boundedOffset,
      lineIndex + 1,
      boundedOffset - lineStarts(lineIndex) + 1,
    )

  def span(startOffset: Int, endOffset: Int): Cosmo0SourceSpan =
    Cosmo0SourceSpan(
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

final case class Cosmo0Diagnostic(
    phase: Cosmo0Phase,
    severity: Cosmo0DiagnosticSeverity,
    code: String,
    message: String,
    span: Option[Cosmo0SourceSpan] = None,
)

final case class Cosmo0Result[+A](
    phase: Cosmo0Phase,
    status: Cosmo0PhaseStatus,
    value: Option[A],
    diagnostics: List[Cosmo0Diagnostic] = Nil,
):
  def isSuccess: Boolean = status == Cosmo0PhaseStatus.Succeeded
  def isPending: Boolean = status == Cosmo0PhaseStatus.Pending
  def isUnsupported: Boolean = status == Cosmo0PhaseStatus.Unsupported
  def isFailure: Boolean = status == Cosmo0PhaseStatus.Failed

object Cosmo0Result:
  def success[A](phase: Cosmo0Phase, value: A): Cosmo0Result[A] =
    Cosmo0Result(phase, Cosmo0PhaseStatus.Succeeded, Some(value))

  def pending[A](
      phase: Cosmo0Phase,
      diagnostic: Cosmo0Diagnostic,
      value: Option[A] = None,
  ): Cosmo0Result[A] =
    Cosmo0Result(phase, Cosmo0PhaseStatus.Pending, value, List(diagnostic))

  def unsupported[A](
      phase: Cosmo0Phase,
      diagnostic: Cosmo0Diagnostic,
      value: Option[A] = None,
  ): Cosmo0Result[A] =
    Cosmo0Result(phase, Cosmo0PhaseStatus.Unsupported, value, List(diagnostic))

  def failure[A](
      phase: Cosmo0Phase,
      diagnostics: List[Cosmo0Diagnostic],
  ): Cosmo0Result[A] =
    Cosmo0Result(phase, Cosmo0PhaseStatus.Failed, None, diagnostics)

final case class Cosmo0ParsedModule(
    source: Cosmo0SourceFile,
    ast: syntax.Block,
)

final case class Cosmo0CheckedModule(
    parsed: Cosmo0ParsedModule,
)

final case class Cosmo0CompiledModule(
    checked: Cosmo0CheckedModule,
    output: String,
)
