package cosmo0

import scala.collection.mutable.ListBuffer

private[cosmo0] final case class ProfileDirective(
    name: String,
    span: SourceSpan,
)

/** Extracts checker assertion directives from profile-specific source.
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
