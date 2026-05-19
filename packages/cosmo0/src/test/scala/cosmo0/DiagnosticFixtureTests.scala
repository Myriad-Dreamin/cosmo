package cosmo0

import scala.collection.mutable.ListBuffer

class DiagnosticFixtureTests extends munit.FunSuite:
  test("diagnostic fixture directives are well formed"):
    val fixtures = DiagnosticFixtureScanner.load()
    val discovered = DiagnosticFixtureScanner.discover()

    assert(fixtures.nonEmpty, "fixtures/diagnostics must contain at least one .cos fixture with /// diag")
    assertEquals(fixtures.map(_.id).distinct.length, fixtures.length)
    assertEquals(fixtures, discovered, "diagnostic manifest must match fixture discovery")

    fixtures.foreach: fixture =>
      assert(ParserFixtureManifest.exists(fixture.path), s"missing diagnostic fixture ${fixture.path}")
      DiagnosticFixtureParser.parse(fixture)

  test("parser_test and Scala diagnostic tests consume the same manifest"):
    val parserTestSource = ParserFixtureManifest.readFile(ParserFixtureManifest.parserTestSourcePath)
    assert(
      parserTestSource.contains(s""""${DiagnosticFixtureScanner.manifestPath}""""),
      s"parser_test.cos must reference ${DiagnosticFixtureScanner.manifestPath}",
    )

  test("cosmo0 diagnostic fixtures report expected diagnostics"):
    DiagnosticFixtureScanner.load().foreach: fixtureRef =>
      val fixture = DiagnosticFixtureParser.parse(fixtureRef)
      val result = checkFixture(fixture)

      assert(
        !result.isSuccess,
        s"${fixture.id} should fail but succeeded",
      )

      fixture.expected.foreach: expected =>
        assert(
          result.diagnostics.exists(matches(expected, _)),
          s"${fixture.id} missing ${expected.label}; got ${renderDiagnostics(result.diagnostics)}",
        )

  private def checkFixture(fixture: DiagnosticFixture): Result[CheckedPackage] =
    val modules = fixture.files.map(file =>
      Cosmo0PackageModule(
        modulePath(file.path),
        SourceFile(file.path, file.text),
      ),
    )
    val pkg = Cosmo0Package(
      rootPath = "fixtures/diagnostics",
      metadata = Cosmo0PackageMetadata(
        name = s"@cosmo0/diagnostic-${fixture.id}",
        version = "0.0.0",
        target = Some("cosmo0"),
      ),
      modules = modules,
    )
    Cosmo0().checkPackage(pkg)

  private def modulePath(path: String): List[String] =
    path.stripSuffix(".cos").split('/').toList.filter(_.nonEmpty)

  private def matches(
      expected: ExpectedDiagnostic,
      actual: Diagnostic,
  ): Boolean =
    actual.severity == expected.severity &&
      actual.span.exists(span =>
        span.fileName == expected.path &&
          span.start.line == expected.line &&
          span.start.column == expected.column,
      ) &&
      s"${actual.code}: ${actual.message}".contains(expected.text)

  private def renderDiagnostics(diagnostics: List[Diagnostic]): String =
    diagnostics
      .map: diagnostic =>
        val location = diagnostic.span match
          case Some(span) => s"${span.fileName}:${span.start.line}:${span.start.column}"
          case None       => "<no span>"
        s"$location ${diagnostic.code}: ${diagnostic.message}"
      .mkString("; ")

final case class DiagnosticFixtureRef(
    id: String,
    path: String,
)

final case class DiagnosticFixture(
    id: String,
    path: String,
    files: List[DiagnosticFixtureFile],
    expected: List[ExpectedDiagnostic],
)

final case class DiagnosticFixtureFile(
    path: String,
    text: String,
)

final case class ExpectedDiagnostic(
    path: String,
    severity: DiagnosticSeverity,
    line: Int,
    column: Int,
    text: String,
):
  def label: String =
    s"$path:$line:$column $text"

object DiagnosticFixtureScanner:
  val rootPath: String =
    "fixtures/diagnostics"

  val manifestPath: String =
    "fixtures/diagnostics/manifest.tsv"

  def load(): List[DiagnosticFixtureRef] =
    ParserFixtureManifest
      .readFile(manifestPath)
      .split("\n")
      .toList
      .map(_.stripSuffix("\r"))
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map(parseManifestLine)

  def discover(): List[DiagnosticFixtureRef] =
    TestFixtureScanner
      .filesUnder(rootPath, _.endsWith(".cos"))
      .filter(hasDiagnosticDirective)
      .map(path => DiagnosticFixtureRef(idFromPath(path), path))

  private def parseManifestLine(line: String): DiagnosticFixtureRef =
    val parts = line.split("\\|", -1).toList
    assert(parts.length == 2, s"diagnostic fixture manifest line must have 2 fields: $line")
    val id = parts(0)
    val path = parts(1)
    assert(id.nonEmpty, s"diagnostic fixture manifest line has empty id: $line")
    assert(path.nonEmpty, s"diagnostic fixture manifest line has empty path: $line")
    DiagnosticFixtureRef(id, path)

  private def hasDiagnosticDirective(path: String): Boolean =
    ParserFixtureManifest
      .readFile(path)
      .split("\n")
      .toList
      .map(_.stripSuffix("\r").trim)
      .exists(_.startsWith("/// diag("))

  private def idFromPath(path: String): String =
    path
      .stripPrefix(s"$rootPath/")
      .stripSuffix(".cos")
      .replace('/', '-')

object DiagnosticFixtureParser:
  private val PathPrefix = "/// path: "
  private val EndPathPrefix = "/// end path: "
  private val DiagnosticLine =
    """^/// diag\(([^,]+),[ ]*([0-9]+):([0-9]+)\):[ ]*(.*)$""".r

  def parse(ref: DiagnosticFixtureRef): DiagnosticFixture =
    val lines = ParserFixtureManifest
      .readFile(ref.path)
      .split("\n", -1)
      .toList
      .map(_.stripSuffix("\r"))

    val files = ListBuffer.empty[DiagnosticFixtureFile]
    val expected = ListBuffer.empty[ExpectedDiagnostic]
    var currentPath: Option[String] = None
    var body = ListBuffer.empty[String]

    lines.zipWithIndex.foreach: (line, index) =>
      val directiveLine = index + 1

      line match
        case value if value.startsWith(PathPrefix) =>
          assert(currentPath.isEmpty, s"${ref.path}:$directiveLine starts a path before ending ${currentPath.get}")
          currentPath = Some(value.stripPrefix(PathPrefix).trim)
          body = ListBuffer.empty[String]

        case value if value.startsWith(EndPathPrefix) =>
          val expectedPath = value.stripPrefix(EndPathPrefix).trim
          val path = currentPath.getOrElse {
            throw new AssertionError(s"${ref.path}:$directiveLine ends $expectedPath without an open path")
          }
          assert(path == expectedPath, s"${ref.path}:$directiveLine ends $expectedPath but current path is $path")
          files += DiagnosticFixtureFile(path, body.mkString("\n"))
          currentPath = None
          body = ListBuffer.empty[String]

        case DiagnosticLine(rawSeverity, rawLine, rawColumn, text) =>
          val path = currentPath.getOrElse {
            throw new AssertionError(s"${ref.path}:$directiveLine declares a diagnostic outside a path block")
          }
          expected += ExpectedDiagnostic(
            path,
            parseSeverity(rawSeverity, ref.path, directiveLine),
            rawLine.toInt,
            rawColumn.toInt,
            text,
          )

        case value if value.startsWith("///") =>
          throw new AssertionError(s"${ref.path}:$directiveLine has unknown directive: $value")

        case value =>
          currentPath match
            case Some(_) =>
              body += value
            case None =>
              assert(value.trim.isEmpty, s"${ref.path}:$directiveLine has source text outside a path block")

    assert(currentPath.isEmpty, s"${ref.path} ended before closing ${currentPath.get}")
    assert(files.nonEmpty, s"${ref.path} must contain at least one path block")
    assert(expected.nonEmpty, s"${ref.path} must declare at least one expected diagnostic")

    DiagnosticFixture(ref.id, ref.path, files.toList, expected.toList)

  private def parseSeverity(
      raw: String,
      path: String,
      line: Int,
  ): DiagnosticSeverity =
    raw.trim match
      case "error"   => DiagnosticSeverity.Error
      case "warning" => DiagnosticSeverity.Warning
      case "info"    => DiagnosticSeverity.Info
      case other =>
        throw new IllegalArgumentException(s"$path:$line has unknown diagnostic severity '$other'")
