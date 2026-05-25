package cosmo0

import scala.collection.mutable.ListBuffer

class NameResolutionFixtureTests extends munit.FunSuite:
  private val positiveRoot = "fixtures/name-resolution/positive"
  private val negativeRoot = "fixtures/name-resolution/negative"

  test("positive name-resolution fixtures check"):
    val fixtures = TestFixtureScanner.filesUnder(positiveRoot, _.endsWith(".cos"))
    assert(fixtures.nonEmpty, s"$positiveRoot must contain .cos fixtures")

    fixtures.foreach: path =>
      val source = ParserFixtureManifest.readFile(path)
      val result = Cosmo0().check(SourceFile(path, source))
      assert(
        result.isSuccess,
        s"$path failed with diagnostics: ${renderDiagnostics(result.diagnostics)}",
      )

  test("negative name-resolution fixtures report stable diagnostics"):
    val fixtures = TestFixtureScanner.filesUnder(negativeRoot, _.endsWith(".cos"))
    assert(fixtures.nonEmpty, s"$negativeRoot must contain .cos fixtures")

    fixtures.foreach: path =>
      val fixture = checkNegativeFixture(path)
      val result = fixture.result

      assert(
        !result.isSuccess,
        s"$path should fail but succeeded",
      )

      fixture.expected.foreach: expected =>
        assert(
          result.diagnostics.exists(matches(expected, _)),
          s"$path missing ${expected.label}; got ${renderDiagnostics(result.diagnostics)}",
        )

  test("C++ namespace imports merge headers and reach backend requirements"):
    val source =
      """import std as cstd from "c++/vector"
        |import std as cstd from "c++/string"
        |
        |type CppVector[T] = cstd::vector[T]
        |""".stripMargin

    val lowered = Cosmo0().lower(SourceFile("fixtures/name-resolution/cpp-backend.cos", source))

    assert(
      lowered.isSuccess,
      s"C++ namespace fixture failed with diagnostics: ${renderDiagnostics(lowered.diagnostics)}",
    )

    val emitted = CppBackend(lowered.value.get.lir).emit()
    assert(
      emitted.isSuccess,
      s"C++ namespace backend failed with diagnostics: ${renderDiagnostics(emitted.diagnostics)}",
    )

    val output = emitted.value.get
    assert(output.backendRequirements.contains(BackendRequirement.include("<vector>")))
    assert(output.backendRequirements.contains(BackendRequirement.include("<string>")))
    assert(
      output.backendRequirements.contains(
        BackendRequirement.cppNamespaceImport("cstd=::std from c++/vector,c++/string"),
      ),
    )
    assert(output.source.contains("#include <vector>"))
    assert(output.source.contains("#include <string>"))
    assert(output.source.contains("template <typename T> using"))
    assert(output.source.contains("= ::std::vector<T>;"))
    assert(output.source.contains("::std::vector"))

  test("C++ namespace imports do not create package graph edges"):
    val pkg = Cosmo0Package(
      rootPath = "fixtures/name-resolution",
      metadata = Cosmo0PackageMetadata(
        name = "@cosmo0/name-resolution-cpp-import",
        version = "0.0.0",
        target = Some("cosmo0"),
      ),
      modules = List(
        Cosmo0PackageModule(
          List("main"),
          SourceFile(
            "fixtures/name-resolution/package-main.cos",
            """import std as cstd from "c++/vector"
              |
              |type CppVector[T] = cstd::vector[T]
              |""".stripMargin,
          ),
        ),
      ),
    )

    val checked = Cosmo0().checkPackage(pkg)

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"package graph treated C++ import as a module edge: ${renderDiagnostics(checked.diagnostics)}",
    )
    assertEquals(checked.value.get.moduleOrder, List("main"))

  private def parseNegativeFixture(path: String): NameResolutionNegativeFixture =
    val expected = ListBuffer.empty[NameResolutionExpectedDiagnostic]
    val source = ParserFixtureManifest
      .readFile(path)
      .split("\n", -1)
      .toList
      .map(_.stripSuffix("\r"))
      .map:
        case DiagnosticLine(rawSeverity, rawLine, rawColumn, text) =>
          expected += NameResolutionExpectedDiagnostic(
            path,
            parseSeverity(rawSeverity, path),
            rawLine.toInt,
            rawColumn.toInt,
            text,
          )
          ""
        case line => line
      .mkString("\n")

    assert(expected.nonEmpty, s"$path must declare at least one /// diag directive")
    NameResolutionNegativeFixture(expected.toList, Cosmo0().check(SourceFile(path, source)))

  private def checkNegativeFixture(path: String): NameResolutionNegativeFixture =
    val text = ParserFixtureManifest.readFile(path)
    if text.split("\n").exists(_.startsWith("/// path: ")) then
      val ref = DiagnosticFixtureRef(path.stripPrefix(s"$negativeRoot/").stripSuffix(".cos"), path)
      val fixture = DiagnosticFixtureParser.parse(ref)
      val modules = fixture.files.map(file =>
        Cosmo0PackageModule(
          modulePath(file.path),
          SourceFile(file.path, file.text),
        ),
      )
      val pkg = Cosmo0Package(
        rootPath = negativeRoot,
        metadata = Cosmo0PackageMetadata(
          name = s"@cosmo0/name-resolution-${ref.id}",
          version = "0.0.0",
          target = Some("cosmo0"),
        ),
        modules = modules,
      )
      val expected = fixture.expected.map(expected =>
        NameResolutionExpectedDiagnostic(
          expected.path,
          expected.severity,
          expected.line,
          expected.column,
          expected.text,
        ),
      )
      NameResolutionNegativeFixture(expected, Cosmo0().checkPackage(pkg))
    else parseNegativeFixture(path)

  private def modulePath(path: String): List[String] =
    path.stripSuffix(".cos").split('/').toList.filter(_.nonEmpty)

  private def parseSeverity(raw: String, path: String): DiagnosticSeverity =
    raw.trim match
      case "error"   => DiagnosticSeverity.Error
      case "warning" => DiagnosticSeverity.Warning
      case "info"    => DiagnosticSeverity.Info
      case other     => throw new IllegalArgumentException(s"$path has unknown diagnostic severity '$other'")

  private def matches(
      expected: NameResolutionExpectedDiagnostic,
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

  private val DiagnosticLine =
    """^/// diag\(([^,]+),[ ]*([0-9]+):([0-9]+)\):[ ]*(.*)$""".r

final case class NameResolutionNegativeFixture(
    expected: List[NameResolutionExpectedDiagnostic],
    result: Result[?],
)

final case class NameResolutionExpectedDiagnostic(
    path: String,
    severity: DiagnosticSeverity,
    line: Int,
    column: Int,
    text: String,
):
  def label: String =
    s"$path:$line:$column $text"
