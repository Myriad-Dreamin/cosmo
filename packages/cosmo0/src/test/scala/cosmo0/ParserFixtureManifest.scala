package cosmo0

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object ParserFixtureManifest:
  enum ExpectedStatus:
    case Ok, Error

  final case class Fixture(
      id: String,
      path: String,
      expectedStatus: ExpectedStatus,
      diagnosticCode: Option[String],
  )

  val fixtureRootPath: String =
    "fixtures/cosmo0/parser"

  val parserSourcePath: String =
    "packages/cosmoc/src/parser.cos"

  val parserTestSourcePath: String =
    "packages/cosmoc/src/parser_test.cos"

  def load(): List[Fixture] =
    loadFromDirective("/// parser-fixture:")

  def loadAst(): List[Fixture] =
    loadFromDirective("/// parser-ast-fixture:")

  private def loadFromDirective(prefix: String): List[Fixture] =
    TestFixtureScanner
      .filesUnder(fixtureRootPath, _.endsWith(".cos"))
      .flatMap(path => fixtureFromDirective(path, prefix))

  def readFile(path: String): String =
    NodeFs.readFileSync(path, "utf8").asInstanceOf[String]

  def exists(path: String): Boolean =
    NodeFs.existsSync(path)

  private def fixtureFromDirective(path: String, prefix: String): Option[Fixture] =
    val lines = readFile(path).split("\n").toList.map(_.stripSuffix("\r").trim)
    lines.find(_.startsWith(prefix)).map: line =>
      val expectedStatus = line.stripPrefix(prefix).trim match
        case "ok"    => ExpectedStatus.Ok
        case "error" => ExpectedStatus.Error
        case other =>
          throw new IllegalArgumentException(s"unknown parser fixture status '$other' in $path")
      Fixture(
        id = idFromPath(path),
        path = path,
        expectedStatus = expectedStatus,
        diagnosticCode = directiveValue(lines, "/// parser-diagnostic:"),
      )

  private def directiveValue(lines: List[String], prefix: String): Option[String] =
    lines
      .find(_.startsWith(prefix))
      .map(_.stripPrefix(prefix).trim)
      .filter(_.nonEmpty)

  private def idFromPath(path: String): String =
    path
      .stripPrefix(s"$fixtureRootPath/")
      .stripSuffix(".cos")
      .replace('/', '-')
end ParserFixtureManifest

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object NodeFs extends js.Object:
  def existsSync(path: String): Boolean = js.native
  def readFileSync(path: String, encoding: String): js.Any = js.native
