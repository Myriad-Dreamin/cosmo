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

  val manifestPath: String =
    "fixtures/cosmo0/parser/manifest.tsv"

  val parserSourcePath: String =
    "packages/cosmoc/src/parser.cos"

  val parserTestSourcePath: String =
    "packages/cosmoc/src/parser_test.cos"

  def load(): List[Fixture] =
    readFile(manifestPath)
      .split("\n")
      .toList
      .map(_.stripSuffix("\r"))
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map(parseLine)

  def readFile(path: String): String =
    NodeFs.readFileSync(path, "utf8").asInstanceOf[String]

  def exists(path: String): Boolean =
    NodeFs.existsSync(path)

  private def parseLine(line: String): Fixture =
    val parts = line.split("\\|", -1).toList
    assert(
      parts.length == 3 || parts.length == 4,
      s"fixture manifest line must have 3 or 4 fields: $line",
    )
    val expectedStatus = parts(2) match
      case "ok"    => ExpectedStatus.Ok
      case "error" => ExpectedStatus.Error
      case other =>
        throw new IllegalArgumentException(s"unknown parser fixture status '$other' in line: $line")
    Fixture(
      id = parts(0),
      path = parts(1),
      expectedStatus = expectedStatus,
      diagnosticCode = parts.lift(3).filter(_.nonEmpty),
    )
end ParserFixtureManifest

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object NodeFs extends js.Object:
  def existsSync(path: String): Boolean = js.native
  def readFileSync(path: String, encoding: String): js.Any = js.native
