package cosmo0

class SampleCorpusTests extends munit.FunSuite:
  test("cosmo0 sample manifest points at existing current samples"):
    val samples = SampleCorpusManifest.load()

    assert(samples.nonEmpty, "samples/manifest.tsv must list at least one current cosmo0 sample")
    assertEquals(samples.map(_.id).distinct.length, samples.length)
    assert(samples.exists(_.kind == SampleCorpusManifest.SampleKind.Source))
    assert(samples.exists(_.kind == SampleCorpusManifest.SampleKind.Package))

    samples.foreach: sample =>
      assert(
        ParserFixtureManifest.exists(sample.path),
        s"missing sample ${sample.path}",
      )

  test("current cosmo0 source samples compile"):
    SampleCorpusManifest.load()
      .filter(_.kind == SampleCorpusManifest.SampleKind.Source)
      .foreach: sample =>
        val source = SourceFile(sample.path, ParserFixtureManifest.readFile(sample.path))
        val result = Cosmo0().compile(source)

        assertEquals(result.phase, Phase.Compile)
        assert(
          result.isSuccess,
          s"${sample.id} should compile but got ${result.diagnostics.map(d => d.code -> d.message)}",
        )

  test("current cosmo0 package samples compile"):
    SampleCorpusManifest.load()
      .filter(_.kind == SampleCorpusManifest.SampleKind.Package)
      .foreach: sample =>
        val result = Cosmo0().compilePackage(sample.path)

        assertEquals(result.phase, Phase.Compile)
        assert(
          result.isSuccess,
          s"${sample.id} package should compile but got ${result.diagnostics.map(d => d.code -> d.message)}",
        )

object SampleCorpusManifest:
  enum SampleKind:
    case Source, Package

  final case class Sample(
      id: String,
      kind: SampleKind,
      path: String,
  )

  val manifestPath: String =
    "samples/manifest.tsv"

  def load(): List[Sample] =
    ParserFixtureManifest
      .readFile(manifestPath)
      .split("\n")
      .toList
      .map(_.stripSuffix("\r"))
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map(parseLine)

  private def parseLine(line: String): Sample =
    val parts = line.split("\\|", -1).toList
    assert(parts.length == 3, s"sample manifest line must have 3 fields: $line")

    Sample(
      id = parts(0),
      kind = parseKind(parts(1), line),
      path = parts(2),
    )

  private def parseKind(raw: String, line: String): SampleKind =
    raw match
      case "source"  => SampleKind.Source
      case "package" => SampleKind.Package
      case other =>
        throw new IllegalArgumentException(s"unknown sample kind '$other' in line: $line")

