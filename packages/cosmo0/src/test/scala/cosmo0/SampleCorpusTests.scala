package cosmo0

class SampleCorpusTests extends munit.FunSuite:
  test("cosmo0 sample directives discover current samples"):
    val samples = SampleCorpusScanner.load()
    val discovered = SampleCorpusScanner.discover()

    assert(samples.nonEmpty, "samples must contain at least one .cos file with a /// expect directive")
    assertEquals(samples.map(sampleKey).distinct.length, samples.length)
    assertEquals(samples, discovered, "sample manifest must match directive discovery")
    assert(samples.exists(_.kind == SampleCorpusScanner.SampleKind.Source))
    assert(samples.exists(_.kind == SampleCorpusScanner.SampleKind.Package))

    samples.foreach: sample =>
      assert(sample.sourcePaths.nonEmpty, s"${sample.id} must list at least one source path")
      sample.sourcePaths.foreach(path => assert(ParserFixtureManifest.exists(path), s"missing sample source $path"))
      sample.kind match
        case SampleCorpusScanner.SampleKind.Source =>
          assert(ParserFixtureManifest.exists(sample.path), s"missing sample ${sample.path}")
          assertEquals(sample.sourcePaths, List(sample.path))
        case SampleCorpusScanner.SampleKind.Package =>
          assert(ParserFixtureManifest.exists(s"${sample.path}/cosmo.json"), s"missing sample package ${sample.path}")

  test("parser_test and Scala sample tests consume the same manifest"):
    val parserTestSource = ParserFixtureManifest.readFile(ParserFixtureManifest.parserTestSourcePath)
    assert(
      parserTestSource.contains(s""""${SampleCorpusScanner.manifestPath}""""),
      s"parser_test.cos must reference ${SampleCorpusScanner.manifestPath}",
    )

  test("current cosmo0 source samples compile"):
    SampleCorpusScanner.load()
      .filter(_.kind == SampleCorpusScanner.SampleKind.Source)
      .foreach: sample =>
        val source = SourceFile(sample.path, ParserFixtureManifest.readFile(sample.path))
        val result = Cosmo0().compile(source)

        assertEquals(result.phase, Phase.Compile)
        assert(
          result.isSuccess,
          s"${sample.id} should compile but got ${result.diagnostics.map(d => d.code -> d.message)}",
        )

  test("current cosmo0 package samples compile"):
    SampleCorpusScanner.load()
      .filter(_.kind == SampleCorpusScanner.SampleKind.Package)
      .foreach: sample =>
        val result = Cosmo0().compilePackage(sample.path)

        assertEquals(result.phase, Phase.Compile)
        assert(
          result.isSuccess,
          s"${sample.id} package should compile but got ${result.diagnostics.map(d => d.code -> d.message)}",
        )

  private def sampleKey(sample: SampleCorpusScanner.Sample): String =
    s"${sample.kind}:${sample.path}"

object SampleCorpusScanner:
  enum SampleKind:
    case Source, Package

  final case class Sample(
      id: String,
      kind: SampleKind,
      path: String,
      directivePath: String,
      sourcePaths: List[String],
  )

  val rootPath: String =
    "samples"

  val manifestPath: String =
    "samples/manifest.tsv"

  private val CompileDirective = "/// expect: compile"
  private val CompilePackageDirective = "/// expect: compile-package"

  def load(): List[Sample] =
    ParserFixtureManifest
      .readFile(manifestPath)
      .split("\n")
      .toList
      .map(_.stripSuffix("\r"))
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map(parseManifestLine)

  def discover(): List[Sample] =
    TestFixtureScanner
      .filesUnder(rootPath, _.endsWith(".cos"))
      .flatMap(sampleFromFile)
      .groupBy(sample => sample.kind -> sample.path)
      .values
      .map(_.head)
      .toList
      .sortBy(sample => sample.kind.toString -> sample.path)

  private def sampleFromFile(path: String): Option[Sample] =
    val directives = ParserFixtureManifest
      .readFile(path)
      .split("\n")
      .toList
      .map(_.stripSuffix("\r").trim)
      .filter(_.startsWith("/// expect:"))

    directives match
      case Nil =>
        None
      case CompileDirective :: Nil =>
        Some(Sample(idFromPath(path), SampleKind.Source, path, path, List(path)))
      case CompilePackageDirective :: Nil =>
        val root = packageRootFor(path)
        Some(Sample(idFromPath(root), SampleKind.Package, root, path, packageSourcePaths(root)))
      case unknown :: Nil =>
        throw new IllegalArgumentException(s"$path has unknown sample directive: $unknown")
      case _ =>
        throw new IllegalArgumentException(s"$path has multiple sample directives: ${directives.mkString(", ")}")

  private def parseManifestLine(line: String): Sample =
    val parts = line.split("\\|", -1).toList
    assert(parts.length == 4, s"sample manifest line must have 4 fields: $line")
    val id = parts(0)
    val kind = parseKind(parts(1), line)
    val path = parts(2)
    val sources = parts(3).split(",", -1).toList.filter(_.nonEmpty)
    assert(id.nonEmpty, s"sample manifest line has empty id: $line")
    assert(path.nonEmpty, s"sample manifest line has empty path: $line")
    assert(sources.nonEmpty, s"sample manifest line has no sources: $line")
    val directivePath =
      kind match
        case SampleKind.Source =>
          assert(sources == List(path), s"source sample must list itself: $line")
          path
        case SampleKind.Package =>
          sources.find(hasCompilePackageDirective).getOrElse {
            throw new IllegalArgumentException(s"package sample manifest sources do not contain a compile-package directive: $line")
          }
    Sample(id, kind, path, directivePath, sources)

  private def parseKind(raw: String, line: String): SampleKind =
    raw match
      case "source"  => SampleKind.Source
      case "package" => SampleKind.Package
      case other =>
        throw new IllegalArgumentException(s"sample manifest line has unknown kind '$other': $line")

  private def packageSourcePaths(root: String): List[String] =
    TestFixtureScanner.filesUnder(root, _.endsWith(".cos"))

  private def hasCompilePackageDirective(path: String): Boolean =
    ParserFixtureManifest
      .readFile(path)
      .split("\n")
      .toList
      .map(_.stripSuffix("\r").trim)
      .contains(CompilePackageDirective)

  private def packageRootFor(path: String): String =
    var current = parent(path)
    while current.nonEmpty do
      if ParserFixtureManifest.exists(s"$current/cosmo.json") then return current

      val next = parent(current)
      if next == current then
        throw new IllegalArgumentException(s"$path has $CompilePackageDirective but no ancestor cosmo.json")
      current = next

    throw new IllegalArgumentException(s"$path has $CompilePackageDirective but no ancestor cosmo.json")

  private def parent(path: String): String =
    val index = path.lastIndexOf('/')
    if index < 0 then path else path.take(index)

  private def idFromPath(path: String): String =
    path
      .stripPrefix(s"$rootPath/")
      .stripSuffix(".cos")
      .replace('/', '-')
