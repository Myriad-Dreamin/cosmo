package cosmo0

class Cosmo1CoreAcceptanceCorpusTests extends munit.FunSuite:
  private val corpusPath =
    "fixtures/cosmo0/cosmo1/core-acceptance/cosmo1_core_acceptance.cos"
  private val manifestPath =
    "fixtures/cosmo0/cosmo1/core-acceptance/manifest.json"

  test("manifest lists core acceptance corpus"):
    assert(
      ParserFixtureManifest.exists(manifestPath),
      s"missing manifest: $manifestPath",
    )
    assert(
      ParserFixtureManifest.exists(corpusPath),
      s"missing corpus: $corpusPath",
    )

    val manifest = ParserFixtureManifest.readFile(manifestPath)
    assert(
      manifest.contains(corpusPath),
      s"manifest does not list corpus path: $corpusPath",
    )
    assert(
      manifest.contains("\"status\": \"acceptance-target\""),
      "manifest must mark the corpus as a future acceptance target",
    )

  test("core acceptance corpus parses through shared parser"):
    val source = ParserFixtureManifest.readFile(corpusPath)
    val parsed = Cosmo0().parse(SourceFile(corpusPath, source))

    assertEquals(parsed.phase, Phase.Parse)
    assert(
      parsed.isSuccess,
      s"failed to parse corpus: ${parsed.diagnostics.map(d => d.code -> d.message)}",
    )
    assert(
      parsed.value.exists(_.ast.stmts.nonEmpty),
      s"corpus has no top-level forms: $corpusPath",
    )
end Cosmo1CoreAcceptanceCorpusTests
