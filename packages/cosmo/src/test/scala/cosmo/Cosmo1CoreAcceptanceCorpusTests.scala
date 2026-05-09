package cosmo

class Cosmo1CoreAcceptanceCorpusTest extends TestBase:
  private val corpusPath =
    "fixtures/cosmo0/cosmo1/core-acceptance/cosmo1_core_acceptance.cos"
  private val manifestPath =
    "fixtures/cosmo0/cosmo1/core-acceptance/manifest.json"

  test("manifest lists core acceptance corpus") {
    assert(NodeFs.existsSync(manifestPath), s"missing manifest: $manifestPath")
    assert(NodeFs.existsSync(corpusPath), s"missing corpus: $corpusPath")

    val manifest =
      NodeFs.readFileSync(manifestPath, "utf8").asInstanceOf[String]
    assert(
      manifest.contains(corpusPath),
      s"manifest does not list corpus path: $corpusPath",
    )
    assert(
      manifest.contains("\"status\": \"acceptance-target\""),
      "manifest must mark the corpus as a future acceptance target",
    )
  }

  test("core acceptance corpus parses through shared parser") {
    val source = NodeFs.readFileSync(corpusPath, "utf8").asInstanceOf[String]
    val parsed = compiler.parse(source)

    assert(parsed.isDefined, s"failed to parse corpus: $corpusPath")
    assert(parsed.get.stmts.nonEmpty, s"corpus has no top-level forms: $corpusPath")
  }
end Cosmo1CoreAcceptanceCorpusTest
