package cosmo0

class ParserFixtureTests extends munit.FunSuite:
  test("shared parser fixture manifest is well formed and points at existing fixtures"):
    val fixtures = ParserFixtureManifest.load()

    assert(fixtures.nonEmpty, "shared parser fixture manifest must list at least one fixture")
    assertEquals(fixtures.map(_.id).distinct.length, fixtures.length)
    assert(
      fixtures.exists(_.expectedStatus == ParserFixtureManifest.ExpectedStatus.Ok),
      "shared parser fixture manifest must include an accepted fixture",
    )
    assert(
      fixtures.exists(_.expectedStatus == ParserFixtureManifest.ExpectedStatus.Error),
      "shared parser fixture manifest must include a rejected fixture",
    )
    fixtures.foreach: fixture =>
      assert(ParserFixtureManifest.exists(fixture.path), s"missing parser fixture ${fixture.path}")

  test("cosmo0 parser validates every shared parser fixture"):
    ParserFixtureManifest.load().foreach: fixture =>
      val source = ParserFixtureManifest.readFile(fixture.path)
      val result = Cosmo0().parse(SourceFile(fixture.path, source))

      fixture.expectedStatus match
        case ParserFixtureManifest.ExpectedStatus.Ok =>
          assertEquals(result.phase, Phase.Parse)
          assert(
            result.isSuccess,
            s"${fixture.id} should parse but got ${result.diagnostics.map(d => d.code -> d.message)}",
          )
        case ParserFixtureManifest.ExpectedStatus.Error =>
          assertEquals(result.phase, Phase.Parse)
          assertEquals(result.status, PhaseStatus.Failed)
          fixture.diagnosticCode.foreach: code =>
            assert(
              result.diagnostics.exists(_.code == code),
              s"${fixture.id} should report $code but got ${result.diagnostics.map(_.code)}",
            )

  test("parser_test and Scala parser tests consume the same manifest"):
    val parserTestSource = ParserFixtureManifest.readFile(ParserFixtureManifest.parserTestSourcePath)
    assert(
      parserTestSource.contains(s""""${ParserFixtureManifest.manifestPath}""""),
      s"parser_test.cos must reference ${ParserFixtureManifest.manifestPath}",
    )

  test("packaged parser sources mirror shared parser program fixtures"):
    assertEquals(
      ParserFixtureManifest.readFile("packages/cosmoc/src/parser.cos"),
      ParserFixtureManifest.readFile(ParserFixtureManifest.parserSourcePath),
    )
    assertEquals(
      ParserFixtureManifest.readFile("packages/cosmoc/src/parser_test.cos"),
      ParserFixtureManifest.readFile(ParserFixtureManifest.parserTestSourcePath),
    )
end ParserFixtureTests
