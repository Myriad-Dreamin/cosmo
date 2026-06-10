package cosmo0

class CheckOrderSnapshotTests extends munit.FunSuite:
  private val snapshotRoot = "fixtures/name-resolution/check-order"

  test("check-order pretty printer matches snapshots"):
    val fixtures =
      TestFixtureScanner.filesUnder(snapshotRoot, _.endsWith(".cos"))
    assert(fixtures.nonEmpty, s"$snapshotRoot must contain .cos fixtures")

    fixtures.foreach { sourcePath =>
      val snapshotPath = sourcePath.stripSuffix(".cos") + ".snapshot"
      assert(
        ParserFixtureManifest.exists(snapshotPath),
        s"$sourcePath must have matching snapshot $snapshotPath",
      )

      val source = ParserFixtureManifest.readFile(sourcePath)
      val result = Cosmo0().elaborate(SourceFile(sourcePath, source))
      assert(
        result.isSuccess,
        s"$sourcePath failed with diagnostics: ${renderDiagnostics(result.diagnostics)}",
      )

      val actual = normalizeSnapshot(
        UntypedCheckOrder.pretty(result.value.get),
      )
      val expected =
        normalizeSnapshot(ParserFixtureManifest.readFile(snapshotPath))
      assertEquals(actual, expected, sourcePath)
    }

  private def normalizeSnapshot(text: String): String =
    text.replace("\r\n", "\n").stripSuffix("\n")

  private def renderDiagnostics(diagnostics: List[Diagnostic]): String =
    diagnostics
      .map: diagnostic =>
        val location = diagnostic.span match
          case Some(span) =>
            s"${span.fileName}:${span.start.line}:${span.start.column}"
          case None => "<no span>"
        s"$location ${diagnostic.code}: ${diagnostic.message}"
      .mkString("; ")
