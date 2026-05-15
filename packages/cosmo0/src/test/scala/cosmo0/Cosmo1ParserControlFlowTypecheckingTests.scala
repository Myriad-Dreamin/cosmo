package cosmo0

class Cosmo1ParserControlFlowTypecheckingTests extends munit.FunSuite:
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val astPath = "packages/cosmoc/src/syntax/ast.cos"
  private val parserPath = "packages/cosmoc/src/parser.cos"

  test("cosmo1 parser source type-checks"):
    val source = combineSources(List(spanPath, astPath, parserPath))
    val checked = Cosmo0().check(SourceFile(parserPath, source))

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"parser.cos check failed with diagnostics: ${checked.diagnostics.map(d => d.code -> d.message)}",
    )
    val typed = checked.value.get.typed
    assert(typed.declarations.exists(_.name == "SyntaxParserState"))
    assert(typed.declarations.exists(_.name == "parse_source_ast"))

  test("control-flow conditions must be Bool"):
    assertCheckFails(
      """
      |def bad_if(): Unit = {
      |  if (1) {
      |  }
      |}
      |""".stripMargin,
      "cosmo0.type.expected-bool",
    )
    assertCheckFails(
      """
      |def bad_while(): Unit = {
      |  while (1) {
      |  }
      |}
      |""".stripMargin,
      "cosmo0.type.expected-bool",
    )

  test("returns must match declared function type"):
    assertCheckFails(
      """
      |def bad_return(): Bool = {
      |  return 1
      |}
      |""".stripMargin,
      "cosmo0.type.return-mismatch",
    )

  test("non-unit functions must return on block control-flow paths"):
    assertCheckFails(
      """
      |def missing_return(flag: Bool): Bool = {
      |  if (flag) {
      |    return true
      |  }
      |}
      |""".stripMargin,
      "cosmo0.type.missing-return",
    )

  test("early returns in both branches satisfy function return compatibility"):
    val checked = Cosmo0().check(
      """
      |def ok(flag: Bool): Bool = {
      |  if (flag) {
      |    return true
      |  } else {
      |    return false
      |  }
      |}
      |""".stripMargin,
    )

    assertEquals(checked.phase, Phase.Check)
    assert(
      checked.isSuccess,
      s"early return control-flow check failed with diagnostics: ${checked.diagnostics.map(d => d.code -> d.message)}",
    )

  private def assertCheckFails(source: String, code: String): Unit =
    val checked = Cosmo0().check(source)
    assertEquals(checked.phase, Phase.Check)
    assertEquals(checked.status, PhaseStatus.Failed)
    assert(
      checked.diagnostics.exists(_.code == code),
      s"expected $code but got ${checked.diagnostics.map(d => d.code -> d.message)}",
    )

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1ParserControlFlowTypecheckingTests
