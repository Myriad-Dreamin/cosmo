package cosmo0

class Cosmo1Stage1LexerTests extends munit.FunSuite:
  private val core0TextPath = "library/std/src/std/text.cos"
  private val core0TextOutputPath = "library/std/src/std/text_output.cos"
  private val core0PathFsPath = "library/std/src/std/path_fs.cos"
  private val charClassPath = "library/std/src/std/char_class.cos"
  private val spanPath = "packages/cosmoc/src/source/span.cos"
  private val sourcePath = "packages/cosmoc/src/source/source.cos"
  private val sourceMapPath = "packages/cosmoc/src/source/source_map.cos"
  private val diagnosticPath = "packages/cosmoc/src/driver/diagnostic.cos"
  private val tokenPath = "packages/cosmoc/src/lex/token.cos"
  private val lexerPath = "packages/cosmoc/src/lex/lexer.cos"
  private val lexerTestPath = "packages/cosmoc/src/lex/lexer_test.cos"

  test("cosmo1 Stage 1 lexer fixture source compiles"):
    val source = combineSources(
      List(
        core0TextPath,
        core0TextOutputPath,
        core0PathFsPath,
        charClassPath,
        spanPath,
        sourcePath,
        sourceMapPath,
        diagnosticPath,
        tokenPath,
        lexerPath,
        lexerTestPath,
      ),
    )
    val compiled = Cosmo0().compile(SourceFile(lexerTestPath, source))

    assertEquals(compiled.phase, Phase.Compile)
    assert(
      compiled.isSuccess,
      s"cosmo1 lexer compile failed with diagnostics: ${compiled.diagnostics
          .map(d => d.code -> d.message)}",
    )

    val output = compiled.value.get.output
    assert(output.contains("struct LexerState"))
    assert(output.contains("struct Token"))
    assert(output.contains("lex_source("))
    assert(
      output.contains("inline bool lexer_identifier_number_punctuation_smoke()"),
    )
    assert(output.contains("inline bool lexer_invalid_non_ascii_smoke()"))
    assert(
      output.contains(
        "inline bool lexer_large_integer_literal_preserves_raw_text()",
      ),
    )
    assert(
      output.contains(
        "inline bool lexer_radix_and_separator_literals_preserve_raw_text()",
      ),
    )
    assert(
      output.contains(
        "inline bool lexer_floating_literal_preserves_raw_text()",
      ),
    )
    assert(output.contains("0xFFFF_FFFF"))
    assert(output.contains("1_234.567_890e+123"))

  private def combineSources(paths: List[String]): String =
    paths.map(readCosmoSource).mkString("\n")

  private def readCosmoSource(path: String): String =
    ParserFixtureManifest
      .readFile(path)
      .linesIterator
      .filterNot(_.startsWith("import "))
      .mkString("\n")
end Cosmo1Stage1LexerTests
