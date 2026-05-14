package cosmo

class Cosmo1Stage1NumericLiteralTest extends TestBase:
  private val sourceFiles = List(
    "packages/cosmoc/src/lex/token.cos",
    "packages/cosmoc/src/lex/lexer.cos",
    "packages/cosmoc/src/lex/lexer_test.cos",
    "packages/cosmoc/src/syntax/ast.cos",
  )

  test("cosmoc numeric literal source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse numeric literal source: $path")
    }
  }

  test("lexer source preserves raw numeric token text") {
    val token =
      NodeFs.readFileSync("packages/cosmoc/src/lex/token.cos", "utf8").asInstanceOf[String]
    val lexer =
      NodeFs.readFileSync("packages/cosmoc/src/lex/lexer.cos", "utf8").asInstanceOf[String]
    val lexerTest =
      NodeFs.readFileSync("packages/cosmoc/src/lex/lexer_test.cos", "utf8").asInstanceOf[String]
    val ast =
      NodeFs.readFileSync("packages/cosmoc/src/syntax/ast.cos", "utf8").asInstanceOf[String]

    assert(token.contains("def token_numeric_text(token: Token): String"))
    assert(lexer.contains("scan_hex_digits_and_separators"))
    assert(lexer.contains("scan_exponent_suffix"))
    assert(lexerTest.contains("lexer_large_integer_literal_preserves_raw_text"))
    assert(lexerTest.contains("0xFFFF_FFFF"))
    assert(lexerTest.contains("1_234.567_890e+123"))
    assert(ast.contains("case NumericLiteral(String, Span)"))
  }

  test("numeric literal docs and specs keep big numbers std-owned") {
    val expr = NodeFs.readFileSync("docs/cosmo0/expr.typ", "utf8").asInstanceOf[String]
    val std = NodeFs.readFileSync("docs/cosmo0/std.typ", "utf8").asInstanceOf[String]
    val runtime =
      NodeFs.readFileSync("docs/cosmo0/runtime.typ", "utf8").asInstanceOf[String]
    val delta = NodeFs
      .readFileSync(
        "openspec/changes/add-core0-lossless-numeric-literals/specs/core0-lossless-numeric-literals/spec.md",
        "utf8",
      )
      .asInstanceOf[String]
    val stageDelta = NodeFs
      .readFileSync(
        "openspec/changes/add-core0-lossless-numeric-literals/specs/core0-stage-capability-registry/spec.md",
        "utf8",
      )
      .asInstanceOf[String]

    assert(expr.contains("Numeric literal lexing and parser-facing data preserve the source text"))
    assert(expr.contains("must not reject a numeric token because its value is outside"))
    assert(std.contains("== `core0.big-number`"))
    assert(std.contains("def parse_big_int(text: String): Result[BigInt, NumericParseError]"))
    assert(std.contains("not part of the `cosmo1.stage1` profile"))
    assert(runtime.contains("Numeric literal tokens may carry raw source text through Stage 1"))
    assert(delta.contains("Lossless Numeric Literal Text"))
    assert(delta.contains("descriptor metadata does not register `BigInt`, `BigDecimal`, or `core0.big-number`"))
    assert(stageDelta.contains("core0.big-number is not required by Stage 1"))
    assert(stageDelta.contains("cosmo0.stage.missing-capability"))
  }
end Cosmo1Stage1NumericLiteralTest
