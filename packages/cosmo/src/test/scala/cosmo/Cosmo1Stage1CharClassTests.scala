package cosmo

class Cosmo1Stage1CharClassTest extends TestBase:
  private val sourceFiles = List(
    "packages/cosmoc/src/core0/char_class.cos",
    "packages/cosmoc/src/core0/char_class_test.cos",
    "packages/cosmoc/src/lex/lexer.cos",
    "packages/cosmoc/src/lex/lexer_test.cos",
  )

  test("cosmoc Stage 1 char class and lexer source files parse") {
    for (path <- sourceFiles) {
      val source = NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
      val parsed = compiler.parse(source)
      assert(parsed.isDefined, s"failed to parse Stage 1 char class source: $path")
    }
  }

  test("cosmoc manifest includes char class and lexer test slice") {
    val manifest =
      NodeFs.readFileSync("packages/cosmoc/cosmo.json", "utf8").asInstanceOf[String]

    assert(manifest.contains("\"core0/char_class.cos\""))
    assert(manifest.contains("\"core0/char_class_test.cos\""))
    assert(manifest.contains("\"lex/lexer.cos\""))
    assert(manifest.contains("\"lex/lexer_test.cos\""))
  }

  test("char class docs and specs define ascii-only std capability") {
    val std = NodeFs.readFileSync("docs/cosmo0/std.typ", "utf8").asInstanceOf[String]
    val expr = NodeFs.readFileSync("docs/cosmo0/expr.typ", "utf8").asInstanceOf[String]
    val delta = NodeFs
      .readFileSync(
        "openspec/changes/add-core0-char-class/specs/core0-char-class/spec.md",
        "utf8",
      )
      .asInstanceOf[String]
    val stageDelta = NodeFs
      .readFileSync(
        "openspec/changes/add-core0-char-class/specs/core0-stage-capability-registry/spec.md",
        "utf8",
      )
      .asInstanceOf[String]

    assert(std.contains("`core0.char-class`"))
    assert(std.contains("def is_identifier_start(byte: Byte): Bool"))
    assert(std.contains("bytes above `127`"))
    assert(expr.contains("compares `Byte` values against ASCII code ranges"))
    assert(delta.contains("Unsupported non-ASCII bytes are not classified"))
    assert(delta.contains("are not registered descriptor families"))
    assert(stageDelta.contains("cosmo0.stage.missing-capability"))
    assert(stageDelta.contains("`core0.char-class`"))
  }
end Cosmo1Stage1CharClassTest
