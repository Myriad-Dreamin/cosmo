package cosmo0

class Core0TextInterfaceTests extends munit.FunSuite:
  private val core0TextPath = "packages/cosmoc/src/core0/text.cos"
  private val sourceTextPath = "packages/cosmoc/src/source/source.cos"
  private val sourceMapPath = "packages/cosmoc/src/source/source_map.cos"
  private val sourceFixturePath = "fixtures/cosmo0/cosmo1/source-text/source_text_fixture.cos"

  test("direct core0.text source APIs cover length, slicing, char and byte access, and builders"):
    val source = combineSources(
      List(core0TextPath),
      """def core0_text_direct_smoke(): Bool = {
        |  val text = "abcdef";
        |  val view = core0_text_view(text, 1, 4);
        |  var builder = core0_text_builder();
        |  builder.push("view=");
        |  builder.push_view(view);
        |  builder.push_line("");
        |  val rendered = builder.finish();
        |
        |  core0_text_len(text) == 6 and
        |    !core0_text_is_empty(text) and
        |    core0_text_is_empty("") and
        |    core0_text_slice(text, 2, 5) == "cde" and
        |    core0_text_byte_at(text, 1) == 98 and
        |    core0_text_char_at(text, 0) != core0_text_char_at(text, 1) and
        |    view.len() == 3 and
        |    !view.is_empty() and
        |    view.to_string() == "bcd" and
        |    view.byte_at(0) == 98 and
        |    view.char_at(0) != view.char_at(1) and
        |    rendered == "view=bcd\n"
        |}
        |""".stripMargin,
    )

    val lowered = Cosmo0().lower(SourceFile("core0_text_direct.cos", source))

    assertEquals(lowered.phase, Phase.Compile)
    assert(
      lowered.isSuccess,
      s"core0.text direct lowering failed with diagnostics: ${lowered.diagnostics.map(d => d.code -> d.message)}",
    )

    val rendered = LirDebugRenderer.renderModule(lowered.value.get.lir)
    assert(rendered.contains("fn @core0_text_len core0_text_len"))
    assert(rendered.contains("fn @TextBuilder.push push"))
    assert(rendered.contains("fn @TextView.byte_at byte_at"))
    assert(rendered.contains("descriptor String::slice("))
    assert(rendered.contains("descriptor String::char_at("))
    assert(rendered.contains("descriptor String::byte_at("))
    assert(!rendered.contains("descriptor TextBuilder"))
    assert(!rendered.contains("descriptor TextView"))
    assert(!rendered.contains("descriptor StringBuilder"))

    val compiled = CppBackend().emit(lowered.value.get.lir)

    assert(
      compiled.isSuccess,
      s"core0.text direct C++ emission failed with diagnostics: ${compiled.diagnostics.map(d => d.code -> d.message)}",
    )
    val output = compiled.value.get.source
    assert(output.contains("struct TextBuilder"))
    assert(output.contains("struct TextView"))
    assert(output.contains("inline std::size_t core0_text_len(std::string text)"))
    assert(output.contains(".substr("))
    assert(output.contains(".at("))
    assert(!output.contains("StringBuilder"))

  test("cosmo1 source text fixture uses SourceText and SourceMap helpers"):
    val source = combineSources(
      List(core0TextPath, sourceTextPath, sourceMapPath, sourceFixturePath),
      "",
    )

    val result = Cosmo0().compile(SourceFile(sourceFixturePath, source))

    assertEquals(result.phase, Phase.Compile)
    assert(
      result.isSuccess,
      s"cosmo1 source text fixture compile failed with diagnostics: ${result.diagnostics.map(d => d.code -> d.message)}",
    )

    val output = result.value.get.output
    assert(output.contains("struct SourceText"))
    assert(output.contains("struct SourceMap"))
    assert(output.contains("struct SourceLocation"))
    assert(output.contains("inline bool source_text_fixture()"))
    assert(output.contains("inline std::string source_text_slice("))
    assert(output.contains("inline SourceLocation SourceMap_location("))
    assert(!output.contains("StringBuilder"))

  private def combineSources(paths: List[String], extra: String): String =
    (paths.map(ParserFixtureManifest.readFile) :+ extra).mkString("\n")
